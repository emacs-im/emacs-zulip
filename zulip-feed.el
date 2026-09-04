;;; zulip-feed.el --- Appkit-backed Zulip message feeds -*- lexical-binding: t; -*-

;;; Commentary:

;; This is the smallest useful vertical feed slice: a protocol narrow selects
;; an exact Appkit history window, canonical state projects into the shared
;; timeline, and a trailing composer performs optimistic sends.  Message IDs
;; stay opaque decimal strings.  Local optimistic keys are explicitly rekeyed
;; to server IDs in the same timeline reconciliation that projects the server
;; row.

;;; Code:

(require 'cl-lib)
(require 'browse-url)
(require 'button)
(require 'seq)
(require 'subr-x)
(require 'url-util)
(require 'appkit-core)
(require 'appkit-compose)
(require 'appkit-invalidation)
(require 'appkit-chat-avatar)
(require 'appkit-chatbuf)
(require 'appkit-chat-history)
(require 'appkit-chat-ins)
(require 'appkit-chat-timeline)
(require 'appkit-scroll)
(require 'appkit-name-color)
(require 'appkit-ui)
(require 'appkit-markup)
(require 'appkit-markup-ui)
(require 'appkit-markup-compose)
(require 'appkit-presentation)
(require 'zulip-customize)
(require 'zulip-completion)
(require 'zulip-runtime)
(require 'zulip-media)
(require 'zulip-narrow)
(require 'zulip-markup)
(require 'zulip-state)

(declare-function zulip-state-object-get "zulip-state" (object key))
(declare-function zulip-state-messages-for-narrow "zulip-state" (state narrow-key))
(declare-function zulip-state-upsert-message "zulip-state" (state message &optional narrow-keys))
(declare-function zulip-state-merge-messages "zulip-state" (state messages narrow-key))
(declare-function zulip-api-get-messages "zulip-api" (account narrow anchor before after callback &rest options))
(declare-function zulip-api-send-message "zulip-api" (account type to topic content callback &rest options))
(declare-function zulip-api-update-message-flags "zulip-api" (account message-ids operation flag callback &rest options))
(declare-function zulip-api-get-message "zulip-api" (account message-id callback &rest options))
(declare-function zulip-api-update-message "zulip-api" (account message-id callback &rest options))
(declare-function zulip-api-delete-message "zulip-api" (account message-id callback &rest options))
(declare-function zulip-api-add-reaction "zulip-api" (account message-id emoji-name callback &rest options))
(declare-function zulip-api-remove-reaction "zulip-api" (account message-id callback &rest options))
(declare-function zulip-api-result-ok-p "zulip-api" (result))
(declare-function zulip-api-result-data "zulip-api" (result))
(declare-function zulip-api-result-message "zulip-api" (result))
(declare-function zulip-http-cancel-request "zulip-http" (request))
(declare-function zulip-message-transient "zulip-transient" (&rest arguments))

(autoload 'zulip-message-transient "zulip-transient" nil t)


(defgroup zulip-feed nil
  "Zulip feed buffers."
  :group 'zulip)

(defconst zulip-feed--anchor-property 'zulip-message-id
  "Text property carrying a feed row's stable message ID.")

(defconst zulip-feed--message-object-property 'zulip-message-object
  "Text property identifying generated Zulip message content.")

(defvar zulip-feed--local-sequence 0
  "Monotonic component of optimistic message IDs.")

(defvar-local zulip-feed--account nil
  "Account owning the current feed buffer.")

(defvar-local zulip-feed--narrow nil
  "Canonical narrow displayed by the current feed buffer.")

(defvar-local zulip-feed--pending nil
  "Equal-tested local ID to optimistic message table.")

(defvar-local zulip-feed--last-error nil
  "Last asynchronous feed error, for passive presentation and tests.")

(defvar-local zulip-feed--latest-live-keys nil
  "Message keys observed while the current latest request is in flight.")

(defvar-local zulip-feed--history-reload-needed-p nil
  "Non-nil when event reconciliation invalidated the exact history window.")

(defvar-local zulip-feed--fill-column nil
  "Stable presentation width measured from a window displaying this feed.")

(defvar-local zulip-feed--pending-jump-id nil
  "Message ID to visit after an exact-anchor history request completes.")

(defvar-local zulip-feed--pending-read-ids nil
  "Equal-tested set of server IDs awaiting a read event or failed response.")

(defvar-local zulip-feed--auto-read-suppressed-ids nil
  "Server IDs explicitly marked unread and excluded from automatic reads.")

(defvar-local zulip-feed--last-read-target-id nil
  "Newest timeline key submitted as an automatic read frontier.")
(defvar-local zulip-feed--scroll-observer nil)

(defvar-local zulip-feed-timeline-mode nil
  "Non-nil when point-local timeline commands are active.")

(defvar-local zulip-feed--edit-generation nil
  "Opaque generation owning the current edit session and deferred actions.")

(defvar-local zulip-feed--edit-operation-owner nil
  "Exact owner of the current edit GET or PATCH operation.")

(defvar-local zulip-feed--edit-sync-request nil
  "Pending generation-owned edit composer materialization request.")

(defun zulip-feed--edit-state ()
  "Return the active Appkit edit aux state, or nil."
  (let ((state (appkit-chatbuf-aux-state)))
    (and (eq (plist-get state :aux-type) 'edit) state)))

(defun zulip-feed--edit-message-id ()
  "Return the opaque message ID staged in Appkit's edit aux state."
  (plist-get (zulip-feed--edit-state) :message-id))

(defun zulip-feed--edit-request-p ()
  "Return non-nil while the Appkit edit aux state owns an HTTP request."
  (and (plist-get (zulip-feed--edit-state) :request-p) t))

(defun zulip-feed--edit-composer-busy-p ()
  "Return non-nil while an edit operation or materialization owns input."
  (or (zulip-feed--edit-request-p)
      zulip-feed--edit-sync-request))

(defun zulip-feed--assert-edit-composer-mutable ()
  "Reject a programmatic composer mutation while an edit owner is active."
  (when (zulip-feed--edit-composer-busy-p)
    (user-error "The Zulip edit composer is busy")))

(cl-defun zulip-feed--set-edit-state
    (message-id request-p &optional message saved-input
                &key
                (saved-codec nil saved-codec-supplied-p)
                (generation nil generation-supplied-p)
                (operation-owner nil operation-owner-supplied-p))
  "Set Appkit edit aux state for MESSAGE-ID and REQUEST-P.

MESSAGE supplies the context row when starting an edit; later state changes
preserve the existing context object.  SAVED-INPUT and SAVED-CODEC restore the
preceding rich draft interpretation when editing finishes or is cancelled.
GENERATION owns the logical edit session, while OPERATION-OWNER identifies its
exact GET or PATCH."
  (let* ((existing (zulip-feed--edit-state))
         (state (copy-sequence
                 (or existing
                     (list :aux-type 'edit :saved-input saved-input
                           :saved-codec saved-codec
                           :generation zulip-feed--edit-generation)))))
    (setq state (plist-put state :aux-type 'edit)
          state (plist-put state :message-id message-id)
          state (plist-put state :request-p (and request-p t)))
    (when message
      (setq state (plist-put state :aux-msg message)))
    (when saved-codec-supplied-p
      (setq state (plist-put state :saved-codec saved-codec)))
    (when generation-supplied-p
      (setq state (plist-put state :generation generation)))
    (when operation-owner-supplied-p
      (setq state (plist-put state :operation-owner operation-owner)))
    (prog1 (appkit-chatbuf-aux-set state)
      ;; Emacs disables change hooks after one of them aborts a modification.
      ;; Keep an in-flight edit owner read-only through the built-in guard, so
      ;; ordinary user mutations never need to trip that destructive fallback.
      (when request-p
        (setq-local buffer-read-only t)))))

(defun zulip-feed--finish-edit-and-restore-draft ()
  "Clear edit model state and restore the canonical preceding draft.

The next Appkit synchronization transaction materializes that canonical
composer state.  This function never edits generated buffer content or point."
  (let* ((state (zulip-feed--edit-state))
         (saved-input (plist-get state :saved-input))
         (saved-codec (plist-get state :saved-codec)))
    (appkit-chatbuf-aux-reset)
    (when (and saved-codec
               (not (eq saved-codec
                        appkit-markup-compose-active-codec)))
      (appkit-markup-compose-set-active-codec saved-codec))
    (appkit-chatbuf-input-state-set
     (or saved-input "") :reset-history-p t)))

(defun zulip-feed--advance-edit-generation ()
  "Invalidate prior edit operations and return a fresh generation."
  (setq zulip-feed--edit-generation (list 'zulip-feed-edit-generation)
        zulip-feed--edit-operation-owner nil
        zulip-feed--edit-sync-request nil)
  zulip-feed--edit-generation)

(defun zulip-feed--new-edit-operation-owner
    (view generation message-id kind)
  "Install and return an edit operation owner for VIEW and GENERATION.

MESSAGE-ID is the opaque server identity and KIND is either `get' or `patch'."
  (setq zulip-feed--edit-operation-owner
        (list :view view
              :generation generation
              :message-id message-id
              :kind kind
              :token (list 'zulip-feed-edit-operation))))

(defun zulip-feed--captured-view-current-p (view)
  "Return non-nil when VIEW is this buffer's exact live feed controller."
  (and (appkit-view-live-p view)
       (eq (appkit-view-buffer view) (current-buffer))
       (eq (appkit-current-view) view)
       (derived-mode-p 'zulip-feed-mode)))

(defun zulip-feed--edit-generation-current-p (view generation)
  "Return non-nil when GENERATION still owns exact captured VIEW."
  (and (zulip-feed--captured-view-current-p view)
       (eq generation zulip-feed--edit-generation)))

(defun zulip-feed--edit-operation-current-p (view generation owner)
  "Return non-nil when OWNER still owns GENERATION in exact VIEW."
  (let ((state (zulip-feed--edit-state)))
    (and (zulip-feed--edit-generation-current-p view generation)
         (eq owner zulip-feed--edit-operation-owner)
         (eq generation (plist-get state :generation))
         (eq owner (plist-get state :operation-owner)))))

(defun zulip-feed--request-edit-sync (view generation &optional action)
  "Materialize edit model state in VIEW for GENERATION, then run ACTION.

ACTION runs inside the exact view's synchronization callback after Appkit has
projected the frame and canonical composer.  A replacement generation or view
makes both materialization and ACTION inert."
  (when (zulip-feed--edit-generation-current-p view generation)
    (setq zulip-feed--edit-sync-request
          (list :view view :generation generation :action action))
    (appkit-request-sync
     view :parts '(frame composer) :position t)))

(defun zulip-feed--update-edit-read-only-state ()
  "Reflect the current edit owner/barrier in `buffer-read-only'."
  (setq-local buffer-read-only
              (and (zulip-feed--edit-composer-busy-p)
                   t)))

(defun zulip-feed--run-edit-sync-request (view)
  "Materialize and finish VIEW's current edit synchronization request."
  (when-let* ((request zulip-feed--edit-sync-request))
    (if (not (and (eq view (plist-get request :view))
                  (zulip-feed--edit-generation-current-p
                   view (plist-get request :generation))))
        (when (eq request zulip-feed--edit-sync-request)
          (setq zulip-feed--edit-sync-request nil))
      ;; Canonical input is callback-owned model state.  Only this generated
      ;; transaction is allowed to replace the live composer from it.
      (appkit-chat-timeline-run-preserving-position
       (lambda ()
         (appkit-chatbuf-input-set-text (appkit-chatbuf-input-state))))
      (when-let* ((action (plist-get request :action)))
        (funcall action))
      (when (eq request zulip-feed--edit-sync-request)
        (setq zulip-feed--edit-sync-request nil))))
  (zulip-feed--update-edit-read-only-state))

(defun zulip-feed--account-table (account name)
  "Return ACCOUNT's equal-tested feed registry named NAME."
  (let* ((app (zulip-account-app account))
         (store (and (appkit-app-p app) (appkit-app-resource-store app)))
         (key (list 'zulip-feed name)))
    (unless store
      (error "Zulip account has no Appkit resource store"))
    (or (gethash key store)
        (let ((table (make-hash-table :test #'equal)))
          (puthash key table store)
          table))))

(defun zulip-feed--bind-account-tables (account)
  "Bind current feed buffer registries to tables owned by ACCOUNT."
  (setq-local zulip-feed--pending
              (zulip-feed--account-table account 'pending)))

(defun zulip-feed--rebase-pending (account state)
  "Return STATE with ACCOUNT's in-flight optimistic messages restored."
  (let ((next state))
    (maphash
     (lambda (_local-id message)
       (let ((narrow-key (zulip-feed--field message 'narrow-key)))
         (setq next
               (zulip-state-upsert-message
                next message (list narrow-key)))))
     (zulip-feed--account-table account 'pending))
    next))

(defun zulip-feed--field (object key)
  "Return field KEY from hash-table or alist OBJECT.

Both Lisp hyphen names and API underscore names are accepted."
  (if (fboundp 'zulip-state-object-get)
      (zulip-state-object-get object key)
    (let* ((name (if (symbolp key) (symbol-name key) key))
           (underscore (replace-regexp-in-string "-" "_" name))
           (hyphen (replace-regexp-in-string "_" "-" name))
           (names (delete-dups (list name underscore hyphen)))
           (missing (make-symbol "missing"))
           (value missing))
      (cond
       ((hash-table-p object)
        (while (and names (eq value missing))
          (let* ((candidate (pop names))
                 (symbol (intern candidate)))
            (setq value (gethash candidate object missing))
            (when (eq value missing)
              (setq value (gethash symbol object missing))))))
       ((listp object)
        (while (and names (eq value missing))
          (let* ((candidate (pop names))
                 (symbol (intern candidate))
                 (cell (or (assoc symbol object)
                           (assoc candidate object))))
            (when cell (setq value (cdr cell)))))))
      (unless (eq value missing) value))))

(defun zulip-feed--message-key (message)
  "Return MESSAGE's stable opaque ID as a string."
  (let ((id (zulip-feed--field message 'id)))
    (cond
     ((and (stringp id) (not (string-empty-p id))) id)
     ((integerp id) (number-to-string id))
     (t (error "Zulip message has no stable ID: %S" message)))))

(defun zulip-feed--true-p (value)
  "Return non-nil when JSON-like VALUE denotes true."
  (not (memq value '(nil :false :json-false json-false))))

(defun zulip-feed--string-equal-ignore-case (left right)
  "Return non-nil when strings LEFT and RIGHT are equal ignoring case."
  (and (stringp left)
       (stringp right)
       (equal (downcase left) (downcase right))))

(defun zulip-feed--sequence (value)
  "Return VALUE as a list when it is a list or vector."
  (cond ((vectorp value) (append value nil))
        ((listp value) value)
        ((null value) nil)
        (t (list value))))

(defun zulip-feed--message-kind (message)
  "Return canonical `channel' or `direct' kind for MESSAGE."
  (let ((kind (zulip-feed--field message 'kind))
        (type (zulip-feed--field message 'type)))
    (cond
     ((memq kind '(channel direct)) kind)
     ((member (downcase (format "%s" type)) '("stream" "channel"))
      'channel)
     ((member (downcase (format "%s" type)) '("private" "direct" "dm"))
      'direct))))

(defun zulip-feed--message-channel-id (message)
  "Return MESSAGE's canonical channel ID, or nil."
  (when-let* ((id (or (zulip-feed--field message 'channel-id)
                      (zulip-feed--field message 'stream-id))))
    (format "%s" id)))

(defun zulip-feed--message-topic (message)
  "Return MESSAGE's topic name, or nil for a direct message."
  (when (eq (zulip-feed--message-kind message) 'channel)
    (format "%s" (or (zulip-feed--field message 'topic)
                     (zulip-feed--field message 'subject)
                     ""))))

(defun zulip-feed--message-self-p (message)
  "Return non-nil when MESSAGE was sent by the current account."
  (let ((self-id (and (zulip-state-p (zulip-feed--account-state))
                      (zulip-state-self-user-id
                       (zulip-feed--account-state))))
        (sender-id (zulip-feed--field message 'sender-id)))
    (or (and self-id sender-id
             (equal (format "%s" self-id) (format "%s" sender-id)))
        (and (zulip-account-p zulip-feed--account)
             (zulip-feed--string-equal-ignore-case
              (or (zulip-account-email zulip-feed--account) "")
              (format "%s"
                      (or (zulip-feed--field message 'sender-email) "")))))))

(defun zulip-feed--channel-name (message)
  "Return a human-readable channel name for MESSAGE."
  (or (and (stringp (zulip-feed--field message 'display-recipient))
           (zulip-feed--field message 'display-recipient))
      (when-let* ((id (zulip-feed--message-channel-id message))
                  (channel (zulip-state-channel
                            (zulip-feed--account-state) id)))
        (format "%s" (or (zulip-feed--field channel 'name) id)))
      (zulip-feed--message-channel-id message)
      "channel"))

(defun zulip-feed--user-name (user-id)
  "Return the current account's display name for USER-ID, or nil."
  (when-let* ((state (zulip-feed--account-state))
              (user (zulip-state-user state user-id)))
    (let ((name (or (zulip-feed--field user 'full-name)
                    (zulip-feed--field user 'name)
                    (zulip-feed--field user 'email))))
      (and name (format "%s" name)))))

(defun zulip-feed--message-direct-title (message)
  "Return a human-readable direct-conversation title for MESSAGE."
  (let* ((state (zulip-feed--account-state))
         (self-id (and (zulip-state-p state)
                       (zulip-state-self-user-id state)))
         (recipient-ids
          (and (zulip-state-p state)
               (zulip-state--direct-narrow-ids state message)))
         (display-users
          (zulip-feed--sequence
           (zulip-feed--field message 'display-recipient)))
         names)
    (if (and self-id (equal recipient-ids (list self-id)))
        "Saved messages"
      (dolist (recipient-id recipient-ids)
        (let* ((id (format "%s" recipient-id))
               (display-user
                (seq-find
                 (lambda (user)
                   (equal id
                          (format "%s"
                                  (or (zulip-feed--field user 'id)
                                      (zulip-feed--field user 'user-id)))))
                 display-users))
               (name (or (zulip-feed--field display-user 'full-name)
                         (zulip-feed--field display-user 'name)
                         (zulip-feed--user-name recipient-id)
                         (format "User %s" id))))
          (push (format "%s" name) names)))
      (if names
          (mapconcat #'identity (nreverse names) ", ")
        "Direct message"))))

(defun zulip-feed--message-context-label (message)
  "Return narrow breadcrumb text useful for MESSAGE in the current feed."
  (pcase (zulip-narrow-kind zulip-feed--narrow)
    ('all
     (if (eq (zulip-feed--message-kind message) 'channel)
         (format "#%s  ›  %s"
                 (zulip-feed--channel-name message)
                 (zulip-feed--message-topic message))
       (zulip-feed--message-direct-title message)))
    ('channel (format "%s" (zulip-feed--message-topic message)))
    (_ nil)))

(defun zulip-feed--message-timestamp (message)
  "Return MESSAGE's timestamp as a floating-point epoch value."
  (let ((value (zulip-feed--field message 'timestamp)))
    (cond ((numberp value) (float value))
          ((and (stringp value)
                (string-match-p "\\`[0-9]+\\(?:\\.[0-9]+\\)?\\'" value))
           (string-to-number value))
          (t 0.0))))

(defun zulip-feed--message-day-key (message)
  "Return local calendar-day key for MESSAGE."
  (format-time-string "%Y-%m-%d"
                      (seconds-to-time
                       (zulip-feed--message-timestamp message))))

(defun zulip-feed--message-day-label (message)
  "Return a readable date-divider label for MESSAGE."
  (format-time-string "%Y-%m-%d  %A"
                      (seconds-to-time
                       (zulip-feed--message-timestamp message))))

(defun zulip-feed--message-time-label (message &optional short)
  "Return MESSAGE time label, compact when SHORT is non-nil."
  (format-time-string (if short "%H:%M" "%Y-%m-%d %H:%M")
                      (seconds-to-time
                       (zulip-feed--message-timestamp message))))

(defun zulip-feed--sender-fallback (message)
  "Return a compact textual avatar fallback for MESSAGE."
  (let* ((name (zulip-feed--message-sender message))
         (words (split-string name "[[:space:]]+" t))
         (first (car words))
         (last (car (last words))))
    (upcase
     (concat (if (string-empty-p (or first "")) "@" (substring first 0 1))
             (if (or (null last) (equal first last)) "" (substring last 0 1))))))

(defun zulip-feed--update-fill-column ()
  "Refresh and return the current feed's responsive presentation width."
  (setq-local zulip-feed--fill-column
              (max 40
                   (or (appkit-view-responsive-width 2)
                       (and (integerp zulip-feed--fill-column)
                            zulip-feed--fill-column)
                       80))))

(defun zulip-feed--messages-compact-p (previous message)
  "Return non-nil when MESSAGE may visually continue PREVIOUS."
  (and previous
       (equal (format "%s" (or (zulip-feed--field previous 'sender-id)
                               (zulip-feed--field previous 'sender-email)
                               (zulip-feed--message-sender previous)))
              (format "%s" (or (zulip-feed--field message 'sender-id)
                               (zulip-feed--field message 'sender-email)
                               (zulip-feed--message-sender message))))
       (eq (zulip-feed--message-kind previous)
           (zulip-feed--message-kind message))
       (equal (zulip-feed--message-channel-id previous)
              (zulip-feed--message-channel-id message))
       (equal (zulip-feed--message-topic previous)
              (zulip-feed--message-topic message))
       (let ((gap (- (zulip-feed--message-timestamp message)
                     (zulip-feed--message-timestamp previous))))
         (and (>= gap 0)
              (<= gap (max 0 zulip-message-compact-seconds))))))

(defun zulip-feed--first-unread-key (messages)
  "Return the first unread server key present in ordered MESSAGES."
  (when (and zulip-show-unread-divider
             (zulip-state-p (zulip-feed--account-state)))
    (seq-some
     (lambda (message)
       (let ((key (zulip-feed--message-key message)))
         (and (zulip-state-unread-message-p
               (zulip-feed--account-state) key)
              key)))
     messages)))

(defun zulip-feed--message-context (previous message first-unread-key)
  "Return projected render context for MESSAGE after PREVIOUS."
  (let ((day (zulip-feed--message-day-key message))
        (previous-day (and previous
                           (zulip-feed--message-day-key previous)))
        (key (zulip-feed--message-key message)))
    (list :compact (and (zulip-feed--messages-compact-p previous message) t)
          :insert-date (and zulip-show-date-separators
                            (not (equal day previous-day))
                            (zulip-feed--message-day-label message))
          :insert-unread (and first-unread-key
                              (equal key first-unread-key)
                              t)
          :breadcrumb (zulip-feed--message-context-label message))))

(defun zulip-feed--message-dependencies (message)
  "Return resource keys whose presentation can affect MESSAGE."
  (delete-dups
   (delq nil
         (list
          (when-let* ((sender-id (zulip-feed--field message 'sender-id)))
            (list :user (format "%s" sender-id)))
          (when-let* ((channel-id (zulip-feed--message-channel-id message)))
            (list :channel channel-id))))))

(defun zulip-feed--message-properties (message key)
  "Return generated text properties for MESSAGE identified by KEY."
  (append
   (list zulip-feed--anchor-property key
         zulip-feed--message-object-property t
         'read-only t
         'front-sticky '(read-only)
         'rear-nonsticky
         '(read-only zulip-message-id zulip-message-object))
   (when (zulip-feed--message-flag-p message "starred")
     (list 'zulip-message-starred t))))

(defun zulip-feed--account-state ()
  "Return canonical state for the current feed account."
  (and (zulip-account-p zulip-feed--account)
       (zulip-account-state zulip-feed--account)))

(defun zulip-feed--set-account-state (account state)
  "Install immutable STATE as ACCOUNT's current canonical state."
  (zulip-runtime-publish-state account state))

(defun zulip-feed--state-entries ()
  "Return ordered state entries for the current narrow."
  (unless (fboundp 'zulip-state-messages-for-narrow)
    (error "Zulip state message projection is unavailable"))
  (copy-sequence
   (or (zulip-state-messages-for-narrow
        (zulip-feed--account-state)
        (zulip-narrow-key zulip-feed--narrow))
       '())))

(defun zulip-feed--timeline-entries ()
  "Return the current exact history slice for timeline projection."
  (if (not (appkit-chat-history-window-known-p))
      nil
    (let ((slice (appkit-chat-history-window-slice
                  (zulip-feed--state-entries)
                  #'zulip-feed--message-key)))
      (and (plist-get slice :valid-p)
           (plist-get slice :entries)))))

(defun zulip-feed--message-sender (message)
  "Return a plain sender label for MESSAGE."
  (format "%s"
          (or (zulip-feed--field message 'sender-full-name)
              (zulip-feed--field message 'sender-email)
              (zulip-feed--field message 'sender-id)
              "unknown")))

(defun zulip-feed--message-sender-color-key (message)
  "Return MESSAGE's stable sender key for shared name coloring."
  (format "%s"
          (or (zulip-feed--field message 'sender-id)
              (zulip-feed--field message 'sender-email)
              (zulip-feed--message-sender message))))

(defun zulip-feed--message-sender-face (message)
  "Return the sender heading face for MESSAGE."
  (if (zulip-feed--message-self-p message)
      'zulip-message-self-face
    (if-let* ((color-face
               (appkit-name-color-face
                (zulip-feed--message-sender-color-key message))))
        (list color-face 'zulip-message-sender-face)
      'zulip-message-sender-face)))

(defun zulip-feed--markup-fallback-text (node)
  "Return semantic fallback text stored by provider object NODE."
  (cond
   ((appkit-markup-object-p node)
    (appkit-markup-plain-text
     (appkit-markup-document
      (list
       (appkit-markup-paragraph
        (appkit-markup-object-fallback node))))))
   ((appkit-markup-object-block-p node)
    (appkit-markup-plain-text
     (appkit-markup-document
      (appkit-markup-object-block-fallback node))))
   (t "")))

(defun zulip-feed--markup-value (node)
  "Return NODE's validated Zulip provider value."
  (let ((value
         (cond
          ((appkit-markup-object-p node)
           (appkit-markup-object-value node))
          ((appkit-markup-object-block-p node)
           (appkit-markup-object-block-value node)))))
    (and (zulip-markup-provider-object-p value) value)))

(defun zulip-feed--decode-narrow-segment (segment)
  "Decode one Zulip hash-narrow URL SEGMENT."
  (url-unhex-string
   (replace-regexp-in-string
    "\\.\\([[:xdigit:]][[:xdigit:]]\\)" "%\\1" (or segment ""))))

(defun zulip-feed--markup-navigation (data)
  "Return native navigation operands decoded from provider DATA."
  (when-let* ((url (plist-get data :url))
              ((string-match "#narrow/\\([^?#]*\\)" url)))
    (let* ((segments (split-string (match-string 1 url) "/" t))
           (channel-segment (cadr (member "channel" segments)))
           (topic-segment (cadr (member "topic" segments)))
           (near-segment
            (or (cadr (member "near" segments))
                (cadr (member "with" segments))))
           (configured-id (plist-get data :channel-id))
           (channel-id
            (cond
             ((and (stringp configured-id)
                   (string-match-p "\\`[1-9][0-9]*\\'" configured-id))
              (string-to-number configured-id))
             ((and channel-segment
                   (string-match "\\`\\([1-9][0-9]*\\)" channel-segment))
              (string-to-number (match-string 1 channel-segment))))))
      (list :channel-id channel-id
            :topic (and topic-segment
                        (zulip-feed--decode-narrow-segment topic-segment))
            :message-id (and near-segment
                             (string-match-p "\\`[0-9]+\\'" near-segment)
                             near-segment)
            :url url))))

(defun zulip-feed--markup-navigation-action (kind data)
  "Return native action for provider navigation KIND and DATA."
  (let* ((navigation (zulip-feed--markup-navigation data))
         (channel-id (plist-get navigation :channel-id))
         (topic (plist-get navigation :topic))
         (message-id (plist-get navigation :message-id))
         (url (or (plist-get navigation :url) (plist-get data :url)))
         (account zulip-feed--account))
    (cond
     ((and (eq kind 'channel-link) channel-id)
      (lambda ()
        (zulip-feed-open account (zulip-narrow-channel channel-id))))
     ((and (memq kind '(topic-link message-link)) channel-id topic message-id
           (eq kind 'message-link))
      (lambda ()
        (zulip-feed-open-message
         account (zulip-narrow-topic channel-id topic) message-id)))
     ((and (memq kind '(topic-link message-link)) channel-id topic)
      (lambda ()
        (zulip-feed-open account (zulip-narrow-topic channel-id topic))))
     ((stringp url) (lambda () (browse-url url))))))

(defun zulip-feed--markup-link-action (url)
  "Return a browser action for already validated semantic URL."
  (and (stringp url) (lambda () (browse-url url))))

(defun zulip-feed--insert-markup-fallback (node &optional face action)
  "Insert NODE's visible fallback with optional FACE and ACTION."
  (let ((start (point)))
    (insert (zulip-feed--markup-fallback-text node))
    (when face
      (add-face-text-property start (point) face 'append))
    (when action
      (appkit-ui-add-action start (point) action :face face))
    (cons start (point))))

(defun zulip-feed--insert-markup-document (document)
  "Insert semantic DOCUMENT with Zulip's interactive object policy."
  (appkit-markup-ui-insert-document
   document
   :final-newline-p t
   :interactive-p t
   :link-action #'zulip-feed--markup-link-action
   :object-inserter #'zulip-feed--insert-markup-object))

(defun zulip-feed--insert-markup-object (node)
  "Insert one Zulip provider object NODE with native actions."
  (let* ((value (zulip-feed--markup-value node))
         (kind (and value (zulip-markup-provider-object-kind value)))
         (data (and value (zulip-markup-provider-object-data value))))
    (pcase kind
      ((or 'channel-link 'topic-link 'message-link)
       (zulip-feed--insert-markup-fallback
        node 'zulip-message-navigation-face
        (zulip-feed--markup-navigation-action kind data)))
      ('user-mention
       (let* ((id (plist-get data :id))
              (user-id
               (and (stringp id)
                    (string-match-p "\\`[1-9][0-9]*\\'" id)
                    (string-to-number id)))
              (label (zulip-feed--markup-fallback-text node))
              (action
               (and user-id
                    (lambda ()
                      (zulip-feed-open
                       zulip-feed--account
                       (zulip-narrow-direct user-id label))))))
         (zulip-feed--insert-markup-fallback
          node
          (if (plist-get data :silent-p)
              'zulip-message-silent-mention-face
            'zulip-message-mention-face)
          action)))
      ((or 'group-mention 'wildcard-mention)
       (zulip-feed--insert-markup-fallback
        node
        (if (plist-get data :silent-p)
            'zulip-message-silent-mention-face
          'zulip-message-mention-face)))
      ('timestamp
       (let* ((datetime (plist-get data :datetime))
              (label
               (condition-case nil
                   (format-time-string
                    "%Y-%m-%d %H:%M %Z" (date-to-time datetime))
                 (error (zulip-feed--markup-fallback-text node))))
              (start (point)))
         (insert label)
         (add-face-text-property
          start (point) 'zulip-message-timestamp-face 'append)))
      ('spoiler
       (let* ((header (plist-get data :header))
              (content (plist-get data :content))
              (header-start (point)))
         (when (appkit-markup-document-p header)
           (zulip-feed--insert-markup-document header))
         (add-face-text-property
          header-start (point) 'zulip-message-spoiler-face 'append)
         ;; Initial native cutover preserves the old visible-content behavior.
         ;; The object boundary retains enough semantics for later reveal state.
         (when (appkit-markup-document-p content)
           (zulip-feed--insert-markup-document content))))
      ('media
       (zulip-feed--insert-markup-fallback
        node 'zulip-message-media-face
        (when-let* ((url (or (plist-get data :url)
                             (plist-get data :preview-url))))
          (lambda () (browse-url url)))))
      ('emoji
       (zulip-feed--insert-markup-fallback node))
      (_
       (if (appkit-markup-object-block-p node)
           (zulip-feed--insert-markup-document
            (appkit-markup-document
             (appkit-markup-object-block-fallback node)))
         (zulip-feed--insert-markup-fallback node))))))

(defun zulip-feed--insert-message-body (message)
  "Insert MESSAGE body through the Appkit semantic markup boundary.

Local optimistic content is Markdown source and remains literal until the
server returns authoritative rendered HTML."
  (let ((local-content (zulip-feed--field message 'local-content))
        (content
         (format "%s"
                 (or (zulip-feed--field message 'rendered-content)
                     (zulip-feed--field message 'content)
                     ""))))
    (if (and (stringp local-content)
             (not (zulip-feed--true-p
                   (zulip-feed--field message 'authoritative))))
        (insert local-content)
      (appkit-markup-ui-insert-document
       (zulip-markup-parse
        content
        (and (zulip-account-p zulip-feed--account)
             (zulip-account-server zulip-feed--account)))
       :final-newline-p nil
       :interactive-p t
       :link-action #'zulip-feed--markup-link-action
       :object-inserter #'zulip-feed--insert-markup-object))))

(defun zulip-feed--reaction-groups (message)
  "Return grouped reaction display records for MESSAGE."
  (let ((table (make-hash-table :test #'equal))
        (self-id (and (zulip-state-p (zulip-feed--account-state))
                      (zulip-state-self-user-id
                       (zulip-feed--account-state))))
        order)
    (dolist (reaction
             (zulip-feed--sequence
              (zulip-feed--field message 'reactions)))
      (let* ((name (format "%s"
                           (or (zulip-feed--field reaction 'emoji-name)
                               (zulip-feed--field reaction 'emoji-code)
                               "reaction")))
             (type (format "%s"
                           (or (zulip-feed--field reaction 'reaction-type)
                               "unicode_emoji")))
             (code (zulip-feed--field reaction 'emoji-code))
             (key (cons type name))
             (record (gethash key table))
             (user-id (zulip-feed--field reaction 'user-id)))
        (unless record
          (setq record (list :key key :name name :code code :type type :count 0
                             :selected nil :users nil))
          (puthash key record table)
          (push key order))
        (setq record (plist-put record :count
                                (1+ (or (plist-get record :count) 0))))
        (when user-id
          (setq record
                (plist-put record :users
                           (cons (format "%s" user-id)
                                 (plist-get record :users)))))
        (when (and self-id user-id
                   (equal (format "%s" self-id) (format "%s" user-id)))
          (setq record (plist-put record :selected t)))
        (puthash key record table)))
    (mapcar (lambda (key) (gethash key table)) (nreverse order))))

(defun zulip-feed--reaction-label (reaction)
  "Return one compact chip label for grouped REACTION."
  (format ":%s: %d"
          (plist-get reaction :name)
          (or (plist-get reaction :count) 0)))

(defun zulip-feed--insert-breadcrumb (message breadcrumb prefix)
  "Insert clickable BREADCRUMB for MESSAGE using PREFIX."
  (when (and (stringp breadcrumb) (not (string-empty-p breadcrumb)))
    (appkit-chat-ins-insert-prefixed-line
     breadcrumb
     :prefix prefix
     :face 'zulip-message-context-face
     :action (lambda () (zulip-feed-open-message-context message))
     :help-echo "Open this Zulip topic or direct conversation")))

(cl-defun zulip-feed--insert-message-content
    (message prefix
             &key timestamp target-width left-prefix-width starred-p)
  "Insert MESSAGE content and apply Appkit line PREFIX geometry.

When TIMESTAMP is non-nil, append it at the right edge of the first content
line.  TARGET-WIDTH and LEFT-PREFIX-WIDTH use the same geometry contract as
`appkit-chat-ins-insert-right-aligned-text'.  STARRED-P marks saved messages."
  (let ((start (point)))
    (zulip-feed--insert-message-body message)
    (unless (bolp) (insert "\n"))
    (when (and (stringp timestamp)
               (not (string-empty-p timestamp))
               (< start (point)))
      (save-excursion
        (goto-char start)
        (end-of-line)
        (let ((span
               (appkit-chat-ins-insert-right-aligned-text
                (concat timestamp (if starred-p " ★" ""))
                target-width
                :face 'zulip-message-timestamp-face
                :left-prefix-width left-prefix-width)))
          (when starred-p
            (add-face-text-property
             (1- (cdr span)) (cdr span)
             'font-lock-constant-face 'append)))))
    (appkit-ui-apply-line-prefix start (point) prefix)))

(defun zulip-feed--row-printer (row)
  "Insert timeline ROW using shared telega-style Appkit geometry."
  (let* ((message (appkit-chat-timeline-row-payload row))
         (context (appkit-chat-timeline-row-context row))
         (key (appkit-chat-timeline-row-key row))
         (sender (zulip-feed--message-sender message))
         (pending (zulip-feed--field message 'pending))
         (failed (zulip-feed--field message 'failed))
         (starred (zulip-feed--message-flag-p message "starred"))
         (compact (plist-get context :compact))
         (breadcrumb (plist-get context :breadcrumb))
         (width (max 40 (or zulip-feed--fill-column 80)))
         (avatar-prefixes
          (appkit-chat-avatar-prefixes
           (zulip-media-avatar-image zulip-feed--account message)
           (zulip-feed--sender-fallback message)
           :pixel-size (appkit-chat-avatar-two-line-pixel-size)
           :resize t))
         (header-prefix (or (plist-get avatar-prefixes :header) ""))
         (first-body-prefix
          (or (plist-get avatar-prefixes :first-body) "    "))
         (rest-body-prefix
          (or (plist-get avatar-prefixes :rest-body) "    "))
         (body-prefix
          (if compact
              (appkit-ui-make-prefix-state
               rest-body-prefix rest-body-prefix)
            (appkit-ui-make-prefix-state
             first-body-prefix rest-body-prefix)))
         (properties (zulip-feed--message-properties message key))
         (start (point)))
    (when-let* ((date (plist-get context :insert-date)))
      (appkit-chat-ins-insert-divider-row date 'shadow width properties))
    (when (plist-get context :insert-unread)
      (appkit-chat-ins-insert-divider-row
       "Unread" 'zulip-message-unread-divider-face width properties))
    (if compact
        (progn
          (zulip-feed--insert-breadcrumb message breadcrumb body-prefix)
          (zulip-feed--insert-message-content
           message body-prefix
           :timestamp (zulip-feed--message-time-label message t)
           :target-width width
           :left-prefix-width (string-width rest-body-prefix)
           :starred-p starred))
      (let ((heading-start (point))
            (sender-face (zulip-feed--message-sender-face message)))
        (insert (propertize sender 'face sender-face))
        (when starred
          (insert (propertize "  ★" 'face 'font-lock-constant-face
                              'help-echo "Starred Zulip message")))
        (when pending
          (insert (propertize "  sending…" 'face 'shadow)))
        (appkit-chat-ins-insert-right-aligned-text
         (zulip-feed--message-time-label message t)
         width
         :face 'zulip-message-timestamp-face
         :left-prefix-width (string-width header-prefix))
        (insert "\n")
        (appkit-ui-apply-line-prefix
         heading-start (point)
         (appkit-ui-make-prefix-state header-prefix rest-body-prefix)))
      (zulip-feed--insert-breadcrumb message breadcrumb body-prefix)
      (zulip-feed--insert-message-content message body-prefix))
    (when failed
      (appkit-chat-ins-insert-prefixed-line
       (format "Send failed: %s · click or R to retry" failed)
       :prefix body-prefix :face 'zulip-message-failed-face
       :action (lambda () (zulip-feed-retry-send message))
       :help-echo "Retry this failed Zulip send"))
    (appkit-chat-ins-insert-reaction-line
     (zulip-feed--reaction-groups message)
     :prefix body-prefix
     :selected-face 'font-lock-constant-face
     :unselected-face 'shadow
     :label-function #'zulip-feed--reaction-label
     :selected-p-function (lambda (reaction)
                            (plist-get reaction :selected))
     :action-function
     (lambda (reaction)
       (zulip-feed-toggle-reaction reaction key))
     :help-echo-function
     (lambda (reaction)
       (format "%d reaction%s; click to %s :%s:"
               (or (plist-get reaction :count) 0)
               (if (= (or (plist-get reaction :count) 0) 1) "" "s")
               (if (plist-get reaction :selected) "remove" "add")
               (plist-get reaction :name))))
    (insert "\n")
    (add-text-properties start (point) properties)))

(defun zulip-feed--header-text ()
  "Return the timeline header text.

Feed identity lives in `header-line-format', leaving the keyed message
timeline flush with the top of the buffer like telega chat buffers."
  "")

(defun zulip-feed--header-line ()
  "Return a dynamic account, connection, and narrow header line."
  (let* ((account zulip-feed--account)
         (state (and (zulip-account-p account)
                     (zulip-account-state account)))
         (status (if (and (zulip-account-p account)
                          (zulip-account-connected-p account))
                     "online"
                   "connecting"))
         (unread (and (zulip-state-p state)
                      (zulip-state-unread-count state))))
    (format " Zulip  [%s]  %s  ·  %s%s%s"
            status
            (if (zulip-account-p account)
                (zulip-account-server account)
              "")
            (if (zulip-narrow-p zulip-feed--narrow)
                (zulip-narrow-title zulip-feed--narrow)
              "Messages")
            (if (and (integerp unread) (> unread 0))
                (format "  (%d unread)" unread)
              "")
            (if (and (bound-and-true-p appkit-markup-compose-active-codec)
                     (zulip-feed--composer-visible-p))
                (format "  ·  compose: %s"
                        (appkit-markup-compose-codec-label))
              ""))))

(defun zulip-feed--footer-text ()
  "Return a passive history delimiter for the current feed."
  (let* ((edit-id (zulip-feed--edit-message-id))
         (edit-request-p (zulip-feed--edit-request-p))
         (note
          (cond
           (zulip-feed--last-error
            (propertize (format "Request failed: %s\n" zulip-feed--last-error)
                        'face 'error))
           (edit-id
            (propertize
             (format "%s message %s · C-c C-k cancels\n"
                     (if edit-request-p
                         "Working on"
                       "Editing")
                     edit-id)
             'face 'font-lock-doc-face))
           ((and (not (zulip-feed--composer-visible-p))
                 (memq (zulip-narrow-kind zulip-feed--narrow)
                       '(all channel)))
            (propertize
             "Open a topic or direct conversation to write a message.\n"
             'face 'shadow))
           ((appkit-chat-history-window-partial-p)
            (propertize "Load newer messages to resume composing.\n"
                        'face 'shadow))
           (t ""))))
    (concat "\n"
            (appkit-chat-history-delimiter-string
             (max 8 (or zulip-feed--fill-column 80))
             :loading-text "loading")
            "\n"
            note)))

(defun zulip-feed--composer-visible-p ()
  "Return non-nil when the narrow has an unambiguous send target."
  (or (and (zulip-feed--edit-message-id) t)
      (and (zulip-narrow-send-target zulip-feed--narrow)
           (not (appkit-chat-history-window-partial-p))
           t)))

(defun zulip-feed--bind-composer ()
  "Create or remove the feed's trailing text composer."
  (appkit-chatbuf-bind-input-region
   :visible-p (zulip-feed--composer-visible-p)
   :prompt (if (zulip-feed--edit-message-id) "edit> " ">>> ")
   :input-text (appkit-chatbuf-input-state)))

(defun zulip-feed--ensure-timeline ()
  "Ensure the current view owns its Appkit timeline."
  (appkit-chat-timeline-ensure
   :printer #'zulip-feed--row-printer
   :anchor-property zulip-feed--anchor-property
   :header (zulip-feed--header-text)
   :footer (zulip-feed--footer-text)
   :after-mutation-function #'appkit-chatbuf-update-context-mode))

(defun zulip-feed--project-rows ()
  "Project current ordered state entries into Appkit rows."
  (let* ((messages (zulip-feed--timeline-entries))
         (first-unread (zulip-feed--first-unread-key messages)))
    (appkit-chat-timeline-project
     messages
     #'zulip-feed--message-key
     :context-function
     (lambda (previous message)
       (zulip-feed--message-context previous message first-unread))
     :dependencies-function #'zulip-feed--message-dependencies)))

(defun zulip-feed--rekey-history-edge (old-key new-key)
  "Map exact history edge OLD-KEY to NEW-KEY."
  (when (and (appkit-chat-history-window-known-p)
             old-key new-key (not (equal old-key new-key)))
    (let ((first (appkit-chat-history-window-first-key))
          (last (appkit-chat-history-window-last-key)))
      (when (or (equal first old-key) (equal last old-key))
        (appkit-chat-history-window-set
         (if (equal first old-key) new-key first)
         (if (equal last old-key) new-key last))))))

(defun zulip-feed--reconcile-history-edges ()
  "Repair exact history edges after canonical event projection changes."
  (when (and (appkit-chat-history-window-known-p)
             (not (appkit-chat-history-window-empty-p)))
    (let* ((first (appkit-chat-history-window-first-key))
           (last (appkit-chat-history-window-last-key))
           (old-keys (appkit-chat-timeline-keys))
           (current-keys
            (mapcar #'zulip-feed--message-key
                    (zulip-feed--state-entries)))
           (first-missing (and first (not (member first current-keys))))
           (last-missing (and last (not (member last current-keys))))
           (first-position (and first (seq-position old-keys first #'equal)))
           (last-position (and last (seq-position old-keys last #'equal)))
           (new-first
            (if first-missing
                (and first-position
                     (seq-find
                      (lambda (key) (member key current-keys))
                      (nthcdr (1+ first-position) old-keys)))
              first))
           (new-last
            (if last-missing
                (and last-position
                     (seq-find
                      (lambda (key) (member key current-keys))
                      (reverse (seq-take old-keys last-position))))
              last))
           (new-first-position
            (and new-first (seq-position current-keys new-first #'equal)))
           (new-last-position
            (and new-last (seq-position current-keys new-last #'equal))))
      (when (or first-missing last-missing)
        (cond
         ((and (or (not first-missing) new-first)
               (or (not last-missing) new-last)
               (or (null new-first-position)
                   (null new-last-position)
                   (<= new-first-position new-last-position)))
          (appkit-chat-history-window-set new-first new-last))
         ((and (null current-keys)
               (null last)
               (appkit-chat-history-older-loaded-p))
          (appkit-chat-history-window-establish-empty))
         (t
          (appkit-chat-history-request-cancel)
          (appkit-chat-history-window-clear)
          (setq zulip-feed--history-reload-needed-p t)))))))

(cl-defun zulip-feed--sync-timeline
    (&key rekeys force-keys changed-resources)
  "Synchronize feed with REKEYS, FORCE-KEYS, and CHANGED-RESOURCES."
  (zulip-feed--update-fill-column)
  (zulip-feed--ensure-timeline)
  (dolist (mapping rekeys)
    (zulip-feed--rekey-history-edge (car mapping) (cdr mapping)))
  (let* ((rows (zulip-feed--project-rows))
         (keys (mapcar #'appkit-chat-timeline-row-key rows))
         ;; Appkit requires every rekey target to exist in the new projection.
         (applicable
          (seq-filter
           (lambda (mapping)
             (and (appkit-chat-timeline-node (car mapping))
                  (member (cdr mapping) keys)))
           rekeys)))
    (appkit-chat-timeline-sync
     rows
     :force-keys force-keys
     :changed-resources changed-resources
     :rekeys applicable)))

(defun zulip-feed--update-frame ()
  "Update header, delimiter, and trailing composer in place."
  (zulip-feed--ensure-timeline)
  (appkit-chat-timeline-set-frame
   (zulip-feed--header-text)
   (zulip-feed--footer-text)
   :bind-input-function #'zulip-feed--bind-composer
   :composer-visible-p (zulip-feed--composer-visible-p)))

(defun zulip-feed-render ()
  "Invalidate and synchronously render the current feed through Appkit."
  (interactive)
  (let ((view (appkit-current-view)))
    (unless (appkit-view-live-p view)
      (error "Zulip feed has no live Appkit view"))
    (appkit-invalidate view
                       :structure t
                       :parts '(timeline frame composer))
    (appkit-sync-invalidations view)))

(defun zulip-feed--event-promotion (event)
  "Return EVENT's local-to-server ID mapping, or nil."
  (let* ((message (zulip-feed--field event 'message))
         (local-id (or (zulip-feed--field event 'local-message-id)
                       (zulip-feed--field event 'local-id)
                       (and message
                            (or (zulip-feed--field message 'local-message-id)
                                (zulip-feed--field message 'local-id)))))
         (server-id (and message
                         (ignore-errors
                           (zulip-feed--message-key message)))))
    (and (stringp local-id)
         (string-prefix-p "local-" local-id)
         server-id
         (not (equal local-id server-id))
         (cons local-id server-id))))

(defun zulip-feed--apply-queued-events (events)
  "Apply feed-local bookkeeping for EVENTS and return row rekeys."
  (let (rekeys)
    (dolist (event events)
      (when-let* ((message (zulip-feed--field event 'message))
                  (key (ignore-errors (zulip-feed--message-key message))))
        (when (seq-some (lambda (entry)
                          (equal key (zulip-feed--message-key entry)))
                        (zulip-feed--state-entries))
          ;; An unknown window can safely begin at the first authoritative live
          ;; event.  A known authoritative-empty window uses Appkit's narrower
          ;; empty-to-live transition.
          (if (appkit-chat-history-window-known-p)
              (appkit-chat-history-window-seed-live key)
            (appkit-chat-history-window-set key nil))
          ;; Retain the frontier while latest is in flight so an older empty
          ;; HTTP snapshot cannot erase the concurrent event.
          (when (eq (appkit-chat-history-loading) 'latest)
            (cl-pushnew key zulip-feed--latest-live-keys :test #'equal))))
      (when-let* ((mapping (zulip-feed--event-promotion event)))
        (remhash (car mapping) zulip-feed--pending)
        (push mapping rekeys)))
    (delete-dups (nreverse rekeys))))

(defun zulip-feed--cleanup-pending-read-ids ()
  "Forget read requests already reflected by canonical account state."
  (when (hash-table-p zulip-feed--pending-read-ids)
    (let ((state (zulip-feed--account-state)))
      (maphash
       (lambda (id _present)
         (unless (and (zulip-state-p state)
                      (zulip-state-unread-message-p state id))
           (remhash id zulip-feed--pending-read-ids)))
       zulip-feed--pending-read-ids))))

(defun zulip-feed--sync-invalidations (view invalidations events)
  "Synchronize VIEW from coalesced Appkit INVALIDATIONS and EVENTS."
  (let* ((rekeys (zulip-feed--apply-queued-events events))
         (diff
          (appkit-projection-diff-derive
           invalidations
           :existing-keys
           (and (appkit-chat-timeline-live-p)
                (appkit-chat-timeline-keys))
           :reconcile-parts '(timeline)
           :reconcile rekeys)))
    (zulip-feed--cleanup-pending-read-ids)
    ;; Promotion changes opaque keys before generic missing-edge repair.
    (dolist (mapping rekeys)
      (zulip-feed--rekey-history-edge (car mapping) (cdr mapping)))
    (zulip-feed--reconcile-history-edges)
    (when (appkit-projection-diff-reconcile-p diff)
      (zulip-feed--sync-timeline
       :rekeys rekeys
       :force-keys (appkit-projection-diff-force-keys diff)
       :changed-resources
       (appkit-projection-diff-changed-dependencies diff)))
    (when (or events
              (appkit-invalidations-structure-p invalidations)
              (appkit-invalidations-parts invalidations))
      (zulip-feed--update-frame))
    ;; Edit HTTP callbacks only settle canonical model state.  Composer text
    ;; and any requested point movement belong to this exact-view projection
    ;; transaction and remain guarded by the originating edit generation.
    (zulip-feed--run-edit-sync-request view)
    ;; Exact-anchor navigation belongs to the projection transaction.  History
    ;; callbacks only update the window/domain state and request a coalesced
    ;; sync, so the target position does not exist until this point.
    (when-let* ((target zulip-feed--pending-jump-id)
                (position (appkit-chat-timeline-key-position target)))
      (setq zulip-feed--pending-jump-id nil)
      (goto-char position))
    ;; Event bookkeeping above is idempotent.  Appkit retains the whole batch
    ;; when any later projection step fails.
    (when zulip-feed--history-reload-needed-p
      (setq zulip-feed--history-reload-needed-p nil)
      (unless (appkit-chat-history-loading-p)
        (zulip-feed-load-latest)))
    (when (appkit-scroll-observer-p zulip-feed--scroll-observer)
      (appkit-scroll-observer-check zulip-feed--scroll-observer))))

(defun zulip-feed--event-message-ids (event)
  "Return canonical message IDs directly named by EVENT."
  (delete-dups
   (delq nil
         (mapcar
          (lambda (id)
            (condition-case nil
                (zulip-state-message-id id)
              (error nil)))
          (append
           (when-let* ((message (zulip-feed--field event 'message))
                       (id (zulip-feed--field message 'id)))
             (list id))
           (when-let* ((id (zulip-feed--field event 'message-id)))
             (list id))
           (zulip-feed--sequence
            (zulip-feed--field event 'message-ids)))))))

(defun zulip-feed--event-resource-keys (event)
  "Return presentation resource keys directly changed by EVENT."
  (let ((type (downcase (format "%s" (zulip-feed--field event 'type))))
        resources)
    (pcase type
      ("realm_user"
       (when-let* ((person (or (zulip-feed--field event 'person)
                               (zulip-feed--field event 'user)))
                   (id (or (zulip-feed--field person 'id)
                           (zulip-feed--field person 'user-id))))
         (push (list :user (format "%s" id)) resources)))
      ((or "stream" "subscription")
       (dolist (channel
                (zulip-feed--sequence
                 (or (zulip-feed--field event 'subscriptions)
                     (zulip-feed--field event 'streams)
                     (zulip-feed--field event 'stream))))
         (when-let* ((id (or (zulip-feed--field channel 'id)
                             (zulip-feed--field channel 'stream-id))))
           (push (list :channel (format "%s" id)) resources)))))
    (delete-dups resources)))

(defun zulip-feed--ids-in-view-state-p (view ids state)
  "Return non-nil when any IDS belongs to VIEW's narrow in STATE."
  (and (zulip-state-p state)
       (appkit-view-live-p view)
       (let* ((narrow (appkit-view-state view))
              (key (and (zulip-narrow-p narrow)
                        (zulip-narrow-key narrow)))
              (indexed (and (zulip-narrow-p narrow)
                            (zulip-state-message-ids state key))))
         (seq-some (lambda (id) (member id indexed)) ids))))

(defun zulip-feed--event-relevant-to-view-p
    (view event old-state new-state resources)
  "Return non-nil when EVENT can change VIEW's projection.

OLD-STATE and NEW-STATE bracket the transition, while RESOURCES names any
Appkit resources affected by it."
  (let ((ids (zulip-feed--event-message-ids event))
        (type (downcase (format "%s" (zulip-feed--field event 'type)))))
    (cond
     ((equal type "register") t)
     (ids
      (or (zulip-feed--ids-in-view-state-p view ids old-state)
          (zulip-feed--ids-in-view-state-p view ids new-state)))
     (resources t)
     ;; Unread policy and subscription metadata can change the header or the
     ;; first-unread divider without naming a cached message.
     ((member type '("user_topic" "subscription" "stream")) t)
     (t nil))))

(defun zulip-feed--notify-account-views
    (account event &optional old-state new-state)
  "Queue relevant EVENT invalidations for ACCOUNT's live feed views."
  (let ((app (zulip-account-app account)))
    (when (appkit-app-live-p app)
      (let ((ids (zulip-feed--event-message-ids event))
            (resources (zulip-feed--event-resource-keys event))
            (type (downcase (format "%s"
                                    (zulip-feed--field event 'type)))))
        (maphash
         (lambda (_id view)
           (when (and (appkit-view-live-p view)
                      (eq (appkit-view-mode view) 'zulip-feed-mode)
                      (zulip-feed--event-relevant-to-view-p
                       view event old-state new-state resources))
             (appkit-view-enqueue-event view event)
             (appkit-request-sync
              view
              :structure (member type '("message" "history"
                                        "local_message" "delete_message"
                                        "local_message_promoted"
                                        "local_send_failed"))
              :parts '(timeline frame)
              :entries ids
              :resources resources)))
         (appkit-app-view-registry app))))))

(defun zulip-feed--consume-state-change
    (account event old-state new-state)
  "Consume ACCOUNT's already-published EVENT transition.

OLD-STATE and NEW-STATE bracket the canonical state transition."
  (when (equal (downcase (format "%s" (zulip-feed--field event 'type)))
               "realm_user")
    (zulip-completion-invalidate-account-cache account))
  (when-let* ((mapping (zulip-feed--event-promotion event)))
    (remhash (car mapping)
             (zulip-feed--account-table account 'pending)))
  (zulip-feed--notify-account-views
   account event old-state new-state))

(defun zulip-feed--on-state-changed (change)
  "Consume one structured `zulip-state-changed' CHANGE descriptor."
  (when-let* ((account (plist-get change :account))
              ((zulip-account-p account)))
    (zulip-feed--consume-state-change
     account
     (plist-get change :event)
     (plist-get change :old-state)
     (plist-get change :state))))

(defun zulip-feed--on-app-event (account event _old-state new-state)
  "Publish and fan out one client-originated Zulip EVENT for ACCOUNT.

The protocol event loop uses `zulip-feed--on-state-changed' after publishing
through the runtime.  This compatibility entry remains for optimistic local
transitions and focused reducer tests."
  (when (zulip-account-p account)
    (let ((old-state (zulip-account-state account)))
      (when new-state
        (zulip-feed--set-account-state account new-state))
      (zulip-feed--consume-state-change
       account event old-state (or new-state old-state)))))

(defun zulip-feed--on-register (account new-state)
  "Refresh feed views after ACCOUNT registration installs NEW-STATE."
  (when (zulip-account-p account)
    ;; Queue replacement must not erase sends that were accepted locally but
    ;; have not yet received either their HTTP response or correlated event.
    (setq new-state (zulip-feed--rebase-pending account new-state))
    (zulip-feed--set-account-state account new-state)
    (zulip-feed--notify-account-views
     account '((type . register)) nil new-state)
    ;; A register response is a new protocol epoch.  Invalidate both exact
    ;; history edges and active request owners before loading against the new
    ;; snapshot; callbacks from the previous queue then become harmless.
    (let ((app (zulip-account-app account)))
      (when (appkit-app-live-p app)
        (maphash
         (lambda (_id view)
           (when (and (appkit-view-live-p view)
                      (eq (appkit-view-mode view) 'zulip-feed-mode))
             (appkit-with-live-view view
               (appkit-chat-history-request-cancel)
               (appkit-chat-history-window-clear)
               (when-let* ((pending
                            (seq-filter
                             (lambda (message)
                               (zulip-feed--true-p
                                (zulip-feed--field message 'pending)))
                             (zulip-feed--state-entries))))
                 (appkit-chat-history-window-set
                  (zulip-feed--message-key (car pending)) nil))
               (zulip-feed-load-initial))))
         (appkit-app-view-registry app))))))

(defun zulip-feed--ensure-account-subscriptions (account)
  "Install one Appkit-owned event fanout for ACCOUNT."
  (let* ((app (zulip-account-app account))
         (table (appkit-app-request-table app)))
    (unless (gethash 'zulip-feed-state-subscription table)
      (puthash 'zulip-feed-state-subscription
               (appkit-app-on
                app 'zulip-state-changed #'zulip-feed--on-state-changed)
               table))
    (unless (gethash 'zulip-feed-register-subscription table)
      (puthash 'zulip-feed-register-subscription
               (appkit-app-on app 'zulip-register #'zulip-feed--on-register)
               table))))

(defun zulip-feed--result-ok-p (result)
  "Return non-nil when API RESULT is successful."
  (if (fboundp 'zulip-api-result-ok-p)
      (zulip-api-result-ok-p result)
    (zulip-feed--true-p (zulip-feed--field result 'ok))))

(defun zulip-feed--result-data (result)
  "Return payload data from API RESULT."
  (if (fboundp 'zulip-api-result-data)
      (zulip-api-result-data result)
    (zulip-feed--field result 'data)))

(defun zulip-feed--result-message (result)
  "Return a human-readable error from API RESULT."
  (format "%s"
          (or (and (fboundp 'zulip-api-result-message)
                   (zulip-api-result-message result))
              (zulip-feed--field result 'message)
              "Zulip API request failed")))

(defun zulip-feed--merge-history-messages (messages)
  "Merge API MESSAGES into the current account state."
  (let* ((state (zulip-feed--account-state))
         (key (zulip-narrow-key zulip-feed--narrow))
         (messages
          (mapcar
           (lambda (message)
             (zulip-state-object-put message 'authoritative t))
           (zulip-feed--sequence messages)))
         (next
          (if (fboundp 'zulip-state-merge-messages)
              (zulip-state-merge-messages state messages key)
            (progn
              (unless (fboundp 'zulip-state-upsert-message)
                (error "No Zulip state merge function is available"))
              (dolist (message messages state)
                (setq state (zulip-state-upsert-message state message)))))))
    (zulip-feed--set-account-state zulip-feed--account next)
    messages))

(defun zulip-feed--history-succeeded (kind previous-first data)
  "Merge KIND history DATA after the edge PREVIOUS-FIRST."
  (let* ((messages (zulip-feed--merge-history-messages
                    (zulip-feed--field data 'messages)))
         (previous-last (appkit-chat-history-window-last-key))
         (first (and messages (zulip-feed--message-key (car messages))))
         (last (and messages
                    (zulip-feed--message-key (car (last messages)))))
         (found-oldest
          (zulip-feed--true-p (zulip-feed--field data 'found-oldest)))
         (found-newest
          (zulip-feed--true-p (zulip-feed--field data 'found-newest))))
    (pcase kind
      ((or 'latest 'around)
       (if messages
           (appkit-chat-history-window-set
            first (unless found-newest last))
         (progn
           (appkit-chat-history-window-establish-empty)
           ;; An empty server page may predate a concurrent message event or
           ;; race an in-flight send rebased across registration.  Attach the
           ;; first surviving live/local row to the otherwise-empty edge.
           (when-let* ((entry
                        (or
                         (seq-find
                          (lambda (message)
                            (member (zulip-feed--message-key message)
                                    zulip-feed--latest-live-keys))
                          (zulip-feed--state-entries))
                         (seq-find
                          (lambda (message)
                            (zulip-feed--true-p
                             (zulip-feed--field message 'pending)))
                          (zulip-feed--state-entries)))))
             (appkit-chat-history-window-seed-live
              (zulip-feed--message-key entry))))))
      ('older
       (when first
         (appkit-chat-history-window-set
          first (appkit-chat-history-window-last-key))))
      ('newer
       (cond
        (last
         (appkit-chat-history-window-set
          (appkit-chat-history-window-first-key)
          (unless found-newest last))
         ;; A page containing only the current edge is just as stalled as an
         ;; empty page.  Record it so Appkit's automatic newer-page gate does
         ;; not spin on an unchanged anchor.
         (when (and (not found-newest) (equal last previous-last))
           (appkit-chat-history-newer-stalled-set previous-last)))
        (found-newest
         (appkit-chat-history-window-set
          (appkit-chat-history-window-first-key) nil))
        (t
         ;; Zulip can transiently return an empty page without declaring that
         ;; the live edge was reached.  Preserve the exact edge, but suppress
         ;; automatic retries until that edge changes.
         (appkit-chat-history-newer-stalled-set previous-last)))))
    (when (eq kind 'latest)
      (setq zulip-feed--latest-live-keys nil))
    (when (or found-oldest
              (and (eq kind 'older)
                   (or (null first) (equal first previous-first))))
      (appkit-chat-history-older-loaded-set t))
    messages))


(defun zulip-feed--history-finished
    (view owner kind previous-first result)
  "Finish VIEW history OWNER of KIND using RESULT.
PREVIOUS-FIRST is the history window's older edge before the request."
  (appkit-with-live-view view
    (when (appkit-chat-history-request-end owner)
      (let ((old-state (zulip-feed--account-state))
            messages)
        (if (zulip-feed--result-ok-p result)
            (progn
              (setq zulip-feed--last-error nil)
              (setq messages
                    (zulip-feed--history-succeeded
                     kind previous-first
                     (zulip-feed--result-data result))))
          (setq zulip-feed--last-error
                (zulip-feed--result-message result)))
        ;; Generated content has exactly one mutation entrance: the Appkit
        ;; view sync function.  HTTP completion only records state/events and
        ;; requests one coalesced projection; even a synchronous transport mock
        ;; must not flush the view from inside its callback.
        (let ((event
               (list (cons 'type "history")
                     (cons 'message_ids
                           (vconcat
                            (mapcar #'zulip-feed--message-key messages))))))
          (zulip-feed--notify-account-views
           zulip-feed--account event old-state
           (zulip-feed--account-state)))
        ;; A failed or empty history result may not fan out a relevant event,
        ;; but the loading/error frame still changed.  This request coalesces
        ;; with the event-driven request above when both are present.
        (appkit-request-sync view :parts '(timeline frame))))))


(defun zulip-feed--load-history (kind anchor before after)
  "Load history KIND around ANCHOR with BEFORE and AFTER limits."
  (unless (fboundp 'zulip-api-get-messages)
    (error "Zulip API message history is unavailable"))
  (when (appkit-chat-history-loading-p)
    (user-error "A Zulip history request is already active"))
  (when (eq kind 'latest)
    (setq zulip-feed--latest-live-keys nil))
  (let* ((view (appkit-current-view))
         (owner (appkit-chat-history-request-start view kind))
         (previous-first (appkit-chat-history-window-first-key)))
    ;; Beginning a request changes passive frame state (the loading delimiter
    ;; and, for partial windows, composer availability).  Even when this load
    ;; originates in a register callback, generated content is only mutated by
    ;; the view's Appkit synchronization transaction.
    (appkit-request-sync view :parts '(frame composer))
    (zulip-api-get-messages
     zulip-feed--account
     (zulip-narrow-api-json zulip-feed--narrow)
     anchor before after
     (lambda (result)
       (zulip-feed--history-finished
        view owner kind previous-first result))
     :owner owner)
    owner))

(defun zulip-feed-load-latest ()
  "Load the newest page for the current feed."
  (interactive)
  (zulip-feed--load-history
   'latest "newest" zulip-history-page-size 0))

(defun zulip-feed--unread-details-match-narrow-p
    (details narrow &optional message-id)
  "Return non-nil when unread DETAILS and MESSAGE-ID match NARROW."
  (pcase (zulip-narrow-kind narrow)
    ('all t)
    ('channel
     (let ((operand (zulip-narrow-channel-operand narrow))
           (channel-id (zulip-feed--field details 'channel-id)))
       (or (equal (format "%s" operand) (format "%s" channel-id))
           (when-let* ((channel (zulip-state-channel
                                 (zulip-feed--account-state) channel-id)))
             (zulip-feed--string-equal-ignore-case
              (format "%s" operand)
              (format "%s" (or (zulip-feed--field channel 'name) "")))))))
    ('topic
     (and (zulip-feed--unread-details-match-narrow-p
           details
           (zulip-narrow-channel
            (zulip-narrow-channel-operand narrow))
           message-id)
          (zulip-feed--string-equal-ignore-case
           (format "%s" (zulip-narrow-topic-name narrow))
           (format "%s" (or (zulip-feed--field details 'topic) "")))))
    ('direct
     (equal
      (zulip-state--narrow-dm-operands
       (zulip-narrow-recipient-ids narrow))
      (zulip-state--narrow-dm-operands
       (zulip-state--direct-narrow-ids
        (zulip-feed--account-state) details))))
    ('mentioned
     (and message-id
          (gethash message-id
                   (zulip-state-unread-mentions
                    (zulip-feed--account-state)))))
    ('starred
     (and message-id
          (when-let* ((message
                       (zulip-state-message
                        (zulip-feed--account-state) message-id)))
            (zulip-feed--message-flag-p message "starred"))))
    (_ nil)))

(defun zulip-feed--narrow-has-unread-p ()
  "Return non-nil when canonical state knows this narrow has unread content."
  (let ((state (zulip-feed--account-state))
        found)
    (when (zulip-state-p state)
      (maphash
       (lambda (id _present)
         (when (and (not found)
                    (zulip-feed--unread-details-match-narrow-p
                     (gethash id (zulip-state-unread-details state))
                     zulip-feed--narrow id))
           (setq found t)))
       (zulip-state-unread state)))
    found))

(defun zulip-feed-load-initial ()
  "Load first unread with context, or the newest page when fully read."
  (interactive)
  (if (zulip-feed--narrow-has-unread-p)
      (let* ((context (min 10 (max 0 (/ zulip-history-page-size 4))))
             (after (max 0 (- zulip-history-page-size context 1))))
        (zulip-feed--load-history
         'latest "first_unread" context after))
    (zulip-feed-load-latest)))

(defun zulip-feed-load-older ()
  "Load one older page for the current exact history window."
  (interactive)
  (unless (appkit-chat-history-window-known-p)
    (user-error "Zulip history has not been initialized"))
  (if (appkit-chat-history-older-loaded-p)
      (message "Oldest Zulip message already loaded")
    (zulip-feed--load-history
     'older
     (or (appkit-chat-history-window-first-key) "oldest")
     zulip-history-page-size 0)))

(defun zulip-feed-load-newer ()
  "Load one newer page when the current history window is partial."
  (interactive)
  (unless (appkit-chat-history-window-partial-p)
    (user-error "Zulip feed is already attached to latest"))
  (zulip-feed--load-history
   'newer (appkit-chat-history-window-last-key)
   0 zulip-history-page-size))

(defun zulip-feed--new-local-id ()
  "Return a unique optimistic row key."
  (format "local-%d-%d"
          (truncate (* 1000 (float-time)))
          (cl-incf zulip-feed--local-sequence)))

(defun zulip-feed--pending-message
    (local-id queue-id content target &optional local-content)
  "Return optimistic LOCAL-ID on QUEUE-ID with CONTENT for TARGET.

CONTENT is outbound Zulip Markdown.  LOCAL-CONTENT, when non-nil, is the
human-readable composer projection used for optimistic display."
  (let ((message
         (list (cons 'id local-id)
               (cons 'local-id local-id)
               (cons 'queue-id queue-id)
               (cons 'pending t)
               (cons 'narrow-key (zulip-narrow-key zulip-feed--narrow))
               (cons 'type (plist-get target :type))
               (cons 'content content)
               (cons 'local-content (or local-content content))
               (cons 'sender-email (zulip-account-email zulip-feed--account))
               (cons 'timestamp (float-time)))))
    (when (equal (plist-get target :type) "stream")
      (push (cons 'display-recipient (plist-get target :to)) message)
      (push (cons 'stream-id (and (integerp (plist-get target :to))
                                  (plist-get target :to)))
            message)
      (push (cons 'subject (plist-get target :topic)) message))
    (when (equal (plist-get target :type) "direct")
      ;; Preserve the exact destination on the optimistic object.  The state
      ;; reducer combines these IDs with the current user, so simultaneous
      ;; pending DMs to different participant sets cannot collapse into the
      ;; self-DM conversation before the authoritative event arrives.
      (push (cons 'recipients (copy-sequence (plist-get target :to))) message))
    message))

(defun zulip-feed--copy-promoted-message (message server-id local-id)
  "Return MESSAGE promoted from LOCAL-ID to SERVER-ID."
  (let ((copy (if (hash-table-p message)
                  (copy-hash-table message)
                (copy-tree message))))
    (if (hash-table-p copy)
        (progn
          (puthash "id" server-id copy)
          (puthash "local_id" local-id copy)
          (puthash "pending" nil copy))
      (setf (alist-get 'id copy) server-id
            (alist-get 'local-id copy) local-id
            (alist-get 'pending copy) nil))
    copy))

(defun zulip-feed--mark-send-failed (account local-id reason)
  "Mark ACCOUNT's pending LOCAL-ID failed with REASON."
  (let ((pending (zulip-feed--account-table account 'pending)))
    (when-let* ((message (or (gethash local-id pending)
                             (zulip-state-message
                              (zulip-account-state account) local-id))))
      (let ((copy (copy-tree message))
            (old-state (zulip-account-state account)))
        (setf (alist-get 'pending copy) nil
              (alist-get 'failed copy) reason)
        ;; The canonical state retains the failed row for display and for a
        ;; late authoritative local_message_id event.  It is no longer an
        ;; in-flight operation, so do not leak it in the account registry.
        (remhash local-id pending)
        (let ((new-state (zulip-state-upsert-message old-state copy)))
          (zulip-feed--on-app-event
           account
           (list (cons 'type "local_send_failed")
                 (cons 'message copy))
           old-state new-state)
          copy)))))

(defun zulip-feed--promote-local-message (account local-id server-id)
  "Promote ACCOUNT's optimistic LOCAL-ID to SERVER-ID exactly once."
  (unless (or (null server-id) (equal local-id server-id))
    (let ((pending (zulip-feed--account-table account 'pending)))
      (when-let* ((message (gethash local-id pending)))
        (let* ((promoted
                (zulip-feed--copy-promoted-message
                 message server-id local-id))
               (old-state (zulip-account-state account))
               (new-state (zulip-state-upsert-message old-state promoted)))
          (remhash local-id pending)
          ;; Feed views perform the Appkit row rekey while consuming this
          ;; account event.  The state transition itself does not depend on
          ;; the originating view still being alive.
          (zulip-feed--on-app-event
           account
           (list (cons 'type "local_message_promoted")
                 (cons 'local_message_id local-id)
                 (cons 'message promoted))
           old-state new-state)
          server-id)))))

(defun zulip-feed--send-finished (account local-id result)
  "Finish ACCOUNT's optimistic LOCAL-ID from send API RESULT."
  (when (and (zulip-account-p account)
             (appkit-app-live-p (zulip-account-app account))
             (gethash local-id
                      (zulip-feed--account-table account 'pending)))
    (if (zulip-feed--result-ok-p result)
        (let* ((data (zulip-feed--result-data result))
               (id (zulip-feed--field data 'id))
               (server-id
                (cond ((stringp id) id)
                      ((integerp id) (number-to-string id))
                      (t nil))))
          (if server-id
              (zulip-feed--promote-local-message
               account local-id server-id)
            (zulip-feed--mark-send-failed
             account local-id "send response omitted message ID")))
      (zulip-feed--mark-send-failed
       account local-id (zulip-feed--result-message result)))))

(defun zulip-feed--compose-snapshot (&optional prefix)
  "Return one immutable semantic composer snapshot selected by PREFIX."
  (appkit-chatbuf-input-state-sync)
  (let* ((input (appkit-chatbuf-input-state))
         (capture (appkit-markup-compose-capture prefix))
         (output
          (appkit-markup-compose-output
           capture 'markdown
           :object-printer #'zulip-completion-markup-object-printer))
         (losses (appkit-markup-compose-output-losses output)))
    (when losses
      (user-error "Zulip markup conversion would lose %s"
                  (appkit-markup-loss-kind (car losses))))
    (list :input input
          :capture capture
          :content (appkit-markup-compose-output-source output)
          :local-content
          (appkit-markup-plain-text
           (appkit-markup-compose-output-document output)))))

(defun zulip-feed-preview-message (&optional prefix)
  "Preview the current composer using the markup codec selected by PREFIX."
  (interactive "P")
  (unless (zulip-feed--composer-visible-p)
    (user-error "This feed has no writable composer"))
  (appkit-chatbuf-input-state-sync)
  (let* ((capture (appkit-markup-compose-capture prefix))
         (buffer (get-buffer-create "*Zulip Compose Preview*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (appkit-markup-compose-preview capture)
        (goto-char (point-min))
        (setq-local buffer-read-only t)
        (setq-local truncate-lines nil)))
    (display-buffer buffer)
    buffer))

(defun zulip-feed-send-message (&optional prefix)
  "Send composer contents with markup selected by PREFIX, or submit an edit."
  (interactive "P")
  (zulip-feed--assert-edit-composer-mutable)
  (if (zulip-feed--edit-message-id)
      (zulip-feed-submit-edit prefix)
    (let* ((account zulip-feed--account)
           (target (zulip-narrow-send-target zulip-feed--narrow))
           (queue-id (zulip-account-queue-id account)))
      (unless target
        (user-error "This Zulip feed does not identify a send destination"))
      (when (appkit-chat-history-window-partial-p)
        (user-error "Load newer Zulip messages before sending"))
      (unless (and (stringp queue-id) (not (string-empty-p queue-id)))
        (user-error "Zulip event queue is not ready"))
      (unless (and (fboundp 'zulip-api-send-message)
                   (fboundp 'zulip-state-upsert-message))
        (error "Zulip send/state interfaces are unavailable"))
      (let* ((snapshot (zulip-feed--compose-snapshot prefix))
             (input (plist-get snapshot :input))
             (content (plist-get snapshot :content))
             (local-content (plist-get snapshot :local-content)))
        (when (string-empty-p (string-trim content))
          (user-error "Message is empty"))
        (let* ((local-id (zulip-feed--new-local-id))
               (message (zulip-feed--pending-message
                         local-id queue-id content target local-content))
               (state (zulip-feed--account-state))
               (next-state
                (zulip-state-upsert-message
                 state message (list (zulip-narrow-key zulip-feed--narrow))))
               (view (appkit-current-view)))
          (puthash local-id message zulip-feed--pending)
          (cond
           ((appkit-chat-history-window-empty-p)
            (appkit-chat-history-window-seed-live local-id))
           ((not (appkit-chat-history-window-known-p))
            (appkit-chat-history-window-set local-id nil))
           ((null (appkit-chat-history-window-first-key))
            (appkit-chat-history-window-set
             local-id (appkit-chat-history-window-last-key))))
          (zulip-feed--on-app-event
           account
           (list (cons 'type "local_message")
                 (cons 'message message))
           state next-state)
          (when (appkit-view-live-p view)
            (appkit-sync-invalidations view))
          (appkit-chatbuf-input-history-push input)
          (appkit-chatbuf-input-set-text "")
          (zulip-api-send-message
           account
           (plist-get target :type)
           (plist-get target :to)
           (plist-get target :topic)
           content
           (lambda (result)
             (zulip-feed--send-finished account local-id result))
           :local-id local-id
           :queue-id queue-id)
          local-id)))))


(defun zulip-feed-message-id-at-point (&optional position)
  "Return the stable message ID at POSITION or point."
  (let ((position (or position (point))))
    (or (get-text-property position zulip-feed--anchor-property)
        (and (> position (point-min))
             (get-text-property (1- position)
                                zulip-feed--anchor-property))
        (get-text-property (line-beginning-position)
                           zulip-feed--anchor-property))))

(defun zulip-feed-message-at-point (&optional position)
  "Return the canonical Zulip message at POSITION or point."
  (when-let* ((id (zulip-feed-message-id-at-point position)))
    (zulip-state-message (zulip-feed--account-state) id)))

(defun zulip-feed--server-message-required (&optional message-or-id)
  "Return a canonical server message selected by MESSAGE-OR-ID or point."
  (let* ((message
          (cond
           ((null message-or-id) (zulip-feed-message-at-point))
           ((stringp message-or-id)
            (zulip-state-message
             (zulip-feed--account-state) message-or-id))
           (t message-or-id)))
         (id (and message (ignore-errors
                            (zulip-feed--message-key message)))))
    (unless message (user-error "No Zulip message at point"))
    (unless (zulip-state-server-message-id-p id)
      (user-error "This action requires an acknowledged Zulip message"))
    message))

(defun zulip-feed--failed-local-message-required (&optional message)
  "Return MESSAGE or point message when it is a failed optimistic send."
  (let* ((message (or message (zulip-feed-message-at-point)))
         (id (and message
                  (ignore-errors (zulip-feed--message-key message)))))
    (unless message (user-error "No Zulip message at point"))
    (unless (and (stringp id)
                 (string-prefix-p "local-" id)
                 (zulip-feed--field message 'failed))
      (user-error "This is not a failed local Zulip send"))
    message))

(defun zulip-feed-retry-send (&optional message)
  "Retry failed optimistic MESSAGE using its original local identity."
  (interactive)
  (let* ((account zulip-feed--account)
         (message (zulip-feed--failed-local-message-required message))
         (local-id (zulip-feed--message-key message))
         (target (zulip-narrow-send-target zulip-feed--narrow))
         (queue-id (zulip-account-queue-id account))
         (content (zulip-feed--field message 'content))
         (pending (zulip-feed--account-table account 'pending))
         (view (appkit-current-view)))
    (unless target
      (user-error "Open the original topic or direct feed to retry this send"))
    (when (appkit-chat-history-window-partial-p)
      (user-error "Load newer Zulip messages before retrying"))
    (unless (and (stringp queue-id) (not (string-empty-p queue-id)))
      (user-error "Zulip event queue is not ready"))
    (unless (stringp content)
      (user-error "Failed Zulip row has no retryable Markdown source"))
    (when (gethash local-id pending)
      (user-error "This Zulip send is already being retried"))
    (let* ((retry (copy-tree message))
           (old-state (zulip-feed--account-state)))
      (setf (alist-get 'pending retry) t
            (alist-get 'failed retry) nil
            (alist-get 'queue-id retry) queue-id)
      (puthash local-id retry pending)
      (let ((new-state
             (zulip-state-upsert-message
              old-state retry (list (zulip-narrow-key zulip-feed--narrow)))))
        (zulip-feed--on-app-event
         account
         (list (cons 'type "local_message") (cons 'message retry))
         old-state new-state))
      (when (appkit-view-live-p view)
        (appkit-sync-invalidations view))
      ;; Like an ordinary send, the retry belongs to the account app.  Its
      ;; authoritative response may arrive after the origin view is closed.
      (zulip-api-send-message
       account
       (plist-get target :type)
       (plist-get target :to)
       (plist-get target :topic)
       content
       (lambda (result)
         (zulip-feed--send-finished account local-id result))
       :local-id local-id :queue-id queue-id)
      (message "Zulip: retrying failed send")
      local-id)))

(defun zulip-feed--message-flags (message)
  "Return MESSAGE's personal flags as strings."
  (mapcar (lambda (flag) (format "%s" flag))
          (zulip-feed--sequence (zulip-feed--field message 'flags))))

(defun zulip-feed--message-flag-p (message flag)
  "Return non-nil when MESSAGE contains personal FLAG."
  (member (format "%s" flag) (zulip-feed--message-flags message)))

(defun zulip-feed--request-action-frame (view)
  "Request a coalesced refresh of passive action state in Appkit VIEW."
  (appkit-request-sync view :parts '(frame composer)))

(defun zulip-feed--record-action-error (view description result)
  "Present failed DESCRIPTION from RESULT in live Appkit VIEW."
  (let ((reason (zulip-feed--result-message result)))
    (appkit-with-live-view view
      (setq zulip-feed--last-error reason)
      (zulip-feed--request-action-frame view))
    (message "Zulip: %s failed: %s" description reason)
    reason))

(cl-defun zulip-feed--request-flags
    (message-ids operation flag description
                 &key quiet on-success on-failure)
  "Apply OPERATION FLAG to MESSAGE-IDS and describe it as DESCRIPTION.

The HTTP process is owned by the current Appkit view.  QUIET suppresses the
ordinary success message; ON-SUCCESS and ON-FAILURE receive the API result."
  (let* ((ids (delete-dups
               (mapcar #'zulip-state-message-id message-ids)))
         (view (appkit-current-view)))
    (unless ids (user-error "No Zulip messages need that flag update"))
    (unless (appkit-view-live-p view)
      (user-error "This Zulip feed is no longer live"))
    (zulip-api-update-message-flags
     zulip-feed--account (vconcat ids) operation flag
     (lambda (result)
       ;; plz invokes transport callbacks from its process buffer.  Flag
       ;; continuations own feed-local read/edit bookkeeping, so they must run
       ;; in the captured live view rather than whichever buffer happens to be
       ;; current when the response arrives.
       (appkit-with-live-view view
         (if (zulip-feed--result-ok-p result)
             (progn
               (when on-success (funcall on-success result))
               (unless quiet (message "Zulip: %s" description)))
           (when on-failure (funcall on-failure result))
           (zulip-feed--record-action-error view description result))))
     :owner view)))

(defun zulip-feed--timeline-target-newer-p (candidate previous)
  "Return non-nil when CANDIDATE follows PREVIOUS in this exact window."
  (let* ((keys (appkit-chat-timeline-keys))
         (candidate-index (seq-position keys candidate #'equal))
         (previous-index (and previous
                              (seq-position keys previous #'equal))))
    (and candidate-index
         (not (equal candidate previous))
         (or (null previous-index)
             (> candidate-index previous-index)))))

(defun zulip-feed--read-target-at-position (&optional position)
  "Return the exact timeline read target represented by POSITION."
  (let ((keys (appkit-chat-timeline-keys))
        (position (or position (point))))
    (if (appkit-chatbuf-point-in-input-p position)
        (car (last keys))
      (zulip-feed-message-id-at-point position))))

(defun zulip-feed--unread-ids-through-position (&optional position automatic)
  "Return loaded unread IDs through POSITION.

When AUTOMATIC is non-nil, retain messages the user explicitly marked unread."
  (let* ((keys (appkit-chat-timeline-keys))
         (target (zulip-feed--read-target-at-position position))
         (index (and target (seq-position keys target #'equal)))
         (state (zulip-feed--account-state)))
    (when index
      (seq-filter
       (lambda (id)
         (and (zulip-state-server-message-id-p id)
              (zulip-state-unread-message-p state id)
              (not (and (hash-table-p zulip-feed--pending-read-ids)
                        (gethash id zulip-feed--pending-read-ids)))
              (not (and automatic
                        (hash-table-p zulip-feed--auto-read-suppressed-ids)
                        (gethash id zulip-feed--auto-read-suppressed-ids)))))
       (seq-take keys (1+ index))))))

(cl-defun zulip-feed--mark-read-through
    (&optional position automatic quiet)
  "Mark loaded unread messages through POSITION read.

AUTOMATIC applies duplicate/suppression gates.  QUIET suppresses success
messages.  Return the submitted IDs, or nil when no request is needed."
  (when (and (zulip-account-p zulip-feed--account)
             (zulip-account-connected-p zulip-feed--account))
    (let* ((target (zulip-feed--read-target-at-position position))
           (ids (and target
                     (or (not automatic)
                         (zulip-feed--timeline-target-newer-p
                          target zulip-feed--last-read-target-id))
                     (zulip-feed--unread-ids-through-position
                      position automatic))))
      (when ids
        (dolist (id ids)
          (puthash id t zulip-feed--pending-read-ids)
          (remhash id zulip-feed--auto-read-suppressed-ids))
        (when automatic (setq zulip-feed--last-read-target-id target))
        (zulip-feed--request-flags
         ids 'add "read" (format "marked %d message%s read"
                                 (length ids)
                                 (if (= (length ids) 1) "" "s"))
         :quiet quiet
         :on-failure
         (lambda (_result)
           (dolist (id ids) (remhash id zulip-feed--pending-read-ids))
           (when (equal target zulip-feed--last-read-target-id)
             (setq zulip-feed--last-read-target-id nil))))
        ids))))

(defun zulip-feed-mark-read (&optional position)
  "Mark unread messages through POSITION (or point) as read."
  (interactive)
  (or (zulip-feed--mark-read-through position nil nil)
      (message "Zulip: no unread message through point")))

(defun zulip-feed-mark-unread (&optional message)
  "Mark MESSAGE, defaulting to the message at point, unread."
  (interactive)
  (let* ((message (zulip-feed--server-message-required message))
         (id (zulip-feed--message-key message)))
    (puthash id t zulip-feed--auto-read-suppressed-ids)
    (remhash id zulip-feed--pending-read-ids)
    (zulip-feed--request-flags
     (list id) 'remove "read" "marked message unread"
     :on-failure
     (lambda (_result)
       (remhash id zulip-feed--auto-read-suppressed-ids)))))

(defun zulip-feed-toggle-star (&optional message)
  "Toggle the `starred' flag for MESSAGE at point."
  (interactive)
  (let* ((message (zulip-feed--server-message-required message))
         (id (zulip-feed--message-key message))
         (starred-p (zulip-feed--message-flag-p message "starred")))
    (zulip-feed--request-flags
     (list id) (if starred-p 'remove 'add) "starred"
     (if starred-p "removed star" "starred message"))))

(defun zulip-feed--reaction-record-by-name (message name)
  "Return MESSAGE's grouped reaction named NAME, preferring our own."
  (let ((records (zulip-feed--reaction-groups message)))
    (or (seq-find (lambda (record)
                    (and (equal (plist-get record :name) name)
                         (plist-get record :selected)))
                  records)
        (seq-find (lambda (record)
                    (equal (plist-get record :name) name))
                  records))))

(defun zulip-feed--default-reaction-record (message)
  "Return the best default grouped reaction for MESSAGE."
  (let ((records (zulip-feed--reaction-groups message)))
    (or (seq-find (lambda (record) (plist-get record :selected)) records)
        (car records))))

(defun zulip-feed--read-reaction-name (message)
  "Read one Zulip emoji name for MESSAGE."
  (let* ((default-record (zulip-feed--default-reaction-record message))
         (default (or (plist-get default-record :name) "thumbs_up"))
         (raw (read-string
               (format "Zulip emoji name (default %s): " default)
               nil nil default))
         (name (string-trim raw "[[:space:]:]+" "[[:space:]:]+")))
    (if (string-empty-p name)
        (user-error "Zulip emoji name cannot be empty")
      name)))

(defun zulip-feed-toggle-reaction (&optional reaction message-id)
  "Toggle current user's REACTION on MESSAGE-ID or the message at point.

REACTION is a grouped reaction plist produced by the row projector.  When it
is nil, prompt for a Zulip emoji name and infer whether it is already ours."
  (interactive)
  (let* ((message (zulip-feed--server-message-required message-id))
         (id (zulip-feed--message-key message))
         (name (or (and (listp reaction) (plist-get reaction :name))
                   (zulip-feed--read-reaction-name message)))
         (record (or reaction
                     (zulip-feed--reaction-record-by-name message name)))
         (selected-p (and record (plist-get record :selected)))
         (code (and record (plist-get record :code)))
         (type (and record (plist-get record :type)))
         (view (appkit-current-view))
         (description (if selected-p "removed reaction" "added reaction")))
    (unless (and (stringp name) (not (string-empty-p name)))
      (user-error "Zulip emoji name cannot be empty"))
    (if selected-p
        (zulip-api-remove-reaction
         zulip-feed--account id
         (lambda (result)
           (if (zulip-feed--result-ok-p result)
               (message "Zulip: removed :%s:" name)
             (zulip-feed--record-action-error view description result)))
         :emoji-name name :emoji-code code :reaction-type type :owner view)
      (zulip-api-add-reaction
       zulip-feed--account id name
       (lambda (result)
         (if (zulip-feed--result-ok-p result)
             (message "Zulip: added :%s:" name)
           (zulip-feed--record-action-error view description result)))
       :emoji-code code :reaction-type type :owner view))))

(defun zulip-feed--edit-fetched
    (view generation owner message-id result)
  "Settle OWNER's GENERATION raw fetch RESULT for MESSAGE-ID in exact VIEW."
  (appkit-with-live-view view
    (when (zulip-feed--edit-operation-current-p view generation owner)
      (let* ((data (and (zulip-feed--result-ok-p result)
                        (zulip-feed--result-data result)))
             (wire-message (and data (zulip-feed--field data 'message)))
             (raw (and data
                       (or (zulip-feed--field data 'raw-content)
                           (zulip-feed--field wire-message 'content))))
             (reason
              (cond
               ((not (zulip-feed--result-ok-p result))
                (zulip-feed--result-message result))
               ((not (stringp raw))
                "Zulip did not return raw Markdown for this message"))))
        (setq zulip-feed--edit-operation-owner nil)
        (if reason
            (progn
              ;; Restore only canonical state here.  The scheduled exact-view
              ;; sync below owns composer materialization and point.
              (zulip-feed--finish-edit-and-restore-draft)
              (setq zulip-feed--last-error reason)
              (zulip-feed--request-edit-sync
               view generation
               (lambda ()
                 (when (zulip-feed--composer-visible-p)
                   (zulip-feed-edit-draft))
                 (message "Zulip: fetch message source failed: %s" reason))))
          (zulip-feed--set-edit-state
           message-id nil nil nil
           :generation generation :operation-owner nil)
          (setq zulip-feed--last-error nil)
          (unless (eq appkit-markup-compose-active-codec 'markdown)
            (appkit-markup-compose-set-active-codec 'markdown))
          (appkit-chatbuf-input-state-set raw :reset-history-p t)
          (zulip-feed--request-edit-sync
           view generation
           (lambda ()
             (zulip-feed-edit-draft)
             (message "Zulip: editing message %s" message-id))))))))

(defun zulip-feed--edit-updated
    (view generation owner message-id content result)
  "Settle OWNER's GENERATION PATCH RESULT for MESSAGE-ID and CONTENT in VIEW."
  (appkit-with-live-view view
    (when (zulip-feed--edit-operation-current-p view generation owner)
      (setq zulip-feed--edit-operation-owner nil)
      (if (zulip-feed--result-ok-p result)
          (progn
            (setq zulip-feed--last-error nil)
            (appkit-chatbuf-input-history-push content)
            (zulip-feed--finish-edit-and-restore-draft)
            (zulip-feed--request-edit-sync
             view generation
             (lambda ()
               (when (zulip-feed--composer-visible-p)
                 (zulip-feed-edit-draft))
               (message "Zulip: edited message %s" message-id))))
        (let ((reason (zulip-feed--result-message result)))
          (zulip-feed--set-edit-state
           message-id nil nil nil
           :generation generation :operation-owner nil)
          (setq zulip-feed--last-error reason)
          (zulip-feed--request-edit-sync
           view generation
           (lambda ()
             (message "Zulip: edit message failed: %s" reason))))))))

(defun zulip-feed-edit-message (&optional message)
  "Fetch raw Markdown for MESSAGE and stage it in the Appkit composer."
  (interactive)
  (when (zulip-feed--edit-message-id)
    (user-error "Finish or cancel the current Zulip edit first"))
  (let* ((message (zulip-feed--server-message-required message))
         (id (zulip-feed--message-key message))
         (view (appkit-current-view))
         (generation (zulip-feed--advance-edit-generation))
         (owner (zulip-feed--new-edit-operation-owner
                 view generation id 'get)))
    (zulip-feed--set-edit-state
     id t message (copy-sequence (or (appkit-chatbuf-input-state) ""))
     :saved-codec appkit-markup-compose-active-codec
     :generation generation :operation-owner owner)
    ;; A just-finished or cancelled predecessor may have updated canonical
    ;; draft state without materializing it yet.  The new generation owns that
    ;; barrier as well as its working frame.
    (zulip-feed--request-edit-sync view generation)
    (zulip-api-get-message
     zulip-feed--account id
     (lambda (result)
       (zulip-feed--edit-fetched view generation owner id result))
     :apply-markdown nil :allow-empty-topic-name t :owner view)))

(defun zulip-feed-cancel-edit ()
  "Cancel the current staged message edit and clear its draft."
  (interactive)
  (unless (zulip-feed--edit-message-id)
    (user-error "No Zulip message edit is active"))
  (let ((view (appkit-current-view)))
    (setq zulip-feed--last-error nil)
    (zulip-feed--finish-edit-and-restore-draft)
    ;; Advancing after capturing/restoring the old session draft makes every
    ;; outstanding GET/PATCH and post-sync action from that session inert.
    (let ((generation (zulip-feed--advance-edit-generation)))
      (zulip-feed--request-edit-sync
       view generation
       (lambda ()
         (when (zulip-feed--composer-visible-p)
           (zulip-feed-edit-draft))
         (message "Zulip: edit cancelled"))))))

(defun zulip-feed-submit-edit (&optional prefix)
  "Submit the staged edit using the markup codec selected by PREFIX."
  (interactive "P")
  (unless (zulip-feed--edit-message-id)
    (user-error "No Zulip message edit is active"))
  (when (zulip-feed--edit-request-p)
    (user-error "A Zulip edit request is already active"))
  (when zulip-feed--edit-sync-request
    (user-error "The Zulip edit composer is still being materialized"))
  (let* ((state (zulip-feed--edit-state))
         (message-id (zulip-feed--edit-message-id))
         (snapshot (zulip-feed--compose-snapshot prefix))
         (content (plist-get snapshot :content))
         (view (appkit-current-view))
         (generation (plist-get state :generation)))
    (unless (eq generation zulip-feed--edit-generation)
      (user-error "This Zulip edit session is no longer current"))
    (when (string-empty-p (string-trim content))
      (user-error "Edited Zulip message is empty"))
    (let ((owner (zulip-feed--new-edit-operation-owner
                  view generation message-id 'patch)))
      (zulip-feed--set-edit-state
       message-id t nil nil
       :generation generation :operation-owner owner)
      (zulip-feed--request-action-frame view)
      (zulip-api-update-message
       zulip-feed--account message-id
       (lambda (result)
         (zulip-feed--edit-updated
          view generation owner message-id content result))
       :content content :owner view))))

(defun zulip-feed-delete-message (&optional message)
  "Permanently delete MESSAGE at point after confirmation."
  (interactive)
  (let* ((message (zulip-feed--server-message-required message))
         (id (zulip-feed--message-key message))
         (view (appkit-current-view)))
    (when (yes-or-no-p (format "Delete Zulip message %s permanently? " id))
      (zulip-api-delete-message
       zulip-feed--account id
       (lambda (result)
         (if (zulip-feed--result-ok-p result)
             (message "Zulip: deleted message %s" id)
           (zulip-feed--record-action-error view "delete message" result)))
       :owner view))))

(defun zulip-feed--wire-user-id (id)
  "Return normalized user ID suitable for a Zulip DM narrow."
  (cond ((integerp id) id)
        ((and (stringp id) (string-match-p "\\`[0-9]+\\'" id))
         (string-to-number id))
        (t id)))

(defun zulip-feed--message-direct-recipients (message)
  "Return direct-narrow recipient IDs for MESSAGE."
  (mapcar #'zulip-feed--wire-user-id
          (zulip-state--direct-narrow-ids
           (zulip-feed--account-state) message)))

(defun zulip-feed-open-message-context (&optional message)
  "Open the topic or direct conversation containing MESSAGE at point."
  (interactive)
  (let ((message (or message
                     (zulip-feed-message-at-point)
                     (user-error "No Zulip message at point"))))
    (pcase (zulip-feed--message-kind message)
      ('channel
       (zulip-feed-open
        zulip-feed--account
        (zulip-narrow-topic
         (or (zulip-feed--message-channel-id message)
             (zulip-feed--channel-name message))
         (or (zulip-feed--message-topic message) ""))))
      ('direct
       (zulip-feed-open
        zulip-feed--account
        (let ((recipients
               (or (zulip-feed--message-direct-recipients message)
                   (user-error
                    "Direct-message participants are unavailable"))))
          (zulip-narrow-direct
           recipients (zulip-feed--message-direct-title message)))))
      (_ (user-error "Message has no openable Zulip context")))))

(defun zulip-feed--subscription-choices ()
  "Return completion choices for the current account's subscriptions."
  (let (choices)
    (when (zulip-state-p (zulip-feed--account-state))
      (maphash
       (lambda (id channel)
         (let ((name (format "%s" (or (zulip-feed--field channel 'name) id))))
           (push (cons (format "#%s" name) id) choices)))
       (zulip-state-subscriptions (zulip-feed--account-state))))
    (sort choices (lambda (left right)
                    (string-lessp (downcase (car left))
                                  (downcase (car right)))))))

(defun zulip-feed-open-topic (&optional channel topic)
  "Open CHANNEL and TOPIC, defaulting from the message at point."
  (interactive)
  (let* ((message (zulip-feed-message-at-point))
         (channel
          (or channel
              (and message (zulip-feed--message-channel-id message))
              (and (memq (zulip-narrow-kind zulip-feed--narrow)
                         '(channel topic))
                   (zulip-narrow-channel-operand zulip-feed--narrow))
              (let* ((choices (zulip-feed--subscription-choices))
                     (choice (completing-read "Channel: " choices nil t)))
                (cdr (assoc choice choices)))))
         (default-topic
          (or topic
              (and message (zulip-feed--message-topic message))
              (and (eq (zulip-narrow-kind zulip-feed--narrow) 'topic)
                   (zulip-narrow-topic-name zulip-feed--narrow))))
         (topic (or topic
                    (read-string "Topic: " default-topic))))
    (unless channel (user-error "No Zulip channel selected"))
    (zulip-feed-open
     zulip-feed--account (zulip-narrow-topic channel topic))))

(defun zulip-feed-copy-message (&optional message)
  "Copy plain text for MESSAGE at point."
  (interactive)
  (let* ((message (or message
                      (zulip-feed-message-at-point)
                      (user-error "No Zulip message at point")))
         (local (zulip-feed--field message 'local-content))
         (content (or local
                      (zulip-feed--field message 'rendered-content)
                      (zulip-feed--field message 'content)
                      ""))
         (plain
          (if local
              (format "%s" content)
            (zulip-markup-plain-text
             (format "%s" content)
             (and (zulip-account-p zulip-feed--account)
                  (zulip-account-server zulip-feed--account))
             t))))
    (kill-new plain)
    (message "Copied Zulip message")
    plain))

(defun zulip-feed-next-message (&optional n)
  "Move point to the next message, N times."
  (interactive "p")
  (let* ((keys (appkit-chat-timeline-keys))
         (current (zulip-feed-message-id-at-point))
         (index (and current (seq-position keys current #'equal)))
         (target-index (if index
                           (min (1- (length keys)) (+ index (or n 1)))
                         0))
         (target (nth target-index keys)))
    (unless target (user-error "No next Zulip message"))
    (goto-char (or (appkit-chat-timeline-key-position target) (point)))
    target))

(defun zulip-feed-previous-message (&optional n)
  "Move point to the previous message, N times."
  (interactive "p")
  (let* ((keys (appkit-chat-timeline-keys))
         (current (zulip-feed-message-id-at-point))
         (index (and current (seq-position keys current #'equal)))
         (target-index (if index
                           (max 0 (- index (or n 1)))
                         (1- (length keys))))
         (target (and (>= target-index 0) (nth target-index keys))))
    (unless target (user-error "No previous Zulip message"))
    (goto-char (or (appkit-chat-timeline-key-position target) (point)))
    target))

(defun zulip-feed-edit-draft ()
  "Move point to the editable Zulip composer."
  (interactive)
  (unless (zulip-feed--composer-visible-p)
    (user-error "This feed has no writable composer"))
  (goto-char (or (appkit-chatbuf-input-logical-end-position) (point-max))))

(defun zulip-feed-draft-previous (&optional n)
  "Recall the Nth previous composer input."
  (interactive "p")
  (zulip-feed--assert-edit-composer-mutable)
  (condition-case nil
      (progn
        (appkit-chatbuf-input-history-prev n)
        (appkit-chatbuf-input-state-sync)
        (zulip-feed-edit-draft))
    (user-error (user-error "No previous Zulip input"))))

(defun zulip-feed-draft-next (&optional n)
  "Move N steps toward newer composer input."
  (interactive "p")
  (zulip-feed--assert-edit-composer-mutable)
  (condition-case nil
      (progn
        (appkit-chatbuf-input-history-next n)
        (appkit-chatbuf-input-state-sync)
        (zulip-feed-edit-draft))
    (user-error (user-error "Not browsing Zulip input history"))))


(defun zulip-feed-return-dwim (argument)
  "Open context, complete/send the draft, or insert newline with ARGUMENT."
  (interactive "P")
  (if (appkit-chatbuf-point-in-input-p)
      (cond
       (argument (insert "\n"))
       ((zulip-completion-token-at-point)
        (or (zulip-completion-complete)
            (message "No completion candidate; C-c RET sends literally")))
       (t (zulip-feed-send-message)))
    (zulip-feed-open-message-context)))

(defun zulip-feed--maybe-auto-load-older (&optional position)
  "Load older history when POSITION approaches the timeline start."
  (when (and (not (appkit-chatbuf-point-in-input-p))
             (appkit-chat-history-autoload-older-p
              (or position (point)) (point-min)
              zulip-history-auto-load-threshold))
    (zulip-feed-load-older)))

(defun zulip-feed--maybe-auto-load-newer (&optional position)
  "Load newer history when POSITION approaches a partial-window footer."
  (let ((position (or position (point)))
        (footer (or (appkit-chat-timeline-footer-start-position)
                    (appkit-chatbuf-input-start-position)
                    (point-max))))
    (when (appkit-chat-history-autoload-newer-p
           position footer zulip-history-auto-load-threshold
           (appkit-chatbuf-composer-idle-p))
      (zulip-feed-load-newer))))

(defun zulip-feed--manage-read-position (&optional position)
  "Advance read state through the observed message at POSITION."
  (when (and zulip-auto-mark-read
             (not (zulip-feed--edit-request-p))
             (appkit-chat-history-window-known-p))
    (zulip-feed--mark-read-through position t t)))

(defun zulip-feed--install-scroll-observer (view)
  "Install VIEW's lifecycle-owned history edge observer."
  (unless (and (appkit-scroll-observer-p zulip-feed--scroll-observer)
               (appkit-scroll-observer-active-p
                zulip-feed--scroll-observer)
               (eq view
                   (appkit-scroll-observer-owner
                    zulip-feed--scroll-observer)))
    (when (appkit-scroll-observer-p zulip-feed--scroll-observer)
      (appkit-scroll-observer-cancel zulip-feed--scroll-observer))
    (setq-local
     zulip-feed--scroll-observer
     (appkit-scroll-observer-install
      view
      :end-boundary-function #'appkit-chat-timeline-footer-start-position
      :start-function
      (lambda (_window position _start)
        (zulip-feed--maybe-auto-load-older position))
      :end-function
      (lambda (window position _end)
        ;; Only the selected window is known to have been deliberately observed.
        ;; Background windows retain unread state, matching Telega's semantics.
        (when (eq window (selected-window))
          (zulip-feed--manage-read-position position))
        (zulip-feed--maybe-auto-load-newer position))))))

(defun zulip-feed--post-command ()
  "Maintain Zulip read state after each command."
  (unless (appkit-chatbuf-rendering-p)
    (zulip-feed--manage-read-position)))

(defvar-keymap zulip-feed-message-map
  :doc "Single-key command map active over the generated timeline."
  "q" #'quit-window
  "RET" #'zulip-feed-open-message-context
  "o" #'zulip-feed-open-message-context
  "t" #'zulip-feed-open-topic
  "c" #'zulip-feed-copy-message
  "n" #'zulip-feed-next-message
  "p" #'zulip-feed-previous-message
  "r" #'zulip-feed-mark-read
  "u" #'zulip-feed-mark-unread
  "s" #'zulip-feed-toggle-star
  "R" #'zulip-feed-retry-send
  "e" #'zulip-feed-edit-message
  "d" #'zulip-feed-delete-message
  "!" #'zulip-feed-toggle-reaction
  "?" #'zulip-message-transient)

(define-minor-mode zulip-feed-timeline-mode
  "Use point-local message commands outside the Zulip composer."
  :init-value nil
  :lighter nil
  :keymap zulip-feed-message-map)

(defun zulip-feed-select-compose-codec ()
  "Select the visible active source codec for this composer."
  (interactive)
  (call-interactively #'appkit-markup-compose-set-active-codec)
  (force-mode-line-update)
  (message "Zulip compose codec: %s"
           (appkit-markup-compose-codec-label)))

(defvar-keymap zulip-feed-mode-map
  :doc "Keymap for `zulip-feed-mode'."
  :parent appkit-chatbuf-mode-map
  "RET" #'zulip-feed-return-dwim
  "TAB" #'zulip-completion-complete
  "<tab>" #'zulip-completion-complete
  "C-M-i" #'zulip-completion-complete
  "M-p" #'zulip-feed-draft-previous
  "M-n" #'zulip-feed-draft-next
  "C-c '" #'zulip-feed-edit-draft
  "C-c C-k" #'zulip-feed-cancel-edit
  "C-c C-v" #'zulip-feed-preview-message
  "C-c C-m" #'zulip-feed-select-compose-codec
  "C-c C-a" #'zulip-message-transient
  "C-c C-t" #'zulip-feed-open-topic
  "C-c RET" #'zulip-feed-send-message
  "C-c C-c" #'zulip-feed-send-message
  "M-g p" #'zulip-feed-load-older
  "M-g n" #'zulip-feed-load-newer
  "C-c C-l" #'zulip-feed-load-latest)

(defun zulip-feed--reset-view-local-state ()
  "Reset controller state owned by one concrete feed view.

Appkit can attach a replacement view to an existing same-named buffer without
re-running its major mode.  Keep all view-local ownership resettable from that
path while leaving account-owned optimistic sends in their shared table."
  (appkit-chatbuf-reset-state)
  (appkit-chat-history-reset-state)
  (setq-local zulip-feed--pending nil)
  (setq-local zulip-feed--last-error nil)
  (setq-local zulip-feed--latest-live-keys nil)
  (setq-local zulip-feed--history-reload-needed-p nil)
  (setq-local zulip-feed--fill-column nil)
  (setq-local zulip-feed--pending-jump-id nil)
  (setq-local zulip-feed--pending-read-ids (make-hash-table :test #'equal))
  (setq-local zulip-feed--auto-read-suppressed-ids
              (make-hash-table :test #'equal))
  (setq-local zulip-feed--last-read-target-id nil)
  (setq-local zulip-feed--edit-generation
              (list 'zulip-feed-edit-generation))
  (setq-local zulip-feed--edit-operation-owner nil)
  (setq-local zulip-feed--edit-sync-request nil)
  (setq-local zulip-feed--scroll-observer nil)
  (when (bound-and-true-p appkit-compose-session-mode)
    (appkit-compose-reset))
  (setq-local buffer-read-only nil)
  (setq-local zulip-feed-timeline-mode nil))

(define-derived-mode zulip-feed-mode appkit-chatbuf-mode "Zulip-Feed"
  "Major mode for one Appkit-backed Zulip narrow."
  (setq-local line-spacing 0)
  (zulip-feed--reset-view-local-state)
  (setq-local appkit-chatbuf-input-sync-function
              #'appkit-chatbuf-input-state-sync)
  (setq-local buffer-read-only nil)
  (setq-local truncate-lines nil)
  (setq-local header-line-format '(:eval (zulip-feed--header-line)))
  (appkit-compose-setup
   :snapshot-function #'appkit-chatbuf-input-state
   :source-bounds-function #'appkit-chatbuf-input-region-bounds)
  (appkit-markup-compose-setup
   :codecs zulip-compose-codecs
   :active-codec (car zulip-compose-codecs)
   :object-printer #'zulip-completion-markup-object-printer)
  (appkit-chatbuf-use-timeline-mode #'zulip-feed-timeline-mode)
  (add-hook 'post-command-hook #'zulip-feed--post-command t t))

(defun zulip-feed--view-id (account narrow)
  "Return server-qualified Appkit view identity for ACCOUNT and NARROW."
  (list 'zulip-feed
        (zulip-account-id account)
        (zulip-narrow-key narrow)))

(defun zulip-feed--buffer-name (account narrow)
  "Return a server-qualified buffer name for ACCOUNT and NARROW."
  (format "*Zulip %s <%s> · %s*"
          (zulip-account-server account)
          (zulip-account-email account)
          (zulip-narrow-title narrow)))

(defun zulip-feed--open-buffer (account narrow)
  "Open or reuse ACCOUNT's feed for NARROW and return its buffer."
  (unless (and (zulip-account-p account)
               (appkit-app-live-p (zulip-account-app account)))
    (error "Zulip feed requires a live account"))
  (unless (zulip-narrow-p narrow)
    (error "Invalid Zulip feed narrow: %S" narrow))
  (zulip-feed--ensure-account-subscriptions account)
  (let* ((app (zulip-account-app account))
         (view
          (appkit-open-view
           :app app
           :id (zulip-feed--view-id account narrow)
           :mode 'zulip-feed-mode
           :buffer-name (zulip-feed--buffer-name account narrow)
           :state narrow
           :sync-function #'zulip-feed--sync-invalidations
           :parts '(frame timeline composer history geometry)
           :setup
           (lambda (new-view)
             (appkit-view-enable-responsive-geometry new-view)
             ;; `appkit-open-view' initializes a major mode only once per
             ;; buffer, but SETUP runs for every newly attached view.  A dead
             ;; predecessor must not lend its history owner, edit request,
             ;; pending jump, or read frontier to this replacement.
             (zulip-feed--reset-view-local-state)
             (setq-local zulip-feed--account account
                         zulip-feed--narrow narrow)
             (zulip-feed--bind-account-tables account)
             (zulip-completion-setup account)
             (zulip-feed--install-scroll-observer new-view)
             (appkit-chat-history-window-clear)
             (zulip-feed-render)
             (appkit-chatbuf-update-context-mode)))))
    (with-current-buffer (appkit-view-buffer view)
      (setq-local zulip-feed--account account
                  zulip-feed--narrow narrow)
      (zulip-feed--bind-account-tables account)
      (zulip-completion-setup account)
      (zulip-feed--install-scroll-observer view)
      (unless (appkit-chat-timeline-live-p)
        (zulip-feed-render)))
    (appkit-view-buffer view)))

(defun zulip-feed-open (account narrow)
  "Open ACCOUNT's server-qualified feed buffer for NARROW."
  (let ((buffer (zulip-feed--open-buffer account narrow)))
    (with-current-buffer buffer
      (unless (or (not (zulip-account-connected-p account))
                  (appkit-chat-history-window-known-p)
                  (appkit-chat-history-loading-p))
        (zulip-feed-load-initial)))
    (pop-to-buffer buffer)
    (appkit-view-refresh-responsive-geometry)
    buffer))

(defun zulip-feed-open-message (account narrow message-id)
  "Open ACCOUNT NARROW around exact opaque MESSAGE-ID."
  (unless (and (stringp message-id) (not (string-empty-p message-id)))
    (error "Zulip message ID must be a non-empty string"))
  (let ((buffer (zulip-feed--open-buffer account narrow))
        (before (/ zulip-history-page-size 2)))
    (with-current-buffer buffer
      (unless (zulip-account-connected-p account)
        (user-error "Zulip account is not connected"))
      (appkit-chat-history-request-cancel)
      (appkit-chat-history-window-clear)
      (setq-local zulip-feed--pending-jump-id message-id)
      (zulip-feed--load-history
       'around message-id before
       (max 0 (- zulip-history-page-size before 1))))
    (pop-to-buffer buffer)
    (appkit-view-refresh-responsive-geometry)
    buffer))

(provide 'zulip-feed)

;;; zulip-feed.el ends here
