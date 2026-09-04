;;; zulip-runtime.el --- Multi-account Appkit runtime for Zulip -*- lexical-binding: t; -*-

;;; Commentary:

;; A Zulip realm/account pair owns one Appkit application session.  Protocol
;; state remains Zulip-owned; Appkit owns views, requests, timers, and cleanup.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'appkit-core)
(require 'appkit-app)
(require 'appkit-surface)
(require 'appkit-projection)
(require 'appkit-effect)
(require 'appkit-source)
(require 'zulip-customize)
(require 'zulip-state)

(cl-defstruct (zulip-account
               (:constructor zulip-account--create))
  "The authoritative model of one authenticated Zulip App."
  id server email api-key app state feature-level server-version
  queue-id last-event-id longpoll-timeout poll-process retry-timer retry-handle
  generation connected-p events-enabled-p event-transport address
  (pending (make-hash-table :test #'equal))
  (topics (make-hash-table :test #'equal))
  (topic-errors (make-hash-table :test #'equal))
  (decoded-images (make-hash-table :test #'equal)))

(defvar zulip-runtime--accounts (make-hash-table :test #'equal)
  "Live Zulip accounts keyed by normalized account ID.")

(defvar zulip-runtime-change-hook nil
  "Hook run after canonical Zulip runtime state or account membership changes.

Each function receives ACCOUNT and a REASON symbol: `state', `added', or
`removed'.  Presentation features such as the global mode-line indicator use
this hook without becoming part of protocol reducers.")

(defun zulip-runtime-normalize-server (server)
  "Return normalized base URL for Zulip SERVER."
  (unless (and (stringp server) (not (string-empty-p (string-trim server))))
    (error "Zulip server URL must not be empty"))
  (let ((server (replace-regexp-in-string "/+\\'" "" (string-trim server)))
        (case-fold-search t))
    (unless (string-match-p "\\`https?://" server)
      (setq server (concat "https://" server)))
    (when (string-match
           "\\`\\(https?\\)://\\([^/]+\\)\\(.*\\)\\'" server)
      (setq server
            (concat (downcase (match-string 1 server))
                    "://"
                    (downcase (match-string 2 server))
                    (match-string 3 server))))
    server))

(defun zulip-runtime-account-id (server email)
  "Return stable account identity for SERVER and EMAIL."
  (list (zulip-runtime-normalize-server server)
        (downcase (string-trim email))))

(defun zulip-runtime-account (server email)
  "Return the live account for SERVER and EMAIL, or nil."
  (let ((account (gethash (zulip-runtime-account-id server email)
                          zulip-runtime--accounts)))
    (and (zulip-account-p account)
         (appkit-app-live-p (zulip-account-app account))
         account)))

(defun zulip-runtime-accounts ()
  "Return all live Zulip accounts."
  (let (accounts)
    (maphash
     (lambda (_id account)
       (when (and (zulip-account-p account)
                  (appkit-app-live-p (zulip-account-app account)))
         (push account accounts)))
     zulip-runtime--accounts)
    (nreverse accounts)))

(defun zulip-runtime-publish-state (account state)
  "Publish STATE only through ACCOUNT's authoritative App transition."
  (unless (zulip-account-p account) (error "Not a Zulip account: %S" account))
  (cond
   ((zulip-runtime--account-transition-p account)
    (setf (zulip-account-state account) state)
    (run-hook-with-args 'zulip-runtime-change-hook account 'state))
   (zulip-runtime--transition-context
    (zulip-runtime--post-account account (list 'state state)))
   (t (appkit-app-send (zulip-account-app account) (list 'state state))))
  state)

(defun zulip-runtime--replace-api-key (account api-key)
  "Give ACCOUNT an owned mutable copy of API-KEY.

Any previous account-owned copy is erased before it is released."
  (unless (and (stringp api-key)
               (not (string-empty-p api-key))
               (not (string-match-p "[\r\n]" api-key)))
    (error "Zulip API key must be nonempty and single-line"))
  (let ((replacement (copy-sequence api-key))
        (previous (zulip-account-api-key account)))
    (when (stringp previous)
      (clear-string previous))
    (setf (zulip-account-api-key account) replacement)
    replacement))

(defun zulip-runtime--clear-api-key (account)
  "Erase and release ACCOUNT's owned API key."
  (when-let* ((api-key (zulip-account-api-key account)))
    (when (stringp api-key)
      (clear-string api-key))
    (setf (zulip-account-api-key account) nil)))

(defun zulip-runtime--shutdown (app)
  "Erase APP's credentials and remove only its exact registry membership."
  (let ((account (appkit-app-model app)))
    (when (zulip-account-p account)
      (cl-incf (zulip-account-generation account))
      (setf (zulip-account-events-enabled-p account) nil
            (zulip-account-connected-p account) nil
            (zulip-account-event-transport account) nil)
      (zulip-runtime--clear-api-key account)
      (when (eq app (zulip-account-app account))
        (setf (zulip-account-app account) nil))
      (zulip-runtime--forget-account account)
      (run-hook-with-args 'zulip-runtime-change-hook account 'removed))))

(cl-defun zulip-runtime-create-account (&key server email api-key state)
  "Create or refresh one normalized realm/email App without connecting it."
  (unless (and (stringp email) (not (string-empty-p (string-trim email))))
    (error "Zulip email must not be empty"))
  (unless (and (stringp api-key) (not (string-empty-p api-key))
               (not (string-match-p "[\r\n]" api-key)))
    (error "Zulip API key must be nonempty and single-line"))
  (let* ((server (zulip-runtime-normalize-server server))
         (email (string-trim email))
         (id (zulip-runtime-account-id server email))
         (account (gethash id zulip-runtime--accounts)))
    (if (and (zulip-account-p account)
             (appkit-app-live-p (zulip-account-app account)))
        (progn
          (appkit-app-send (zulip-account-app account)
                           (list 'credentials api-key))
          account)
      (setq account
            (zulip-account--create
             :id id :server server :email email :state state
             :longpoll-timeout zulip-event-long-poll-timeout :generation 0))
      (zulip-runtime--replace-api-key account api-key)
      (let (app complete)
        (unwind-protect
            (progn
              (setq app (appkit-app-start zulip-runtime--app-type
                                          :identity id :input account))
              (setf (zulip-account-app account) app)
              (puthash id account zulip-runtime--accounts)
              (run-hook-with-args 'zulip-runtime-change-hook account 'added)
              (setq complete t)
              account)
          (unless complete
            (unwind-protect
                (when (appkit-app-p app) (appkit-app-close app))
              (zulip-runtime--clear-api-key account)
              (zulip-runtime--forget-account account))))))))

(defun zulip-runtime-stop-account (account)
  "Revoke ACCOUNT and its Sources, Effects, and Surfaces, preserving buffers."
  (when (zulip-account-p account)
    (if (appkit-app-p (zulip-account-app account))
        (appkit-app-close (zulip-account-app account))
      (zulip-runtime--clear-api-key account)
      (zulip-runtime--forget-account account))
    t))

(defun zulip-runtime-stop-all ()
  "Stop every live Zulip account."
  (dolist (account (zulip-runtime-accounts))
    (zulip-runtime-stop-account account)))

(defvar zulip-runtime--transition-context nil
  "Current transition collecting closed commands.")

(defvar zulip-runtime--commands nil
  "Reverse ordered commands belonging to the current transition.")

(defvar-local zulip-runtime--surface-address nil
  "Exact routing address of this generated Surface incarnation.")

(defun zulip-runtime--forget-account (account)
  "Remove ACCOUNT only if it still owns the normalized registry key."
  (when (eq account (gethash (zulip-account-id account)
                             zulip-runtime--accounts))
    (remhash (zulip-account-id account) zulip-runtime--accounts)))

(defun zulip-runtime--post-surface (surface message)
  "Send MESSAGE to an exact live SURFACE after the current commit."
  (when (appkit-surface-live-p surface)
    (if zulip-runtime--transition-context
        (push (appkit-command-post-message
               :target (buffer-local-value 'zulip-runtime--surface-address
                                           (appkit-surface-buffer surface))
               :message message :delivery 'report)
              zulip-runtime--commands)
      (appkit-surface-post surface message))))

(defun zulip-runtime--post-account (account message)
  "Deliver MESSAGE to ACCOUNT without reentering an active Appkit loop."
  (if zulip-runtime--transition-context
      (push (appkit-command-post-message
             :target (zulip-account-address account)
             :message message :delivery 'report)
            zulip-runtime--commands)
    (appkit-app-post (zulip-account-app account) message)))

(defun zulip-runtime--post-owner (owner message)
  "Route a request MESSAGE to its concrete App or Surface OWNER."
  (if (appkit-surface-p owner)
      (zulip-runtime--post-surface owner message)
    (zulip-runtime--post-account (appkit-app-model owner) message)))

(defun zulip-runtime--fanout (account event old-state state)
  "Project ACCOUNT's committed EVENT only into dependent Surfaces."
  (when (fboundp 'zulip-feed--consume-state-change)
    (zulip-feed--consume-state-change account event old-state state))
  (when (fboundp 'zulip-root--queue-refresh)
    (zulip-root--queue-refresh account event)))

(defun zulip-runtime--account-update (context account message)
  "Commit ACCOUNT state and return only closed lifecycle commands."
  (let ((zulip-runtime--transition-context context)
        zulip-runtime--commands)
    (pcase message
      (`(state ,state)
       (let ((old (zulip-account-state account)))
         (setf (zulip-account-state account) state)
         (run-hook-with-args 'zulip-runtime-change-hook account 'state)
         (zulip-runtime--fanout account '((type . state)) old state)))
      (`(credentials ,key)
       (zulip-runtime--replace-api-key account key))
      (`(domain ,event ,state)
       (zulip-runtime--apply-domain account event state))
      (`(history ,messages ,key)
       (let* ((old (zulip-account-state account))
              (state (zulip-state-merge-messages old messages key)))
         (zulip-runtime-publish-state account state)
         (zulip-runtime--fanout
          account (list (cons 'type "history")
                        (cons 'narrow-key key)
                        (cons 'message_ids
                              (mapcar (lambda (entry) (zulip-state-object-get entry 'id))
                                      messages))) old state)))
      (`(cancel-effect ,key)
       (push (appkit-command-cancel-effect key) zulip-runtime--commands))
      (`(start-effect ,effect)
       (push (appkit-command-start-effect effect) zulip-runtime--commands))
      (`(response ,callback ,result)
       (funcall callback result))
      (_ (when (fboundp 'zulip-events--update)
           (zulip-events--update account message))))
    (appkit-next :model account :render appkit-render-none
                 :commands (nreverse zulip-runtime--commands))))

(defun zulip-runtime--sources (account)
  "Declare ACCOUNT's explicitly enabled event transport."
  (when (and (zulip-account-events-enabled-p account)
             (fboundp 'zulip-events--source))
    (list (zulip-events--source account))))

(defconst zulip-runtime--app-type
  (appkit-app-type-create
   :name 'zulip
   :init (lambda (context account)
           (setf (zulip-account-address account)
                 (appkit-transition-context-owner-address context))
           (appkit-next :model account :render appkit-render-none))
   :update #'zulip-runtime--account-update
   :sources #'zulip-runtime--sources
   :shutdown #'zulip-runtime--shutdown))

(defun zulip-runtime--account-transition-p (account)
  "Test whether the current transition belongs to this exact ACCOUNT App."
  (and zulip-runtime--transition-context
       (equal (appkit-transition-context-owner-address zulip-runtime--transition-context)
              (zulip-account-address account))))

(defun zulip-runtime--apply-domain (account event state)
  "Apply EVENT atomically within ACCOUNT's current App transition."
  (let* ((old (zulip-account-state account))
         (entry (zulip-state-object-get event 'message))
         (state (if entry
                    (zulip-state-upsert-message
                     old entry
                     (when-let* ((key (zulip-state-object-get entry 'narrow-key)))
                       (list key)))
                  state)))
    (when (and (equal (format "%s" (zulip-state-object-get event 'type)) "register")
               (fboundp 'zulip-feed--rebase-pending))
      (setq state (zulip-feed--rebase-pending account state)))
    (setf (zulip-account-state account) state)
    (run-hook-with-args 'zulip-runtime-change-hook account 'state)
    (zulip-runtime--fanout account event old state)))

(provide 'zulip-runtime)

;;; zulip-runtime.el ends here
