;;; zulip-root.el --- Account-scoped Zulip navigator -*- lexical-binding: t; -*-

;;; Commentary:

;; Persistent account home backed by one Appkit view.  Canonical Zulip state
;; is projected into stable-keyed EWOC rows for the combined feed, subscribed
;; channels and their locally-known topics, and recent direct conversations.
;; Registration and queue events reconcile the projection in place, preserving
;; the user's semantic row and viewport.

;;; Code:

(require 'button)
(require 'cl-lib)
(require 'ewoc)
(require 'seq)
(require 'subr-x)
(require 'appkit-core)
(require 'appkit-ewoc)
(require 'appkit-invalidation)
(require 'appkit-position)
(require 'appkit-task-queue)
(require 'appkit-transaction)
(require 'appkit-ui)
(require 'appkit-view)
(require 'zulip-api)
(require 'zulip-customize)
(require 'zulip-feed)
(require 'zulip-http)
(require 'zulip-narrow)
(require 'zulip-render)
(require 'zulip-runtime)
(require 'zulip-state)

(defface zulip-root-unread-face
  '((t :inherit font-lock-warning-face :weight bold))
  "Face used for unread counts in the Zulip navigator."
  :group 'zulip)

(defface zulip-root-mention-face
  '((t :inherit error :weight bold))
  "Face used for unread mention counts in the Zulip navigator."
  :group 'zulip)

(defface zulip-root-muted-face
  '((t :inherit shadow :slant italic))
  "Face used for muted channel and topic markers."
  :group 'zulip)

(defconst zulip-root--icon-slot-width 4
  "Reserved icon width for one navigator activity row.")

(defconst zulip-root--event-subscriptions-key
  '(zulip-root event-subscriptions)
  "Resource-store key for one account's root event subscriptions.")

(defconst zulip-root--topic-cache-key
  '(zulip-root topic-cache)
  "App resource-store key for account-scoped channel topic metadata.")

(defconst zulip-root--topic-errors-key
  '(zulip-root topic-errors)
  "App resource-store key for account-scoped topic hydration errors.")

(defconst zulip-root--anchor-property 'zulip-root-entry-key
  "Text property carrying a root row's stable projection key.")

(defvar-local zulip-root--account nil
  "Zulip account owning the current navigator buffer.")

(defvar-local zulip-root--ewoc nil
  "Persistent EWOC containing the current navigator projection.")

(defvar-local zulip-root--node-table nil
  "Stable entry-key to EWOC-node table for the current navigator.")

(defvar-local zulip-root--fill-column nil
  "Last usable row width measured from a window displaying this root.")

(defvar-local zulip-root--topic-tasks nil
  "Appkit task queue owning bounded topic hydration for this root view.")

(cl-defstruct (zulip-root--entry
               (:constructor zulip-root--entry-create))
  key
  type
  title
  preview
  time
  target
  completion
  unread-count
  mention-count
  muted-p
  indent
  width)

(defun zulip-root--field (object key)
  "Return KEY from normalized Zulip OBJECT."
  (and object (zulip-state-object-get object key)))

(defun zulip-root--true-p (value)
  "Return non-nil when JSON-like VALUE denotes true."
  (not (memq value '(nil :false :json-false json-false false))))

(defun zulip-root--state (&optional account)
  "Return ACCOUNT's canonical state, or an empty state before registration."
  (let ((state (and (or account zulip-root--account)
                    (zulip-account-state (or account zulip-root--account)))))
    (if (zulip-state-p state) state (zulip-state-create))))

(defun zulip-root--resource-table (key &optional account)
  "Return account-scoped Appkit resource table stored under KEY.

ACCOUNT defaults to the current root account."
  (let* ((account (or account zulip-root--account))
         (app (and (zulip-account-p account) (zulip-account-app account))))
    (unless (appkit-app-live-p app)
      (error "Zulip root resources require a live account"))
    (let* ((store (appkit-app-resource-store app))
           (table (gethash key store)))
      (unless (hash-table-p table)
        (setq table (make-hash-table :test #'equal))
        (puthash key table store))
      table)))

(defun zulip-root--topic-cache (&optional account)
  "Return ACCOUNT's channel topic metadata cache."
  (zulip-root--resource-table zulip-root--topic-cache-key account))

(defun zulip-root--topic-errors (&optional account)
  "Return ACCOUNT's latest per-channel topic hydration errors."
  (zulip-root--resource-table zulip-root--topic-errors-key account))

(defun zulip-root--table-values (table)
  "Return all values stored in hash TABLE."
  (let (values)
    (when (hash-table-p table)
      (maphash (lambda (_key value) (push value values)) table))
    (nreverse values)))

(defun zulip-root--id-string (id)
  "Return opaque string form of protocol ID, or nil."
  (and id (zulip-state-normalize-id id)))

(defun zulip-root--wire-integer (id description)
  "Return numeric wire form of decimal ID for DESCRIPTION.

Zulip user and channel identifiers are ordinary JSON integers.  Message IDs
never pass through this helper and remain opaque strings everywhere."
  (cond
   ((and (integerp id) (> id 0)) id)
   ((and (stringp id) (string-match-p "\\`[0-9]+\\'" id))
    (let ((value (string-to-number id)))
      (if (> value 0)
          value
        (error "Invalid %s: %S" description id))))
   (t (error "Invalid %s: %S" description id))))

(defun zulip-root--decimal-id< (left right)
  "Compare opaque decimal strings LEFT and RIGHT without numeric conversion."
  (let ((left (and left (replace-regexp-in-string "\\`0+" "" left)))
        (right (and right (replace-regexp-in-string "\\`0+" "" right))))
    (setq left (if (string-empty-p (or left "")) "0" left)
          right (if (string-empty-p (or right "")) "0" right))
    (or (< (length left) (length right))
        (and (= (length left) (length right))
             (string-lessp left right)))))

(defun zulip-root--decimal-id-p (id)
  "Return non-nil when ID is an opaque decimal string."
  (and (stringp id) (string-match-p "\\`[0-9]+\\'" id)))

(defun zulip-root--message-kind (message)
  "Return canonical `channel' or `direct' kind for MESSAGE."
  (let ((kind (zulip-root--field message 'kind))
        (type (downcase (format "%s" (or (zulip-root--field message 'type)
                                         "")))))
    (cond
     ((memq kind '(channel direct)) kind)
     ((member type '("stream" "channel")) 'channel)
     ((member type '("private" "direct" "dm")) 'direct))))

(defun zulip-root--message-channel-id (message)
  "Return MESSAGE's normalized channel ID, or nil."
  (zulip-root--id-string
   (or (zulip-root--field message 'channel-id)
       (zulip-root--field message 'stream-id))))

(defun zulip-root--message-topic (message)
  "Return MESSAGE's topic string, or nil for a direct message."
  (when (eq (zulip-root--message-kind message) 'channel)
    (let ((topic (or (zulip-root--field message 'topic)
                     (zulip-root--field message 'subject))))
      (and (stringp topic) topic))))

(defun zulip-root--message-newer-p (candidate current)
  "Return non-nil when CANDIDATE is later than CURRENT."
  (let ((candidate-time (or (zulip-root--field candidate 'timestamp) 0))
        (current-time (or (zulip-root--field current 'timestamp) 0)))
    (or (null current)
        (> candidate-time current-time)
        (and (= candidate-time current-time)
             (let ((candidate-id (zulip-root--id-string
                                  (zulip-root--field candidate 'id)))
                   (current-id (zulip-root--id-string
                                (zulip-root--field current 'id))))
               (and candidate-id current-id
                    (zulip-root--decimal-id< current-id candidate-id)))))))

(defun zulip-root--message-preview (message)
  "Return compact one-line preview text for MESSAGE."
  (if (null message)
      ""
    (let* ((sender (or (zulip-root--field message 'sender-full-name)
                       (zulip-root--field message 'sender-email)
                       (zulip-root--field message 'sender-id)))
           (content (format "%s"
                            (or (zulip-root--field message 'local-content)
                                (zulip-root--field message 'raw-content)
                                (zulip-root--field message 'content)
                                "")))
           (content (zulip-render-plain-text content))
           (content (string-trim
                     (replace-regexp-in-string "[\t\n\r ]+" " " content))))
      (cond
       ((and sender (not (string-empty-p content)))
        (format "%s: %s" sender content))
       ((not (string-empty-p content)) content)
       (sender (format "%s" sender))
       (t "Message")))))

(defun zulip-root--format-time (message)
  "Return a compact timestamp for MESSAGE."
  (let ((timestamp (and message (zulip-root--field message 'timestamp))))
    (if (and (numberp timestamp) (> timestamp 0))
        (format-time-string "%m-%d %H:%M" (seconds-to-time timestamp))
      "")))

(defun zulip-root--channel-id (channel)
  "Return normalized ID of CHANNEL."
  (zulip-root--id-string
   (or (zulip-root--field channel 'id)
       (zulip-root--field channel 'stream-id))))

(defun zulip-root--channel-name (channel)
  "Return display name of CHANNEL."
  (format "%s"
          (or (zulip-root--field channel 'name)
              (zulip-root--channel-id channel)
              "unknown channel")))

(defun zulip-root--channel-muted-p (channel)
  "Return non-nil when CHANNEL is muted or absent from the home view."
  (cond
   ((zulip-state-object-has-key-p channel 'is-muted)
    (zulip-root--true-p (zulip-root--field channel 'is-muted)))
   ((zulip-state-object-has-key-p channel 'in-home-view)
    (not (zulip-root--true-p (zulip-root--field channel 'in-home-view))))))

(defun zulip-root--topic-policy (state channel-id topic)
  "Return STATE visibility policy for CHANNEL-ID and TOPIC."
  (when-let* ((record
               (gethash (cons channel-id (downcase topic))
                        (zulip-state-user-topics state))))
    (zulip-root--field record 'visibility-policy)))

(defun zulip-root--topic-muted-p (state channel topic)
  "Return non-nil when TOPIC in CHANNEL is effectively muted in STATE."
  (let* ((channel-id (zulip-root--channel-id channel))
         (policy (zulip-root--topic-policy state channel-id topic))
         (channel-muted-p (zulip-root--channel-muted-p channel)))
    (if channel-muted-p
        (not (memq policy '(2 3)))
      (eq policy 1))))

(defun zulip-root--metric-key-channel (channel-id)
  "Return unread metric key for CHANNEL-ID."
  (cons 'channel channel-id))

(defun zulip-root--metric-key-topic (channel-id topic)
  "Return unread metric key for TOPIC in CHANNEL-ID."
  (list 'topic channel-id (downcase topic)))

(defun zulip-root--dm-entry-key (participant-ids)
  "Return stable root entry key for exact PARTICIPANT-IDS."
  (cons 'dm (cdr (zulip-state-dm-key participant-ids))))

(defun zulip-root--metric-add (table key mentioned-p)
  "Increment unread metric TABLE at KEY and optionally MENTIONED-P."
  (when key
    (let ((metric (copy-tree (or (gethash key table) '(0 . 0)))))
      (setcar metric (1+ (car metric)))
      (when mentioned-p (setcdr metric (1+ (cdr metric))))
      (puthash key metric table))))

(defun zulip-root--unread-metrics (state)
  "Return row-keyed unread and mention metrics for STATE."
  (let ((metrics (make-hash-table :test #'equal)))
    (maphash
     (lambda (message-id _present)
       (let* ((details (gethash message-id
                                (zulip-state-unread-details state)))
              (kind (zulip-root--field details 'kind))
              (mentioned-p
               (and (gethash message-id
                             (zulip-state-unread-mentions state))
                    t)))
         (pcase kind
           ('channel
            (when-let* ((channel-id
                         (zulip-root--id-string
                          (zulip-root--field details 'channel-id))))
              (zulip-root--metric-add
               metrics (zulip-root--metric-key-channel channel-id)
               mentioned-p)
              (when-let* ((topic (zulip-root--field details 'topic))
                          ((stringp topic)))
                (zulip-root--metric-add
                 metrics (zulip-root--metric-key-topic channel-id topic)
                 mentioned-p))))
           ('direct
            (when-let* ((participants
                         (zulip-root--field details 'participant-ids)))
              (zulip-root--metric-add
               metrics (zulip-root--dm-entry-key participants) mentioned-p))))))
     (zulip-state-unread state))
    metrics))

(defun zulip-root--metric (metrics key)
  "Return METRICS value for KEY, defaulting to zero counts."
  (or (gethash key metrics) '(0 . 0)))

(defun zulip-root--record-topic
    (table channel-id topic &optional message channel-name max-message-id)
  "Record TOPIC for CHANNEL-ID in TABLE, retaining recent metadata.

MESSAGE supplies the latest locally cached message and CHANNEL-NAME supplies
display context.  MAX-MESSAGE-ID is an opaque decimal string from server topic
metadata; it is compared without numeric conversion."
  (when (and channel-id (stringp topic))
    (let* ((key (zulip-root--metric-key-topic channel-id topic))
           (model (copy-sequence
                   (or (gethash key table)
                       (list :channel-id channel-id :name topic)))))
      (when (and channel-name
                 (not (string-empty-p (format "%s" channel-name))))
        (setq model (plist-put model :channel-name
                               (format "%s" channel-name))))
      (when (and message
                 (zulip-root--message-newer-p
                  message (plist-get model :latest-message)))
        (setq model (plist-put model :latest-message message)))
      (dolist (candidate
               (list max-message-id
                     (and message
                          (zulip-root--id-string
                           (zulip-root--field message 'id)))))
        (when (zulip-root--decimal-id-p candidate)
          (let ((current (plist-get model :max-message-id)))
            (when (or (not (zulip-root--decimal-id-p current))
                      (zulip-root--decimal-id< current candidate))
              (setq model
                    (plist-put model :max-message-id candidate))))))
      (puthash key model table))))

(defun zulip-root--topic-table (state)
  "Return topics known from STATE and account-scoped server metadata."
  (let ((topics (make-hash-table :test #'equal)))
    (maphash
     (lambda (_id message)
       (when (eq (zulip-root--message-kind message) 'channel)
         (zulip-root--record-topic
          topics
          (zulip-root--message-channel-id message)
          (zulip-root--message-topic message)
          message
          (zulip-root--field message 'display-recipient))))
     (zulip-state-messages state))
    (maphash
     (lambda (_message-id details)
       (when (eq (zulip-root--field details 'kind) 'channel)
         (zulip-root--record-topic
          topics
          (zulip-root--id-string (zulip-root--field details 'channel-id))
          (zulip-root--field details 'topic))))
     (zulip-state-unread-details state))
    (maphash
     (lambda (channel-id models)
       (dolist (model models)
         (zulip-root--record-topic
          topics channel-id (plist-get model :name) nil nil
          (plist-get model :max-message-id))))
     (zulip-root--topic-cache))
    topics))

(defun zulip-root--subscribed-channels (state)
  "Return STATE's subscribed channels sorted by display name."
  (sort (zulip-root--table-values (zulip-state-subscriptions state))
        (lambda (left right)
          (string-lessp (downcase (zulip-root--channel-name left))
                        (downcase (zulip-root--channel-name right))))))

(defun zulip-root--topics-for-channel (topic-table channel-id)
  "Return TOPIC-TABLE models belonging to CHANNEL-ID, sorted by name."
  (let (topics)
    (maphash
     (lambda (_key model)
       (when (equal channel-id (plist-get model :channel-id))
         (push model topics)))
     topic-table)
    (sort topics
          (lambda (left right)
            (string-lessp (downcase (plist-get left :name))
                          (downcase (plist-get right :name)))))))

(defun zulip-root--topics-by-channel (topic-table)
  "Index TOPIC-TABLE models by channel ID in one pass."
  (let ((by-channel (make-hash-table :test #'equal)))
    (maphash
     (lambda (_key model)
       (let ((channel-id (plist-get model :channel-id)))
         (push model (gethash channel-id by-channel))))
     topic-table)
    by-channel))

(defun zulip-root--topic-priority< (metrics channel-id left right)
  "Return non-nil when LEFT is more useful than RIGHT in CHANNEL-ID.

Unread and mentioned topics come first.  Remaining recency uses only opaque
decimal message-ID comparison.  METRICS contains the cached unread counts."
  (let* ((left-name (plist-get left :name))
         (right-name (plist-get right :name))
         (left-metric
          (zulip-root--metric
           metrics (zulip-root--metric-key-topic channel-id left-name)))
         (right-metric
          (zulip-root--metric
           metrics (zulip-root--metric-key-topic channel-id right-name)))
         (left-id (plist-get left :max-message-id))
         (right-id (plist-get right :max-message-id)))
    (cond
     ((/= (cdr left-metric) (cdr right-metric))
      (> (cdr left-metric) (cdr right-metric)))
     ((/= (car left-metric) (car right-metric))
      (> (car left-metric) (car right-metric)))
     ((and (zulip-root--decimal-id-p left-id)
           (zulip-root--decimal-id-p right-id)
           (zulip-root--decimal-id< right-id left-id))
      t)
     ((and (zulip-root--decimal-id-p left-id)
           (zulip-root--decimal-id-p right-id)
           (zulip-root--decimal-id< left-id right-id))
      nil)
     ((zulip-root--decimal-id-p left-id)
      (not (zulip-root--decimal-id-p right-id)))
     ((zulip-root--decimal-id-p right-id) nil)
     (t
      (string-lessp (downcase left-name) (downcase right-name))))))

(defun zulip-root--prioritize-topics (topics metrics channel-id)
  "Return TOPICS ordered for root display in CHANNEL-ID using METRICS."
  (sort (copy-sequence topics)
        (lambda (left right)
          (zulip-root--topic-priority<
           metrics channel-id left right))))

(defun zulip-root--visible-topics (topics metrics channel-id)
  "Return (VISIBLE . HIDDEN-COUNT) for CHANNEL-ID from TOPICS.

Every unread or mentioned topic is retained.  Other topics fill the remaining
`zulip-root-visible-topics-per-channel' slots by display priority.  METRICS
supplies cached unread and mention counts."
  (let ((prioritized
         (zulip-root--prioritize-topics topics metrics channel-id)))
    (if (null zulip-root-visible-topics-per-channel)
        (cons prioritized 0)
      (let ((limit (max 0 zulip-root-visible-topics-per-channel))
            protected
            ordinary)
        (dolist (topic prioritized)
          (let ((metric
                 (zulip-root--metric
                  metrics
                  (zulip-root--metric-key-topic
                   channel-id (plist-get topic :name)))))
            (if (or (> (car metric) 0) (> (cdr metric) 0))
                (push topic protected)
              (push topic ordinary))))
        (setq protected (nreverse protected)
              ordinary (nreverse ordinary))
        (let* ((room (max 0 (- limit (length protected))))
               (visible
                (append protected (seq-take ordinary room))))
          (cons visible (- (length topics) (length visible))))))))

(defun zulip-root--latest-topic-message (topics)
  "Return latest cached message represented by TOPICS."
  (let (latest)
    (dolist (topic topics latest)
      (let ((message (plist-get topic :latest-message)))
        (when (and message (zulip-root--message-newer-p message latest))
          (setq latest message))))))

(defun zulip-root--channel-operand (channel)
  "Return a valid narrow operand for CHANNEL."
  (let ((name (zulip-root--field channel 'name)))
    (if (and (stringp name) (not (string-empty-p name)))
        name
      (zulip-root--wire-integer (zulip-root--channel-id channel)
                                "Zulip channel ID"))))

(defun zulip-root--user-name (state user-id)
  "Return display name for USER-ID in STATE."
  (let ((user (zulip-state-user state user-id)))
    (format "%s"
            (or (zulip-root--field user 'full-name)
                (zulip-root--field user 'name)
                (zulip-root--field user 'email)
                user-id))))

(defun zulip-root--active-user-choices (state)
  "Return unique, secret-free completion choices for active users in STATE."
  (let ((name-counts (make-hash-table :test #'equal))
        records
        choices)
    (maphash
     (lambda (table-id user)
       (when (or (not (zulip-state-object-has-key-p user 'is-active))
                 (zulip-root--true-p
                  (zulip-root--field user 'is-active)))
         (when-let* ((id (zulip-root--id-string
                          (or (zulip-root--field user 'id)
                              (zulip-root--field user 'user-id)
                              table-id))))
           (let* ((self-p (equal id (zulip-state-self-user-id state)))
                  (base (if self-p
                            "Saved messages"
                          (format "%s"
                                  (or (zulip-root--field user 'full-name)
                                      (zulip-root--field user 'name)
                                      (format "User %s" id)))))
                  (key (downcase base)))
             (puthash key (1+ (gethash key name-counts 0)) name-counts)
             (push (list base key id) records)))))
     (zulip-state-users state))
    (dolist (record records)
      (pcase-let ((`(,base ,key ,id) record))
        (push (cons (if (> (gethash key name-counts 0) 1)
                        (format "%s (%s)" base id)
                      base)
                    id)
              choices)))
    (sort choices
          (lambda (left right)
            (string-lessp (downcase (car left))
                          (downcase (car right)))))))

(defun zulip-root--dm-other-participants (state participant-ids)
  "Return DM PARTICIPANT-IDS excluding the self user from STATE.

The authenticated user remains present for a self-DM."
  (let* ((self (zulip-state-self-user-id state))
         (participants (copy-sequence participant-ids))
         (others (delete self (copy-sequence participants))))
    (if (and self (null others) (member self participants))
        (list self)
      others)))

(defun zulip-root--dm-title (state participant-ids)
  "Return a title for exact DM PARTICIPANT-IDS using names from STATE."
  (let* ((self (zulip-state-self-user-id state))
         (others (zulip-root--dm-other-participants state participant-ids)))
    (if (and self (equal others (list self)))
        "Saved messages"
      (mapconcat (lambda (id) (zulip-root--user-name state id))
                 others ", "))))

(defun zulip-root--dm-latest-message (state conversation)
  "Return CONVERSATION's latest locally cached message from STATE."
  (let* ((max-id (zulip-dm-conversation-max-message-id conversation))
         (cached-max (and max-id (zulip-state-message state max-id)))
         (ids (zulip-dm-conversation-message-ids conversation)))
    (or cached-max
        (and ids (zulip-state-message state (car (last ids)))))))

(defun zulip-root--dm-newer-p (left right)
  "Return non-nil when DM conversation LEFT is newer than RIGHT."
  (let ((left-id (zulip-dm-conversation-max-message-id left))
        (right-id (zulip-dm-conversation-max-message-id right)))
    (cond
     ((and left-id right-id)
      (zulip-root--decimal-id< right-id left-id))
     (left-id t)
     (right-id nil)
     (t (string-lessp
         (format "%S" (zulip-dm-conversation-key left))
         (format "%S" (zulip-dm-conversation-key right)))))))

(defun zulip-root--connection-label (&optional account)
  "Return concise connection status for ACCOUNT."
  (let ((account (or account zulip-root--account)))
    (cond
     ((and (zulip-account-p account)
           (zulip-account-connected-p account))
      "connected")
     ((and (zulip-account-p account)
           (appkit-app-live-p (zulip-account-app account)))
      "connecting")
     (t "disconnected"))))

(defun zulip-root--global-mention-count (state)
  "Return exact cached unread mention count for STATE."
  (hash-table-count (zulip-state-unread-mentions state)))

(defun zulip-root--starred-unread-metric (state)
  "Return cached (UNREAD . MENTIONS) metric for starred messages in STATE."
  (let ((unread 0) (mentions 0))
    (maphash
     (lambda (message-id _present)
       (when-let* ((message (zulip-state-message state message-id))
                   (flags (zulip-root--field message 'flags))
                   ((seq-some (lambda (flag)
                                (equal (format "%s" flag) "starred"))
                              (if (vectorp flags)
                                  (append flags nil)
                                flags))))
         (cl-incf unread)
         (when (gethash message-id (zulip-state-unread-mentions state))
           (cl-incf mentions))))
     (zulip-state-unread state))
    (cons unread mentions)))

(defun zulip-root--topic-status-counts ()
  "Return (PENDING . ERRORS) for currently subscribed channels."
  (let ((pending 0)
        (errors 0)
        (error-table (zulip-root--topic-errors)))
    (dolist (channel (zulip-root--subscribed-channels
                      (zulip-root--state)))
      (when-let* ((channel-id (zulip-root--channel-id channel)))
        (when (and (appkit-task-queue-live-p zulip-root--topic-tasks)
                   (appkit-task-queue-pending-p
                    zulip-root--topic-tasks channel-id))
          (cl-incf pending))
        (when (gethash channel-id error-table)
          (cl-incf errors))))
    (cons pending errors)))

(defun zulip-root--topic-status-note ()
  "Return a safe user-facing topic hydration status, or nil."
  (pcase-let* ((`(,pending . ,errors)
                (zulip-root--topic-status-counts)))
    (cond
     ((and (> pending 0) (> errors 0))
      (format "Topic metadata: loading %d channel%s; %d previous request%s failed (g retries)."
              pending (if (= pending 1) "" "s")
              errors (if (= errors 1) "" "s")))
     ((> pending 0)
      (format "Topic metadata: loading %d channel%s…"
              pending (if (= pending 1) "" "s")))
     ((> errors 0)
      (format "Topic metadata: %d channel request%s failed; press g to retry."
              errors (if (= errors 1) "" "s"))))))

(defun zulip-root--header-line ()
  "Return dynamic header line for the current account root."
  (if (not (zulip-account-p zulip-root--account))
      " Zulip [no account]"
    (let ((state (zulip-root--state))
          (status (zulip-root--topic-status-counts)))
      (concat
       (format " Zulip [%s] %s @ %s   unread:%d  mentions:%d"
               (zulip-root--connection-label)
               (zulip-account-email zulip-root--account)
               (zulip-account-server zulip-root--account)
               (or (zulip-state-unread-count state) 0)
               (zulip-root--global-mention-count state))
       (pcase-let ((`(,pending . ,errors) status))
         (concat (if (> pending 0) (format "  topics:loading-%d" pending) "")
                 (if (> errors 0) (format "  topic-errors:%d" errors) "")))))))

(defun zulip-root--summary-text (state)
  "Return body summary for STATE and the current account."
  (format "Account %s · %s · unread %d · mentions %d"
          (if (zulip-account-p zulip-root--account)
              (zulip-account-email zulip-root--account)
            "unknown")
          (zulip-root--connection-label)
          (or (zulip-state-unread-count state) 0)
          (zulip-root--global-mention-count state)))

(defun zulip-root--trail (unread-count mention-count muted-p)
  "Return a row trail for UNREAD-COUNT, MENTION-COUNT, and MUTED-P."
  (string-join
   (delq nil
         (list
          (and (> unread-count 0)
               (propertize (number-to-string unread-count)
                           'face (if muted-p
                                     'shadow
                                   'zulip-root-unread-face)))
          (and (> mention-count 0)
               (propertize (format "@%d" mention-count)
                           'face 'zulip-root-mention-face))
          (and muted-p
               (propertize "muted" 'face 'zulip-root-muted-face))))
   " "))

(defun zulip-root--row-icon (type)
  "Return simple icon text for root entry TYPE."
  (pcase type
    ('all "*")
    ('mentioned "!")
    ('starred "★")
    ('channel "#")
    ('topic ">")
    ('dm "@")
    (_ " ")))

(defun zulip-root--insert-action-entry (entry)
  "Insert openable root ENTRY as an Appkit action row."
  (let* ((start (point))
         (type (zulip-root--entry-type entry))
         (unread (or (zulip-root--entry-unread-count entry) 0))
         (mentions (or (zulip-root--entry-mention-count entry) 0))
         (muted-p (zulip-root--entry-muted-p entry))
         (help (format "Open %s" (zulip-root--entry-title entry))))
    (appkit-view-insert-one-line-row
     (appkit-view-one-line-row-create
      :icon-inserter (lambda () (insert (zulip-root--row-icon type)))
      :context (zulip-root--entry-title entry)
      :context-trail (zulip-root--trail unread mentions muted-p)
      :preview (appkit-ui-one-line-preview-create :text (zulip-root--entry-preview entry))
      :time (zulip-root--entry-time entry)
      :time-face 'shadow
      :line-properties
      (list zulip-root--anchor-property (zulip-root--entry-key entry)
            'zulip-root-entry entry
            'zulip-root-row-type type
            'zulip-root-unread-count unread
            'zulip-root-mention-count mentions
            'zulip-root-muted-p (and muted-p t))
      :help-echo help
      :mouse-face 'highlight)
     :indent (or (zulip-root--entry-indent entry) 1)
     :width (or (zulip-root--entry-width entry) 80)
     :icon-slot-width zulip-root--icon-slot-width
     :context-width-spec '(0.34 18 36))
    (appkit-ui-make-action-row
     start (point) entry #'zulip-root--activate-entry
     :help-echo help :mouse-face 'highlight)))

(defun zulip-root--entry-printer (entry)
  "Insert one persistent root ENTRY."
  (pcase (zulip-root--entry-type entry)
    ('summary
     (appkit-view-insert-note-line
      (zulip-root--entry-title entry) :face 'font-lock-doc-face))
    ('heading
     (appkit-view-insert-heading-line
      (zulip-root--entry-title entry) :face 'bold))
    ('note
     (appkit-view-insert-note-line (zulip-root--entry-title entry)))
    ((or 'all 'mentioned 'starred 'channel 'topic 'dm)
     (zulip-root--insert-action-entry entry))
    (type (error "Unknown Zulip root entry type: %S" type))))

(defun zulip-root--project-entries (&optional all-topics-p)
  "Project the current account state into stable root entries.

When ALL-TOPICS-P is non-nil, include the full topic cache for explicit
completion and discovery.  Normal root rendering bounds topic rows according
to `zulip-root-visible-topics-per-channel'."
  (let* ((state (zulip-root--state))
         (width (zulip-root--buffer-width))
         (metrics (zulip-root--unread-metrics state))
         (topic-table (zulip-root--topic-table state))
         (topics-by-channel (zulip-root--topics-by-channel topic-table))
         (channels (zulip-root--subscribed-channels state))
         (conversations
          (sort (zulip-root--table-values
                 (zulip-state-dm-conversations state))
                #'zulip-root--dm-newer-p))
         (all-unread (or (zulip-state-unread-count state) 0))
         (all-mentions (zulip-root--global-mention-count state))
         (starred-metric (zulip-root--starred-unread-metric state))
         (topic-status (zulip-root--topic-status-note))
         entries)
    (cl-labels ((emit (entry) (push entry entries)))
      (emit
       (zulip-root--entry-create
        :key 'summary :type 'summary
        :title (zulip-root--summary-text state) :width width))
      (when topic-status
        (emit
         (zulip-root--entry-create
          :key 'topic-hydration-note :type 'note
          :title topic-status :width width)))
      (emit
       (zulip-root--entry-create
        :key 'messages-heading :type 'heading :title "Messages"
        :width width))
      (emit
       (zulip-root--entry-create
        :key '(feed all) :type 'all :title "All messages"
        :preview "Combined feed"
        :target (zulip-narrow-all)
        :completion "All messages"
        :unread-count all-unread :mention-count all-mentions
        :width width))
      (emit
       (zulip-root--entry-create
        :key '(feed mentioned) :type 'mentioned :title "Mentions"
        :preview "Messages that mention you"
        :target (zulip-narrow-mentioned)
        :completion "Mentions"
        :unread-count all-mentions :mention-count all-mentions
        :width width))
      (emit
       (zulip-root--entry-create
        :key '(feed starred) :type 'starred :title "Starred messages"
        :preview "Messages saved for later"
        :target (zulip-narrow-starred)
        :completion "Starred messages"
        :unread-count (car starred-metric)
        :mention-count (cdr starred-metric)
        :width width))
      (emit
       (zulip-root--entry-create
        :key 'channels-heading :type 'heading
        :title "Channels" :width width))
      (if channels
          (dolist (channel channels)
            (let* ((channel-id (zulip-root--channel-id channel))
                   (channel-name (zulip-root--channel-name channel))
                   (topics (gethash channel-id topics-by-channel))
                   (selection
                    (if all-topics-p
                        (cons (zulip-root--prioritize-topics
                               topics metrics channel-id)
                              0)
                      (zulip-root--visible-topics
                       topics metrics channel-id)))
                   (visible-topics (car selection))
                   (hidden-count (cdr selection))
                   (latest (zulip-root--latest-topic-message topics))
                   (metric (zulip-root--metric
                            metrics
                            (zulip-root--metric-key-channel channel-id)))
                   (muted-p (zulip-root--channel-muted-p channel))
                   (operand (zulip-root--channel-operand channel)))
              (emit
               (zulip-root--entry-create
                :key (cons 'channel channel-id)
                :type 'channel :title channel-name
                :preview (if latest
                             (zulip-root--message-preview latest)
                           (format "%d known topic%s"
                                   (length topics)
                                   (if (= (length topics) 1) "" "s")))
                :time (zulip-root--format-time latest)
                :target (zulip-narrow-channel operand)
                :completion (format "Channel: %s" channel-name)
                :unread-count (car metric) :mention-count (cdr metric)
                :muted-p muted-p :width width))
              (dolist (topic visible-topics)
                (let* ((name (plist-get topic :name))
                       (latest-message (plist-get topic :latest-message))
                       (topic-metric
                        (zulip-root--metric
                         metrics
                         (zulip-root--metric-key-topic channel-id name))))
                  (emit
                   (zulip-root--entry-create
                    :key (zulip-root--metric-key-topic channel-id name)
                    :type 'topic :title name
                    :preview (zulip-root--message-preview latest-message)
                    :time (zulip-root--format-time latest-message)
                    :target (zulip-narrow-topic operand name)
                    :completion (format "Topic: %s > %s"
                                        channel-name name)
                    :unread-count (car topic-metric)
                    :mention-count (cdr topic-metric)
                    :muted-p (zulip-root--topic-muted-p
                              state channel name)
                    :indent 4 :width width))))
              (when (> hidden-count 0)
                (emit
                 (zulip-root--entry-create
                  :key (list 'topics-hidden channel-id)
                  :type 'note
                  :title (format
                          "  … %d older topic%s hidden; use t or / to open."
                          hidden-count (if (= hidden-count 1) "" "s"))
                  :width width)))))
        (emit
         (zulip-root--entry-create
          :key 'channels-empty :type 'note
          :title (if (zulip-account-connected-p zulip-root--account)
                     "No subscribed channels cached."
                   "Waiting for Zulip registration…")
          :width width)))
      (emit
       (zulip-root--entry-create
        :key 'dm-heading :type 'heading
        :title "Direct messages" :width width))
      (if conversations
          (dolist (conversation conversations)
            (let* ((participants
                    (zulip-dm-conversation-participant-ids conversation))
                   (others
                    (zulip-root--dm-other-participants state participants))
                   (title (zulip-root--dm-title state participants))
                   (latest (zulip-root--dm-latest-message state conversation))
                   (key (zulip-root--dm-entry-key participants))
                   (metric (zulip-root--metric metrics key))
                   (recipient-ids
                    (mapcar (lambda (id)
                              (zulip-root--wire-integer id "Zulip user ID"))
                            others)))
              (emit
               (zulip-root--entry-create
                :key key :type 'dm :title title
                :preview (if latest
                             (zulip-root--message-preview latest)
                           "Recent direct conversation")
                :time (zulip-root--format-time latest)
                :target (zulip-narrow-direct recipient-ids title)
                :completion (format "DM: %s" title)
                :unread-count (car metric) :mention-count (cdr metric)
                :width width))))
        (emit
         (zulip-root--entry-create
          :key 'dm-empty :type 'note
          :title "No recent direct conversations cached."
          :width width))))
    (nreverse entries)))

(defun zulip-root--selected-window ()
  "Return the selected window when it displays the current root buffer."
  (let ((window (selected-window)))
    (and (window-live-p window)
         (eq (window-buffer window) (current-buffer))
         window)))

(defun zulip-root--display-window ()
  "Return the widest live window displaying the current root buffer."
  (let ((best nil) (best-width -1))
    (dolist (window (get-buffer-window-list (current-buffer) nil t) best)
      (let ((width (if (window-live-p window)
                       (window-width window 'remap)
                     -1)))
        (when (> width best-width)
          (setq best window best-width width))))))

(defun zulip-root--compute-fill-column (&optional window)
  "Compute root row width from live WINDOW, or return nil."
  (when-let* ((window (or window (zulip-root--display-window)))
              (width (appkit-view-window-fill-column window 3)))
    (max 60 width)))

(defun zulip-root--stable-fill-column ()
  "Return stable width for the next root reconciliation."
  (or (when-let* ((window (zulip-root--selected-window)))
        (zulip-root--compute-fill-column window))
      (and (integerp zulip-root--fill-column)
           (> zulip-root--fill-column 0)
           zulip-root--fill-column)
      (zulip-root--compute-fill-column (zulip-root--display-window))
      80))

(defun zulip-root--buffer-width ()
  "Return current root row width in columns."
  (max 60 (or zulip-root--fill-column
              (setq-local zulip-root--fill-column
                          (zulip-root--stable-fill-column)))))

(defun zulip-root--sync (&optional force-keys)
  "Reconcile the current root, explicitly invalidating FORCE-KEYS."
  (unless (ewoc-p zulip-root--ewoc)
    (error "Zulip root view is not initialized"))
  (let ((view (appkit-current-view))
        (snapshot
         (appkit-position-capture
          :anchor-property zulip-root--anchor-property
          :preserve-window-start t)))
    (unless (appkit-view-live-p view)
      (error "Zulip root sync requires a live Appkit view"))
    (appkit-with-content-update view
      (setq-local zulip-root--fill-column (zulip-root--stable-fill-column))
      (setf (appkit-view-state view) (zulip-root--state))
      (setq-local zulip-root--node-table
                  (appkit-ewoc-reconcile
                   zulip-root--ewoc
                   (zulip-root--project-entries)
                   #'zulip-root--entry-key
                   :force-keys force-keys))
      (when snapshot (appkit-position-restore snapshot)))))

(defun zulip-root--sync-invalidations (view invalidations)
  "Synchronize VIEW from coalesced INVALIDATIONS."
  (let ((events (appkit-view-pending-events-snapshot view)))
    (zulip-root--sync (appkit-invalidations-entry-keys invalidations))
    (appkit-view-acknowledge-events view (length events))))

(defun zulip-root--invalidate-and-sync (&optional force-keys)
  "Invalidate the current root projection and synchronously reconcile it.

FORCE-KEYS are stable entry keys that must be reprinted even when their
projected values compare equal.  Production code uses this immediate form only
for a newly attached buffer's first projection, before it is returned to the
caller.  Runtime callbacks, commands, and geometry changes use the scheduled
form so Appkit can coalesce their invalidations."
  (let ((view (appkit-current-view)))
    (unless (appkit-view-live-p view)
      (error "Zulip root has no live Appkit view"))
    (appkit-invalidate view
                       :structure t
                       :part 'entries
                       :entries force-keys)
    (appkit-sync-invalidations view)))

(defun zulip-root--invalidate-and-schedule (&optional force-keys)
  "Invalidate the current root projection and schedule an Appkit sync.

FORCE-KEYS are stable entry keys that must be reprinted even when their
projected values compare equal."
  (let ((view (appkit-current-view)))
    (unless (appkit-view-live-p view)
      (error "Zulip root has no live Appkit view"))
    (appkit-request-sync
     view :structure t :part 'entries :entries force-keys)))

(defun zulip-root--view-id (account)
  "Return strict account-scoped root view identity for ACCOUNT."
  (list 'zulip-root (zulip-account-id account)))

(defun zulip-root--buffer-name (account)
  "Return strict account-scoped root buffer name for ACCOUNT."
  (format "*Zulip %s <%s> · Home*"
          (zulip-account-server account)
          (zulip-account-email account)))

(defun zulip-root--topic-models-from-result (result)
  "Return normalized, case-deduplicated topic models from API RESULT.

The endpoint's `max_id' message identifier is normalized to an opaque string
and is never used for arithmetic or ordering."
  (unless (and (zulip-api-result-p result)
               (zulip-api-result-ok-p result))
    (error "Zulip topic request did not succeed"))
  (let* ((data (zulip-api-result-data result))
         (raw-topics (and data (zulip-root--field data 'topics)))
         (seen (make-hash-table :test #'equal))
         models)
    (unless (and data
                 (zulip-state-object-has-key-p data 'topics)
                 (or (vectorp raw-topics) (listp raw-topics)))
      (error "Malformed Zulip topic response"))
    (dolist (raw-topic (if (vectorp raw-topics)
                           (append raw-topics nil)
                         raw-topics))
      (let* ((name (zulip-root--field raw-topic 'name))
             (key (and (stringp name) (downcase name)))
             (max-id (zulip-root--field raw-topic 'max-id)))
        (unless (stringp name)
          (error "Malformed Zulip topic record"))
        (unless (gethash key seen)
          (puthash key t seen)
          (push (append (list :name name)
                        (when max-id
                          (list :max-message-id
                                (zulip-state-normalize-id max-id))))
                models))))
    (nreverse models)))

(defun zulip-root--topic-error-record (result &optional malformed-p)
  "Return a credential-free observable error record for RESULT.

MALFORMED-P denotes a successful HTTP response with an invalid payload."
  (list :status (and (zulip-api-result-p result)
                     (zulip-api-result-status result))
        :code (and (zulip-api-result-p result)
                   (zulip-api-result-code result))
        :transport-p (and (zulip-api-result-p result)
                          (zulip-api-result-transport-error result)
                          t)
        :parse-p (and (zulip-api-result-p result)
                      (zulip-api-result-parse-error result)
                      t)
        :malformed-p (and malformed-p t)))

(defun zulip-root--accept-topic-result (view channel-id result)
  "Accept VIEW's current CHANNEL-ID topic request RESULT."
  (appkit-with-live-view view
    (if (and (zulip-api-result-p result)
             (zulip-api-result-ok-p result))
        (condition-case nil
            (let ((models (zulip-root--topic-models-from-result result)))
              (puthash channel-id models (zulip-root--topic-cache))
              (remhash channel-id (zulip-root--topic-errors)))
          (error
           ;; A malformed success is not allowed to erase a previous cache.
           (puthash channel-id
                    (zulip-root--topic-error-record result t)
                    (zulip-root--topic-errors))))
      ;; Network/API failures retain the last good topic list.
      (puthash channel-id
               (zulip-root--topic-error-record result)
               (zulip-root--topic-errors)))
    ;; Appkit retires the keyed task before calling this finisher.  A short
    ;; delay lets staggered HTTP callbacks accumulate behind one projection
    ;; instead of repeatedly rebuilding a large root on Emacs's main thread.
    (appkit-request-sync
     view :structure t :part 'entries
     :delay (max 0 zulip-root-topic-hydration-sync-delay))))

(defun zulip-root--topic-concurrency-limit ()
  "Return the validated topic hydration concurrency limit."
  (max 1 (if (integerp zulip-root-topic-hydration-concurrency)
             zulip-root-topic-hydration-concurrency
           1)))

(defun zulip-root--ensure-topic-tasks (&optional view)
  "Return this root's Appkit topic task queue for live VIEW."
  (setq view (or view (appkit-current-view)))
  (unless (appkit-view-live-p view)
    (error "Zulip topic hydration requires a live root view"))
  (if (and (appkit-task-queue-live-p zulip-root--topic-tasks)
           (eq view (appkit-task-queue-owner zulip-root--topic-tasks)))
      (appkit-task-queue-set-limit
       zulip-root--topic-tasks (zulip-root--topic-concurrency-limit))
    (setq-local
     zulip-root--topic-tasks
     (appkit-task-queue-create
      view (zulip-root--topic-concurrency-limit))))
  zulip-root--topic-tasks)

(defun zulip-root--submit-topic-request (channel)
  "Submit one view-owned topic request for subscribed CHANNEL."
  (let* ((view (appkit-current-view))
         (queue (zulip-root--ensure-topic-tasks view))
         (account zulip-root--account)
         (channel-id (zulip-root--channel-id channel))
         (stream-id
          (zulip-root--wire-integer channel-id "Zulip channel ID")))
    (appkit-task-queue-submit
     queue channel-id
     (lambda (complete)
       (let ((request
               (zulip-api-get-topics
                account stream-id complete :owner view)))
         (when request
           (lambda () (zulip-http-cancel-request request)))))
     :finish
     (lambda (result)
       (zulip-root--accept-topic-result view channel-id result)))))

(defun zulip-root--prune-topic-tasks (subscribed-ids)
  "Cancel topic tasks whose keys are absent from SUBSCRIBED-IDS."
  (when (appkit-task-queue-live-p zulip-root--topic-tasks)
    (let (stale-keys)
      (dolist (channel-id
               (appkit-task-queue-pending-keys zulip-root--topic-tasks))
        (unless (gethash channel-id subscribed-ids)
          (push channel-id stale-keys)))
      (appkit-task-queue-cancel-keys
       zulip-root--topic-tasks (nreverse stale-keys)))))

(defun zulip-root--hydrate-topics (&optional force)
  "Hydrate topics for subscribed channels, returning channels newly queued.

Normally only channels absent from the account cache are requested.  FORCE
requests fresh server metadata even when cached, while still coalescing an
already active or queued request for the same channel.  At most
`zulip-root-topic-hydration-concurrency' requests run simultaneously."
  (let* ((channels (zulip-root--subscribed-channels
                    (zulip-root--state)))
         (subscribed-ids (make-hash-table :test #'equal))
         (queue (zulip-root--ensure-topic-tasks))
         (cache (zulip-root--topic-cache))
         (missing (make-symbol "missing"))
         (queued 0))
    (dolist (channel channels)
      (when-let* ((channel-id (zulip-root--channel-id channel)))
        (puthash channel-id t subscribed-ids)))
    (zulip-root--prune-topic-tasks subscribed-ids)
    (dolist (channel channels)
      (when-let* ((channel-id (zulip-root--channel-id channel)))
        (when (and (not (appkit-task-queue-pending-p queue channel-id))
                   (or force
                       (eq (gethash channel-id cache missing) missing))
                   (zulip-root--submit-topic-request channel))
          (cl-incf queued))))
    queued))

(defun zulip-root--queue-refresh (account event &optional refresh-topics-p)
  "Queue EVENT and refresh ACCOUNT's live root view.

When REFRESH-TOPICS-P is non-nil, request fresh server topic metadata even
for channels already represented in the account cache."
  (when (and (zulip-account-p account)
             (appkit-app-live-p (zulip-account-app account)))
    (when-let* ((view (appkit-view-for-id
                       (zulip-account-app account)
                       (zulip-root--view-id account))))
      (appkit-view-enqueue-event view event)
      (appkit-with-live-view view
        (zulip-root--hydrate-topics refresh-topics-p))
      (appkit-request-sync view :structure t :part 'entries))))

(defun zulip-root--on-register (account state)
  "Refresh ACCOUNT root after registration installs STATE."
  (zulip-root--queue-refresh
   account (list :type 'register :state state) t))

(defun zulip-root--on-state-changed (change)
  "Refresh the affected account root after canonical state CHANGE."
  (when-let* ((account (plist-get change :account)))
    (zulip-root--queue-refresh
     account change (eq (plist-get change :type) 'subscription))))

(defun zulip-root--ensure-event-subscriptions (account)
  "Install one Appkit-owned root event fanout for ACCOUNT."
  (let* ((app (zulip-account-app account))
         (store (appkit-app-resource-store app)))
    (unless (gethash zulip-root--event-subscriptions-key store)
      (puthash
       zulip-root--event-subscriptions-key
       (list
        (appkit-app-on app 'zulip-register #'zulip-root--on-register)
        (appkit-app-on app 'zulip-state-changed
                       #'zulip-root--on-state-changed))
       store))))

(defun zulip-root--entry-at-point (&optional position)
  "Return root entry at POSITION, or current point."
  (let ((position (or position (point))))
    (or (get-text-property position 'zulip-root-entry)
        (get-text-property (line-beginning-position)
                           'zulip-root-entry))))

(defun zulip-root--activate-entry (entry)
  "Open the narrow represented by root ENTRY."
  (unless (and (zulip-root--entry-p entry)
               (zulip-root--entry-target entry))
    (user-error "No Zulip destination on this row"))
  (unless (zulip-account-p zulip-root--account)
    (user-error "This navigator has no live Zulip account"))
  (zulip-feed-open zulip-root--account
                   (zulip-root--entry-target entry)))

(defun zulip-root-open-at-point ()
  "Open the feed represented by the row at point."
  (interactive)
  (zulip-root--activate-entry
   (or (zulip-root--entry-at-point)
       (user-error "No Zulip destination at point"))))

(defun zulip-root-mouse-open-at-point (event)
  "Open the feed clicked by mouse EVENT."
  (interactive "e")
  (mouse-set-point event)
  (zulip-root-open-at-point))

(defun zulip-root--move-linewise (direction predicate &optional wrap)
  "Move in DIRECTION until PREDICATE succeeds, optionally WRAP once."
  (let ((origin (point)) (wrapped nil) found)
    (while (not found)
      (forward-line direction)
      (cond
       ((and (> direction 0) (eobp))
        (if (and wrap (not wrapped))
            (progn (setq wrapped t) (goto-char (point-min)))
          (setq found 'stop)))
       ((and (< direction 0) (bobp))
        (if (and wrap (not wrapped))
            (progn
              (setq wrapped t)
              (goto-char (point-max))
              (forward-line -1))
          (setq found 'stop)))
       ((funcall predicate) (setq found t))))
    (unless (eq found t)
      (goto-char origin)
      nil)))

(defun zulip-root-next-row ()
  "Move to the next openable navigator row, wrapping once."
  (interactive)
  (zulip-root--move-linewise
   1 (lambda () (zulip-root--entry-at-point)) t))

(defun zulip-root-previous-row ()
  "Move to the previous openable navigator row, wrapping once."
  (interactive)
  (zulip-root--move-linewise
   -1 (lambda () (zulip-root--entry-at-point)) t))

(defun zulip-root-next-unread ()
  "Move to the next unread channel, topic, or direct conversation."
  (interactive)
  (unless (zulip-root--move-linewise
           1
           (lambda ()
             (and (zulip-root--entry-at-point)
                  (> (or (get-text-property
                          (point) 'zulip-root-unread-count)
                         0)
                     0)
                  (not (eq (get-text-property
                            (point) 'zulip-root-row-type)
                           'all))))
           t)
    (message "Zulip: no unread conversations")))

(defun zulip-root--completion-choices ()
  "Return completion labels paired with every known destination.

Unlike the rendered root, explicit destination discovery includes the full
account topic cache."
  (let (choices)
    (dolist (entry (zulip-root--project-entries t))
      (when (and (zulip-root--entry-target entry)
                 (zulip-root--entry-completion entry))
        (push (cons (zulip-root--entry-completion entry) entry) choices)))
    (nreverse choices)))

(defun zulip-root--read-channel ()
  "Prompt for and return one subscribed channel object."
  (let* ((channels (zulip-root--subscribed-channels (zulip-root--state)))
         (choices (mapcar (lambda (channel)
                            (cons (zulip-root--channel-name channel) channel))
                          channels)))
    (unless choices (user-error "No subscribed Zulip channels cached"))
    (cdr (assoc (completing-read "Channel: " choices nil t) choices))))

(defun zulip-root-open-topic ()
  "Prompt for a subscribed channel and topic, allowing a new topic name."
  (interactive)
  (let* ((channel (zulip-root--read-channel))
         (channel-id (zulip-root--channel-id channel))
         (models (zulip-root--topics-for-channel
                  (zulip-root--topic-table (zulip-root--state)) channel-id))
         (known (mapcar (lambda (model) (plist-get model :name)) models))
         (topic (completing-read "Topic (new names allowed): " known nil nil)))
    (when (string-empty-p (string-trim topic))
      (user-error "Zulip topic must not be empty"))
    (zulip-feed-open
     zulip-root--account
     (zulip-narrow-topic (zulip-root--channel-operand channel) topic))))

(defun zulip-root-open-new-direct-message ()
  "Prompt for one or more active users and open their direct conversation."
  (interactive)
  (let* ((state (zulip-root--state))
         (choices (zulip-root--active-user-choices state)))
    (unless choices
      (user-error "No active Zulip users cached"))
    (let* ((labels
            (completing-read-multiple
             "Direct message to (comma-separated): " choices nil t))
           (selected
            (delete-dups
             (mapcar
              (lambda (label)
                (or (cdr (assoc label choices))
                    (user-error "Unknown Zulip user: %s" label)))
              labels)))
           (self-id (zulip-state-self-user-id state))
           (others (and self-id (delete self-id (copy-sequence selected))))
           (recipients
            (cond
             (others others)
             ((and self-id (member self-id selected)) (list self-id))
             (t selected))))
      (unless recipients
        (user-error "Choose at least one Zulip user"))
      (let* ((participants
              (delete-dups (if self-id
                               (cons self-id (copy-sequence selected))
                             (copy-sequence selected))))
             (title (zulip-root--dm-title state participants))
             (wire-recipients
              (mapcar (lambda (id)
                        (zulip-root--wire-integer id "Zulip user ID"))
                      recipients)))
        (zulip-feed-open
         zulip-root--account
         (zulip-narrow-direct wire-recipients title))))))

(defun zulip-root-search-messages (&optional query)
  "Prompt for QUERY and open a server-backed Zulip message search."
  (interactive)
  (let ((query (or query (read-string "Search Zulip messages: "))))
    (zulip-feed-open zulip-root--account (zulip-narrow-search query))))

(defun zulip-root-open-destination ()
  "Complete and open a feed, conversation, search, or new topic."
  (interactive)
  (let* ((choices (zulip-root--completion-choices))
         (new-topic-label "New topic…")
         (new-dm-label "New direct message…")
         (search-label "Search messages…")
         (label (completing-read
                 "Open Zulip destination: "
                 (append (mapcar #'car choices)
                         (list new-dm-label search-label new-topic-label))
                 nil t)))
    (cond
     ((equal label new-topic-label)
      (call-interactively #'zulip-root-open-topic))
     ((equal label new-dm-label)
      (call-interactively #'zulip-root-open-new-direct-message))
     ((equal label search-label)
      (call-interactively #'zulip-root-search-messages))
     (t
      (zulip-root--activate-entry
       (or (cdr (assoc label choices))
           (user-error "Unknown Zulip destination: %s" label)))))))

(defun zulip-root-refresh ()
  "Refresh server topic metadata and the current navigator projection."
  (interactive)
  (let ((started (zulip-root--hydrate-topics t)))
    (zulip-root--invalidate-and-schedule
     (mapcar #'zulip-root--entry-key (zulip-root--project-entries)))
    (if (> started 0)
        (message "Zulip: refreshing topic metadata for %d channel%s"
                 started (if (= started 1) "" "s"))
      (message "Zulip: topic metadata is already refreshing"))))

(defun zulip-root--reflow-visible (&optional force)
  "Reflow visible root rows, invalidating all rows when FORCE is non-nil."
  (when (derived-mode-p 'zulip-root-mode)
    (when-let* ((window (or (zulip-root--selected-window)
                            (zulip-root--display-window)))
                (next (zulip-root--compute-fill-column window)))
      (when (or force (not (equal next zulip-root--fill-column)))
        (setq-local zulip-root--fill-column next)
        (zulip-root--invalidate-and-schedule
         (and force
              (mapcar #'zulip-root--entry-key
                      (seq-filter #'zulip-root--entry-target
                                  (zulip-root--project-entries)))))
        t))))

(defun zulip-root--on-window-size-change (&optional _frame)
  "Reflow a visible root after its window geometry changes."
  (zulip-root--reflow-visible nil))

(defun zulip-root--on-text-scale-change ()
  "Reflow a visible root after text scaling changes."
  (zulip-root--reflow-visible t))

(defvar zulip-root-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'zulip-root-open-at-point)
    (define-key map [mouse-1] #'zulip-root-mouse-open-at-point)
    (define-key map (kbd "n") #'zulip-root-next-row)
    (define-key map (kbd "p") #'zulip-root-previous-row)
    (define-key map (kbd "u") #'zulip-root-next-unread)
    (define-key map (kbd "g") #'zulip-root-refresh)
    (define-key map (kbd "/") #'zulip-root-open-destination)
    (define-key map (kbd "m") #'zulip-root-open-new-direct-message)
    (define-key map (kbd "s") #'zulip-root-search-messages)
    (define-key map (kbd "t") #'zulip-root-open-topic)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `zulip-root-mode'.")

(define-derived-mode zulip-root-mode special-mode "Zulip-Home"
  "Major mode for one account-scoped Zulip navigator."
  (setq buffer-read-only t
        truncate-lines t)
  (buffer-disable-undo)
  (setq-local buffer-undo-list t)
  (setq-local switch-to-buffer-preserve-window-point nil)
  (setq-local zulip-root--fill-column nil)
  (setq-local zulip-root--topic-tasks nil)
  (setq-local zulip-root--node-table (make-hash-table :test #'equal))
  (setq-local header-line-format '(:eval (zulip-root--header-line)))
  (let ((inhibit-read-only t) (buffer-undo-list t))
    (erase-buffer)
    (setq-local zulip-root--ewoc
                (ewoc-create #'zulip-root--entry-printer nil nil t)))
  (add-hook 'window-size-change-functions
            #'zulip-root--on-window-size-change nil t)
  (add-hook 'display-line-numbers-mode-hook
            #'zulip-root--on-window-size-change nil t)
  (add-hook 'text-scale-mode-hook #'zulip-root--on-text-scale-change nil t))

(defun zulip-root--open-buffer (account)
  "Open or reuse ACCOUNT's Appkit root view and return its buffer."
  (unless (and (zulip-account-p account)
               (appkit-app-live-p (zulip-account-app account)))
    (error "Zulip root requires a live account"))
  (let* ((app (zulip-account-app account))
         (view-id (zulip-root--view-id account))
         (existing (appkit-view-for-id app view-id))
         (view
          (appkit-open-view
           :app app
           :id view-id
           :mode 'zulip-root-mode
           :buffer-name (zulip-root--buffer-name account)
           :state (zulip-account-state account)
           :sync-function #'zulip-root--sync-invalidations
           :parts '(frame entries)
           :setup
           (lambda (new-view)
             (setq-local zulip-root--account account)
             ;; The buffer can survive a killed view.  Its replacement gets a
             ;; fresh owner-scoped queue; the old queue and task tokens die
             ;; with their detached Appkit view.
             (setq-local zulip-root--topic-tasks nil)
             (zulip-root--ensure-topic-tasks new-view)
             (zulip-root--ensure-event-subscriptions account))))
         (buffer (appkit-view-buffer view)))
    (with-current-buffer buffer
      (setq-local zulip-root--account account)
      (zulip-root--ensure-event-subscriptions account)
      (zulip-root--hydrate-topics)
      (unless existing
        (zulip-root--invalidate-and-sync)))
    buffer))

(defun zulip-root--read-account ()
  "Prompt for one live Zulip account."
  (let* ((accounts (zulip-runtime-accounts))
         (choices
          (mapcar
           (lambda (account)
             (cons (format "%s @ %s"
                           (zulip-account-email account)
                           (zulip-account-server account))
                   account))
           accounts)))
    (unless choices (user-error "No live Zulip accounts"))
    (cdr (assoc (completing-read "Zulip account: " choices nil t)
                choices))))

;;;###autoload
(defun zulip-root-open (account)
  "Open ACCOUNT's persistent Zulip navigator."
  (interactive (list (zulip-root--read-account)))
  (let ((buffer (zulip-root--open-buffer account)))
    (pop-to-buffer buffer)
    (with-current-buffer buffer
      (zulip-root--reflow-visible nil)
      (unless (zulip-root--entry-at-point)
        (goto-char (point-min))
        (zulip-root-next-row)))
    buffer))

(provide 'zulip-root)

;;; zulip-root.el ends here
