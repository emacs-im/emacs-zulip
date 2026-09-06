;;; zulip-state.el --- Normalized immutable Zulip account state -*- lexical-binding: t; -*-

;;; Commentary:

;; This module is the protocol boundary for cached Zulip objects.  JSON hash
;; tables and alists are accepted at the boundary, while cached objects use
;; symbol-key alists.  In particular, server message IDs are always exposed as
;; opaque decimal strings; callers must never perform arithmetic on them.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(cl-defstruct (zulip-dm-conversation
               (:constructor zulip-dm-conversation--create))
  "A direct-message conversation identified by its exact participant set."
  key
  participant-ids
  message-ids
  max-message-id)

(cl-defstruct (zulip-state
               (:constructor zulip-state--create))
  "One account's normalized, reducer-owned state."
  users
  channels
  subscriptions
  messages
  narrows
  unread
  unread-counted
  unread-details
  unread-mentions
  user-topics
  dm-conversations
  self-user-id
  unread-count
  register-data)

(defun zulip-state--new-table ()
  "Return a hash table suitable for protocol identifiers."
  (make-hash-table :test #'equal))

(defun zulip-state-create ()
  "Return an empty normalized Zulip state."
  (let ((state
         (zulip-state--create
          :users (zulip-state--new-table)
          :channels (zulip-state--new-table)
          :subscriptions (zulip-state--new-table)
          :messages (zulip-state--new-table)
          :narrows (zulip-state--new-table)
          :unread (zulip-state--new-table)
          :unread-counted (zulip-state--new-table)
          :unread-details (zulip-state--new-table)
          :unread-mentions (zulip-state--new-table)
          :user-topics (zulip-state--new-table)
          :dm-conversations (zulip-state--new-table)
          :unread-count 0)))
    ;; The nil narrow is the combined feed and is always maintained.
    (puthash nil nil (zulip-state-narrows state))
    state))

(defun zulip-state--copy-table (table &optional value-copier)
  "Return a copy of hash TABLE, optionally copying values with VALUE-COPIER."
  (let ((copy (make-hash-table :test (hash-table-test table)
                               :size (max 1 (hash-table-count table)))))
    (maphash (lambda (key value)
               (puthash key (if value-copier
                                (funcall value-copier value)
                              value)
                        copy))
             table)
    copy))

(defun zulip-state-copy (state)
  "Return a reducer-safe copy of STATE.

Cached protocol objects are immutable values.  Mutable indices and DM records
are copied so subsequent reducer operations cannot alter STATE."
  (let ((copy (copy-zulip-state state)))
    (setf (zulip-state-users copy)
          (zulip-state--copy-table (zulip-state-users state))
          (zulip-state-channels copy)
          (zulip-state--copy-table (zulip-state-channels state))
          (zulip-state-subscriptions copy)
          (zulip-state--copy-table (zulip-state-subscriptions state))
          (zulip-state-messages copy)
          (zulip-state--copy-table (zulip-state-messages state))
          (zulip-state-narrows copy)
          (zulip-state--copy-table (zulip-state-narrows state) #'copy-sequence)
          (zulip-state-unread copy)
          (zulip-state--copy-table (zulip-state-unread state))
          (zulip-state-unread-counted copy)
          (zulip-state--copy-table (zulip-state-unread-counted state))
          (zulip-state-unread-details copy)
          (zulip-state--copy-table (zulip-state-unread-details state)
                                   #'copy-tree)
          (zulip-state-unread-mentions copy)
          (zulip-state--copy-table (zulip-state-unread-mentions state))
          (zulip-state-user-topics copy)
          (zulip-state--copy-table (zulip-state-user-topics state)
                                   #'copy-tree)
          (zulip-state-dm-conversations copy)
          (zulip-state--copy-table
           (zulip-state-dm-conversations state)
           (lambda (conversation)
             (let ((copy (copy-zulip-dm-conversation conversation)))
               (setf (zulip-dm-conversation-participant-ids copy)
                     (copy-sequence
                      (zulip-dm-conversation-participant-ids conversation))
                     (zulip-dm-conversation-message-ids copy)
                     (copy-sequence
                      (zulip-dm-conversation-message-ids conversation)))
               copy))))
    copy))

(defun zulip-state--canonical-key (key)
  "Return canonical symbol form of JSON object KEY."
  (cond
   ((keywordp key) (intern (substring (symbol-name key) 1)))
   ((symbolp key) key)
   ((stringp key) (intern key))
   (t key)))

(defun zulip-state--key-variants (key)
  "Return object key variants accepted for KEY."
  (let* ((symbol (zulip-state--canonical-key key))
         (name (and (symbolp symbol) (symbol-name symbol)))
         (alternate
          (and name
               (if (string-match-p "_" name)
                   (replace-regexp-in-string "_" "-" name)
                 (replace-regexp-in-string "-" "_" name)))))
    (delete-dups
     (delq nil
           (list symbol
                 name
                 (and name (intern (concat ":" name)))
                 (and alternate (intern alternate))
                 alternate
                 (and alternate (intern (concat ":" alternate))))))))

(defun zulip-state-object-get (object key &optional default)
  "Return OBJECT's KEY across hash-table, alist, and plist representations.

Return DEFAULT when no accepted variant of KEY is present."
  (let* ((missing (make-symbol "missing"))
         (value missing))
    (cond
     ((hash-table-p object)
      (dolist (variant (zulip-state--key-variants key))
        (when (eq value missing)
          (setq value (gethash variant object missing)))))
     ((and (listp object)
           (or (null object) (consp (car object))))
      (dolist (variant (zulip-state--key-variants key))
        (when (eq value missing)
          (let ((entry (assoc variant object)))
            (when entry
              (setq value (cdr entry)))))))
     ((listp object)
      (dolist (variant (zulip-state--key-variants key))
        (when (and (eq value missing)
                   (symbolp variant)
                   (plist-member object variant))
          (setq value (plist-get object variant))))))
    (if (eq value missing) default value)))

(defun zulip-state-object-has-key-p (object key)
  "Return non-nil when OBJECT contains KEY, including a nil value."
  (let ((missing (make-symbol "missing")))
    (not (eq (zulip-state-object-get object key missing) missing))))

(defun zulip-state-normalize-object (object)
  "Recursively normalize JSON OBJECT to symbol-key alists and lists."
  (cond
   ((hash-table-p object)
    (let (result)
      (maphash (lambda (key value)
                 (push (cons (zulip-state--canonical-key key)
                             (zulip-state-normalize-object value))
                       result))
               object)
      (nreverse result)))
   ((vectorp object)
    (mapcar #'zulip-state-normalize-object (append object nil)))
   ((and (consp object)
         (cl-every (lambda (entry)
                     (and (consp entry)
                          (or (symbolp (car entry))
                              (stringp (car entry)))))
                   object))
    (mapcar (lambda (entry)
              (cons (zulip-state--canonical-key (car entry))
                    (zulip-state-normalize-object (cdr entry))))
            object))
   ((consp object)
    (mapcar #'zulip-state-normalize-object object))
   (t object)))

(defun zulip-state-object-put (object key value)
  "Return normalized OBJECT with KEY set to VALUE."
  (let* ((object (zulip-state-normalize-object object))
         (canonical (zulip-state--canonical-key key))
         (variants (zulip-state--key-variants key)))
    (cons (cons canonical value)
          (cl-remove-if (lambda (entry)
                          (member (car-safe entry) variants))
                        object))))

(defun zulip-state--merge-objects (base overlay)
  "Return normalized BASE with every field from OVERLAY applied."
  (let ((result (copy-tree (or (zulip-state-normalize-object base) nil))))
    (dolist (entry (zulip-state-normalize-object overlay))
      (setq result (zulip-state-object-put result (car entry) (cdr entry))))
    result))

(defun zulip-state-normalize-id (id)
  "Return opaque string form of integer or string ID."
  (cond
   ((integerp id) (number-to-string id))
   ((stringp id) id)
   ((null id) nil)
   (t (error "Zulip identifier is not an integer or string: %S" id))))

(defun zulip-state-server-message-id-p (id)
  "Return non-nil when ID is a canonical decimal server message ID."
  (and (stringp id) (string-match-p "\\`[0-9]+\\'" id)))

(defun zulip-state-message-id (message-or-id)
  "Return MESSAGE-OR-ID as an opaque string message identifier."
  (let ((id (if (or (hash-table-p message-or-id)
                    (and (consp message-or-id)
                         (consp (car message-or-id))))
                (zulip-state-object-get message-or-id 'id)
              message-or-id)))
    (zulip-state-normalize-id id)))

(defun zulip-state--as-list (value)
  "Return vector or list VALUE as a list."
  (cond ((vectorp value) (append value nil))
        ((listp value) value)
        ((null value) nil)
        (t (list value))))

(defun zulip-state--message-kind (message)
  "Return canonical `channel' or `direct' kind for MESSAGE."
  (let ((type (downcase (format "%s"
                                (or (zulip-state-object-get message 'kind)
                                    (zulip-state-object-get message 'type)
                                    "")))))
    (cond ((member type '("stream" "channel")) 'channel)
          ((member type '("private" "direct" "dm")) 'direct)
          (t nil))))

(defun zulip-state--string-equal-ignore-case (left right)
  "Return non-nil when strings LEFT and RIGHT differ only by case."
  (eq t (compare-strings left nil nil right nil nil t)))

(defun zulip-state-normalize-message (message)
  "Return canonical cached representation of MESSAGE."
  (let* ((message (zulip-state-normalize-object message))
         (id (zulip-state-message-id message))
         (local-id (or (zulip-state-object-get message 'local-id)
                       (zulip-state-object-get message 'local_message_id)))
         (kind (zulip-state--message-kind message))
         (topic (or (zulip-state-object-get message 'topic)
                    (zulip-state-object-get message 'subject)))
         (channel-id (or (zulip-state-object-get message 'channel-id)
                         (zulip-state-object-get message 'stream_id))))
    (unless id
      (error "Zulip message has no id: %S" message))
    (setq message (zulip-state-object-put message 'id id))
    (when local-id
      (setq message
            (zulip-state-object-put
             message 'local-id (zulip-state-normalize-id local-id))))
    (when kind
      (setq message (zulip-state-object-put message 'kind kind)))
    (when topic
      (setq message (zulip-state-object-put message 'topic topic)))
    (when channel-id
      (setq message
            (zulip-state-object-put
             message 'channel-id (zulip-state-normalize-id channel-id))))
    message))

(defun zulip-state-user (state user-id)
  "Return USER-ID's cached user in STATE."
  (gethash (zulip-state-normalize-id user-id) (zulip-state-users state)))

(defun zulip-state-channel (state channel-id)
  "Return CHANNEL-ID's cached channel in STATE."
  (gethash (zulip-state-normalize-id channel-id)
           (zulip-state-channels state)))

(defun zulip-state-message (state message-id)
  "Return MESSAGE-ID's cached message in STATE."
  (gethash (zulip-state-message-id message-id) (zulip-state-messages state)))

(defun zulip-state--decimal-id< (left right)
  "Compare canonical decimal IDs LEFT and RIGHT without numeric conversion."
  (let ((left (replace-regexp-in-string "\\`0+" "" left))
        (right (replace-regexp-in-string "\\`0+" "" right)))
    (setq left (if (string-empty-p left) "0" left)
          right (if (string-empty-p right) "0" right))
    (or (< (length left) (length right))
        (and (= (length left) (length right))
             (string-lessp left right)))))

(defun zulip-state--message-id< (state left right)
  "Return non-nil when message LEFT precedes RIGHT in STATE."
  (cond
   ((and (zulip-state-server-message-id-p left)
         (zulip-state-server-message-id-p right))
    (zulip-state--decimal-id< left right))
   (t
    (let* ((left-message (zulip-state-message state left))
           (right-message (zulip-state-message state right))
           (left-time (or (zulip-state-object-get left-message 'timestamp) 0))
           (right-time (or (zulip-state-object-get right-message 'timestamp) 0)))
      (if (equal left-time right-time)
          (string-lessp left right)
        (< left-time right-time))))))

(defun zulip-state--sort-message-ids (state ids)
  "Return unique IDS sorted in STATE's message order."
  (sort (delete-dups (copy-sequence (delq nil ids)))
        (lambda (left right) (zulip-state--message-id< state left right))))

(defun zulip-state-message-ids (state narrow-key)
  "Return ordered message IDs indexed for NARROW-KEY in STATE."
  (copy-sequence (gethash narrow-key (zulip-state-narrows state))))

(defun zulip-state-messages-for-narrow (state narrow-key)
  "Return STATE messages for NARROW-KEY in ascending message order."
  (delq nil
        (mapcar (lambda (id) (zulip-state-message state id))
                (zulip-state-message-ids state narrow-key))))

(defun zulip-state--message-local-id (message)
  "Return MESSAGE's normalized optimistic local ID, or nil."
  (when-let* ((id (or (zulip-state-object-get message 'local-id)
                      (zulip-state-object-get message 'local_message_id))))
    (zulip-state-normalize-id id)))

(defun zulip-state--replace-index-id (state old-id new-id)
  "Replace OLD-ID by NEW-ID in every STATE index."
  (maphash
   (lambda (key ids)
     (when (member old-id ids)
       (puthash key
                (zulip-state--sort-message-ids
                 state (cons new-id (delete old-id (copy-sequence ids))))
                (zulip-state-narrows state))))
   (zulip-state-narrows state))
  (when (gethash old-id (zulip-state-unread state))
    (remhash old-id (zulip-state-unread state))
    (puthash new-id t (zulip-state-unread state)))
  (when (gethash old-id (zulip-state-unread-counted state))
    (remhash old-id (zulip-state-unread-counted state))
    (puthash new-id t (zulip-state-unread-counted state)))
  (when-let* ((details (gethash old-id (zulip-state-unread-details state))))
    (remhash old-id (zulip-state-unread-details state))
    (puthash new-id details (zulip-state-unread-details state)))
  (when (gethash old-id (zulip-state-unread-mentions state))
    (remhash old-id (zulip-state-unread-mentions state))
    (puthash new-id t (zulip-state-unread-mentions state)))
  (maphash
   (lambda (_key conversation)
     (let ((ids (zulip-dm-conversation-message-ids conversation)))
       (when (member old-id ids)
         (setf (zulip-dm-conversation-message-ids conversation)
               (zulip-state--sort-message-ids
                state (cons new-id (delete old-id (copy-sequence ids))))))))
   (zulip-state-dm-conversations state)))

(defun zulip-state--flags (message)
  "Return MESSAGE's flags as strings."
  (mapcar (lambda (flag) (format "%s" flag))
          (zulip-state--as-list
           (zulip-state-object-get message 'flags))))

(defun zulip-state--json-true-p (value)
  "Return non-nil only when JSON VALUE represents true."
  (and value
       (not (memq value '(:false :json-false json-false false)))))

(defun zulip-state--topic-key (channel-id topic)
  "Return a case-folded user-topic key for CHANNEL-ID and TOPIC."
  (and channel-id topic
       (cons (zulip-state-normalize-id channel-id)
             (downcase (format "%s" topic)))))

(defun zulip-state--user-topic-policy (state channel-id topic)
  "Return STATE's visibility policy for CHANNEL-ID and TOPIC."
  (when-let* ((entry (gethash (zulip-state--topic-key channel-id topic)
                              (zulip-state-user-topics state))))
    (zulip-state-object-get entry 'visibility_policy)))

(defun zulip-state--channel-muted-p (state channel-id)
  "Return non-nil when CHANNEL-ID is muted in STATE."
  (when-let* ((channel (zulip-state-channel state channel-id)))
    (cond
     ((zulip-state-object-has-key-p channel 'is_muted)
      (zulip-state--json-true-p
       (zulip-state-object-get channel 'is_muted)))
     ((zulip-state-object-has-key-p channel 'in_home_view)
      (not (zulip-state--json-true-p
            (zulip-state-object-get channel 'in_home_view)))))))

(defun zulip-state--unread-details-from-object (state object)
  "Return canonical unread context described by OBJECT in STATE."
  (let* ((object (zulip-state-normalize-object object))
         (kind (zulip-state--message-kind object))
         (channel-id
          (or (zulip-state-object-get object 'channel-id)
              (zulip-state-object-get object 'new_stream_id)
              (zulip-state-object-get object 'stream_id)))
         (topic (or (zulip-state-object-get object 'topic)
                    (zulip-state-object-get object 'subject)))
         details)
    (when kind
      (push (cons 'kind kind) details))
    (when channel-id
      (push (cons 'channel-id (zulip-state-normalize-id channel-id)) details)
      (unless kind (push (cons 'kind 'channel) details)))
    (when (not (null topic))
      (push (cons 'topic topic) details))
    (when (eq kind 'direct)
      (when-let* ((participants (zulip-state--dm-participant-ids state object)))
        (push (cons 'participant-ids participants) details)))
    (nreverse details)))

(defun zulip-state--unread-counted-context-p (state details)
  "Return whether unread DETAILS contributes to STATE's displayed count."
  (pcase (zulip-state-object-get details 'kind)
    ('direct t)
    ('channel
     (let* ((channel-id (zulip-state-object-get details 'channel-id))
            (topic (zulip-state-object-get details 'topic))
            (policy (zulip-state--user-topic-policy state channel-id topic))
            (channel-muted-p
             (zulip-state--channel-muted-p state channel-id)))
       (if channel-muted-p
           (memq policy '(2 3))
         (not (eq policy 1)))))
    ;; Unknown legacy details are treated as countable.  Modern mark-unread
    ;; events and every message event provide enough context to avoid this.
    (_ t)))

(defun zulip-state--set-unread-counted! (state message-id counted-p)
  "Set whether unread MESSAGE-ID contributes to STATE's count."
  (let* ((table (zulip-state-unread-counted state))
         (present (and (gethash message-id table) t)))
    (cond
     ((and counted-p (not present))
      (puthash message-id t table)
      (setf (zulip-state-unread-count state)
            (1+ (or (zulip-state-unread-count state) 0))))
     ((and (not counted-p) present)
      (remhash message-id table)
      (setf (zulip-state-unread-count state)
            (max 0 (1- (or (zulip-state-unread-count state) 0))))))))

(defun zulip-state--set-unread! (state message-id unread-p
                                       &optional counted-p counted-known-p)
  "Set MESSAGE-ID unread status in reducer-owned STATE.

When COUNTED-KNOWN-P is non-nil, COUNTED-P says whether this unread contributes
to the displayed count.  Otherwise the contribution is derived from the
stored unread context."
  (let* ((table (zulip-state-unread state))
         (present (and (gethash message-id table) t)))
    (cond
     (unread-p
      (unless present (puthash message-id t table))
      (zulip-state--set-unread-counted!
       state message-id
       (if counted-known-p
           counted-p
         (zulip-state--unread-counted-context-p
          state (gethash message-id (zulip-state-unread-details state))))))
     ((not unread-p)
      (when (gethash message-id (zulip-state-unread-counted state))
        (zulip-state--set-unread-counted! state message-id nil))
      (remhash message-id table)
      (remhash message-id (zulip-state-unread-details state))
      (remhash message-id (zulip-state-unread-mentions state))))))

(defun zulip-state--remember-unread-details! (state message-id object)
  "Merge OBJECT's unread context for MESSAGE-ID into reducer-owned STATE."
  (let* ((incoming (zulip-state--unread-details-from-object state object))
         (current (gethash message-id (zulip-state-unread-details state)))
         (details (zulip-state--merge-objects current incoming)))
    (when details
      (puthash message-id details (zulip-state-unread-details state))
      (when (gethash message-id (zulip-state-unread state))
        (zulip-state--set-unread-counted!
         state message-id
         (zulip-state--unread-counted-context-p state details))))
    details))

(defun zulip-state--mention-flags-p (state flags details)
  "Return whether unread FLAGS put DETAILS in STATE's mention index."
  (or (member "mentioned" flags)
      (and (seq-some
            (lambda (flag)
              (member flag '("wildcard_mentioned"
                             "stream_wildcard_mentioned"
                             "topic_wildcard_mentioned")))
            flags)
           (zulip-state--unread-counted-context-p state details))))

(defun zulip-state--set-unread-mention! (state message-id mentioned-p)
  "Set MESSAGE-ID membership in STATE's unread mention index."
  (if (and mentioned-p (gethash message-id (zulip-state-unread state)))
      (puthash message-id t (zulip-state-unread-mentions state))
    (remhash message-id (zulip-state-unread-mentions state))))

(defun zulip-state--sync-flags! (state message-id flags)
  "Synchronize STATE indices for MESSAGE-ID from personal FLAGS."
  (let* ((flags (mapcar (lambda (flag) (format "%s" flag))
                        (zulip-state--as-list flags)))
         (unread-p (not (member "read" flags))))
    (zulip-state--set-unread! state message-id unread-p)
    (zulip-state--set-unread-mention!
     state message-id
     (and unread-p
          (zulip-state--mention-flags-p
           state flags (gethash message-id
                                (zulip-state-unread-details state)))))))

(defun zulip-state-unread-message-p (state message-id)
  "Return non-nil when MESSAGE-ID is unread in STATE."
  (and (gethash (zulip-state-message-id message-id)
                (zulip-state-unread state))
       t))

(defun zulip-state-set-message-unread (state message-id unread-p
                                             &optional details)
  "Return STATE with MESSAGE-ID unread status set to UNREAD-P.

DETAILS is the optional `message_details' object from a mark-unread event."
  (let ((next (zulip-state-copy state)))
    (let* ((id (zulip-state-message-id message-id))
           (details (and details (zulip-state-normalize-object details)))
           (count-known-p
            (and details
                 (zulip-state-object-has-key-p details
                                               'unmuted_stream_msg)))
           (counted-p
            (and count-known-p
                 (zulip-state--json-true-p
                  (zulip-state-object-get details 'unmuted_stream_msg)))))
      (when details
        (zulip-state--remember-unread-details! next id details))
      (zulip-state--set-unread!
       next id unread-p counted-p count-known-p)
      (when (and unread-p details)
        (zulip-state--set-unread-mention!
         next id (and details
                      (zulip-state--json-true-p
                       (zulip-state-object-get details 'mentioned))))
        (when-let* ((context (gethash id (zulip-state-unread-details next)))
                    ((eq (zulip-state-object-get context 'kind) 'direct))
                    (participants
                     (zulip-state-object-get context 'participant-ids)))
          (zulip-state--touch-dm-conversation!
           next participants id)))
      next)))

(defun zulip-state-sync-message-flags (state message-id flags
                                             &optional details)
  "Return STATE with MESSAGE-ID synchronized from FLAGS and optional DETAILS."
  (let* ((next (zulip-state-copy state))
         (id (zulip-state-message-id message-id)))
    (when details
      (zulip-state--remember-unread-details! next id details))
    (zulip-state--sync-flags! next id flags)
    next))

(defun zulip-state-update-unread-context (state message-id patch)
  "Return STATE with unread MESSAGE-ID's context updated by PATCH."
  (let ((id (zulip-state-message-id message-id)))
    (if (not (gethash id (zulip-state-unread state)))
        state
      (let ((next (zulip-state-copy state)))
        (zulip-state--remember-unread-details! next id patch)
        next))))

(defun zulip-state--refresh-unread-counted! (state)
  "Recompute known unread count contributions in reducer-owned STATE.

The register wire count remains the authority for unread IDs omitted because
`old_unreads_missing' is true; only locally known contributions are changed."
  (maphash
   (lambda (id _present)
     (zulip-state--set-unread-counted!
      state id
      (zulip-state--unread-counted-context-p
       state (gethash id (zulip-state-unread-details state)))))
   (zulip-state-unread state)))

(defun zulip-state--dm-participant-ids (state message)
  "Return exact DM participant IDs for MESSAGE in STATE."
  (when (eq (zulip-state--message-kind message) 'direct)
    (let ((recipients
           (zulip-state--as-list
            (or (zulip-state-object-get message 'display_recipient)
                (zulip-state-object-get message 'recipients)
                ;; Optimistic clients and mark-unread `message_details' can
                ;; provide IDs without constructing display-recipient objects.
                (zulip-state-object-get message 'participant_ids)
                (zulip-state-object-get message 'recipient_ids)
                (zulip-state-object-get message 'user_ids))))
          ids)
      (dolist (recipient recipients)
        (when-let* ((id (if (or (hash-table-p recipient)
                                (and (consp recipient) (consp (car recipient))))
                            (zulip-state-object-get recipient 'id)
                          recipient)))
          (push (zulip-state-normalize-id id) ids)))
      (dolist (id (list (zulip-state-object-get message 'sender_id)
                        (zulip-state-self-user-id state)))
        (when id (push (zulip-state-normalize-id id) ids)))
      (sort (delete-dups ids)
            (lambda (left right)
              (cond ((and (zulip-state-server-message-id-p left)
                          (zulip-state-server-message-id-p right))
                     (zulip-state--decimal-id< left right))
                    (t (string-lessp left right))))))))

(defun zulip-state-dm-key (participant-ids)
  "Return stable DM conversation key for exact PARTICIPANT-IDS."
  (cons 'direct
        (sort (delete-dups
               (mapcar #'zulip-state-normalize-id participant-ids))
              (lambda (left right)
                (cond ((and (zulip-state-server-message-id-p left)
                            (zulip-state-server-message-id-p right))
                       (zulip-state--decimal-id< left right))
                      (t (string-lessp left right)))))))

(defun zulip-state-dm-conversation (state participant-ids-or-key)
  "Return STATE's conversation for PARTICIPANT-IDS-OR-KEY."
  (let ((key (if (eq (car-safe participant-ids-or-key) 'direct)
                 participant-ids-or-key
               (zulip-state-dm-key participant-ids-or-key))))
    (gethash key (zulip-state-dm-conversations state))))

(defun zulip-state--remove-from-dm! (state message)
  "Remove MESSAGE from its DM conversation in reducer-owned STATE."
  (when-let* ((participants (zulip-state--dm-participant-ids state message))
              (key (zulip-state-dm-key participants))
              (conversation (gethash key (zulip-state-dm-conversations state)))
              (id (zulip-state-message-id message)))
    (setf (zulip-dm-conversation-message-ids conversation)
          (delete id (copy-sequence
                      (zulip-dm-conversation-message-ids conversation))))
    ;; A zero-sized local cache is only a lower bound on the server history.
    ;; Keep delivered conversations carrying a last-known max ID; the caller
    ;; may later refresh that value from the server after a deletion.
    (when (and (null (zulip-dm-conversation-message-ids conversation))
               (null (zulip-dm-conversation-max-message-id conversation)))
      (remhash key (zulip-state-dm-conversations state)))))

(defun zulip-state--newer-message-id (current candidate)
  "Return the later server ID of CURRENT and CANDIDATE."
  (cond
   ((not (zulip-state-server-message-id-p candidate)) current)
   ((not (zulip-state-server-message-id-p current)) candidate)
   ((zulip-state--decimal-id< current candidate) candidate)
   (t current)))

(defun zulip-state--touch-dm-conversation!
    (state participants message-id &optional cached-p)
  "Record a DM with PARTICIPANTS and MESSAGE-ID in reducer-owned STATE.

When CACHED-P is non-nil, MESSAGE-ID is also part of the local message cache."
  (when participants
    (let* ((key (zulip-state-dm-key participants))
           (conversation
            (or (gethash key (zulip-state-dm-conversations state))
                (zulip-dm-conversation--create
                 :key key :participant-ids (cdr key) :message-ids nil))))
      (when cached-p
        (setf (zulip-dm-conversation-message-ids conversation)
              (zulip-state--sort-message-ids
               state
               (cons message-id
                     (zulip-dm-conversation-message-ids conversation)))))
      (setf (zulip-dm-conversation-max-message-id conversation)
            (zulip-state--newer-message-id
             (zulip-dm-conversation-max-message-id conversation)
             message-id))
      (puthash key conversation (zulip-state-dm-conversations state))
      conversation)))

(defun zulip-state--add-to-dm! (state message)
  "Add MESSAGE to its DM conversation in reducer-owned STATE."
  (when-let* ((participants (zulip-state--dm-participant-ids state message))
              (id (zulip-state-message-id message)))
    (zulip-state--touch-dm-conversation! state participants id t)))

(defun zulip-state--direct-narrow-ids (state message)
  "Return MESSAGE's DM participants excluding STATE's own user."
  (let* ((self (zulip-state-self-user-id state))
         (participants
          (copy-sequence
           (or (zulip-state--dm-participant-ids state message) nil)))
         (others (delete self (copy-sequence participants))))
    ;; Zulip encodes a self-DM narrow with the current user's ID, not an empty
    ;; operand.  Normal DMs continue to exclude the current user.
    (if (and self (null others) (member self participants))
        (list self)
      others)))

(defun zulip-state--narrow-dm-operands (operand)
  "Return normalized participant IDs encoded by DM narrow OPERAND."
  (let ((values
         (cond
          ((vectorp operand) (append operand nil))
          ((listp operand) operand)
          ((stringp operand) (split-string operand "," t "[[:space:]]+"))
          ((null operand) nil)
          (t (list operand)))))
    (sort (delete-dups (mapcar #'zulip-state-normalize-id values))
          (lambda (left right)
            (cond ((and (zulip-state-server-message-id-p left)
                        (zulip-state-server-message-id-p right))
                   (zulip-state--decimal-id< left right))
                  (t (string-lessp left right)))))))

(defun zulip-state--narrow-term-result (state message term)
  "Return t, nil, or `unknown' for MESSAGE matching TERM in STATE."
  (let* ((operator (downcase (format "%s" (car-safe term))))
         (operand (if (consp term) (cdr term) nil))
         (operand (if (and (listp operand) (= (length operand) 1))
                      (car operand)
                    operand))
         (kind (zulip-state--message-kind message)))
    (pcase operator
      ((or "channel" "stream")
       (let ((channel-id (or (zulip-state-object-get message 'channel-id)
                             (zulip-state-object-get message 'stream_id)))
             (display (zulip-state-object-get message 'display_recipient)))
         (or (equal (format "%s" channel-id) (format "%s" operand))
             (and (stringp display)
                  (zulip-state--string-equal-ignore-case
                   display (format "%s" operand))))))
      ("topic"
       (zulip-state--string-equal-ignore-case
        (format "%s" (or (zulip-state-object-get message 'topic)
                         (zulip-state-object-get message 'subject)
                         ""))
        (format "%s" operand)))
      ("sender"
       (or (equal (format "%s" (zulip-state-object-get message 'sender_id))
                  (format "%s" operand))
           (zulip-state--string-equal-ignore-case
            (format "%s" (or (zulip-state-object-get message 'sender_email) ""))
            (format "%s" operand))))
      ((or "dm" "direct")
       (and (eq kind 'direct)
            (or (null operand)
                (equal
                 (zulip-state--direct-narrow-ids state message)
                 (zulip-state--narrow-dm-operands operand)))))
      ("id" (equal (zulip-state-message-id message)
                   (zulip-state-normalize-id operand)))
      ("is"
       (pcase (downcase (format "%s" operand))
         ((or "dm" "private") (eq kind 'direct))
         ("starred" (member "starred" (zulip-state--flags message)))
         ("mentioned" (or (member "mentioned" (zulip-state--flags message))
                          (member "wildcard_mentioned"
                                  (zulip-state--flags message))))
         (_ 'unknown)))
      (_ 'unknown))))

(defun zulip-state--message-narrow-result (state message narrow-key)
  "Return t, nil, or `unknown' for MESSAGE in STATE NARROW-KEY."
  (cond
   ((or (null narrow-key) (eq narrow-key 'all)
        (equal narrow-key '(all)))
    t)
   ((equal narrow-key (zulip-state-object-get message 'narrow-key)) t)
   ((and (listp narrow-key) (eq (car narrow-key) 'channel))
    (zulip-state--narrow-term-result
     state message (cons 'channel (cadr narrow-key))))
   ((and (listp narrow-key) (eq (car narrow-key) 'topic))
    (let ((channel-match
           (zulip-state--narrow-term-result
            state message (cons 'channel (cadr narrow-key))))
          (topic-match
           (zulip-state--narrow-term-result
            state message (cons 'topic (caddr narrow-key)))))
      (and channel-match topic-match)))
   ((and (listp narrow-key) (eq (car narrow-key) 'direct))
    (and (eq (zulip-state--message-kind message) 'direct)
         (equal (zulip-state--direct-narrow-ids state message)
                (zulip-state--narrow-dm-operands (cdr narrow-key)))))
   ((and (listp narrow-key) (cl-every #'consp narrow-key))
    (let ((result t))
      (dolist (term narrow-key)
        (pcase (zulip-state--narrow-term-result state message term)
          ('nil (setq result nil))
          ('unknown (when result (setq result 'unknown)))))
      result))
   (t 'unknown)))

(defun zulip-state--reindex-message! (state message &optional explicit-keys)
  "Reindex MESSAGE in reducer-owned STATE, forcing membership in EXPLICIT-KEYS."
  (let ((id (zulip-state-message-id message)))
    (maphash
     (lambda (key ids)
       (let ((match (if (member key explicit-keys)
                        t
                      (zulip-state--message-narrow-result state message key))))
         (cond
          ((eq match t)
           (puthash key (zulip-state--sort-message-ids state (cons id ids))
                    (zulip-state-narrows state)))
          ((null match)
           (puthash key (delete id (copy-sequence ids))
                    (zulip-state-narrows state))))))
     (zulip-state-narrows state))
    (dolist (key explicit-keys)
      (puthash key
               (zulip-state--sort-message-ids
                state (cons id (gethash key (zulip-state-narrows state))))
               (zulip-state-narrows state)))))

(defun zulip-state-upsert-message (state message &optional narrow-keys)
  "Return STATE with MESSAGE inserted or replaced.

NARROW-KEYS is a list of indices in which MESSAGE is known to belong.  A
server message carrying `local-id' or `local_message_id' atomically replaces
the corresponding `local-*' placeholder in every index."
  (let* ((next (zulip-state-copy state))
         (incoming (zulip-state-normalize-message message))
         (id (zulip-state-message-id incoming))
         (local-id (zulip-state--message-local-id incoming))
         (local-message (and local-id (zulip-state-message next local-id)))
         (server-message (zulip-state-message next id))
         (merged (zulip-state--merge-objects local-message incoming)))
    (when (and local-id (not (equal local-id id)))
      (when local-message
        (zulip-state--remove-from-dm! next local-message))
      (remhash local-id (zulip-state-messages next))
      (zulip-state--replace-index-id next local-id id))
    ;; If an authoritative server event won the race, a later response built
    ;; from the pending row may only fill missing fields, never regress it.
    (when (and server-message local-id
               (zulip-state-server-message-id-p id))
      (setq merged (zulip-state--merge-objects merged server-message)))
    (unless (and server-message local-id
                 (zulip-state-server-message-id-p id))
      (setq merged (zulip-state--merge-objects server-message merged)))
    (setq merged (zulip-state-object-put merged 'id id))
    (when (and local-id (zulip-state-server-message-id-p id))
      (setq merged (zulip-state-object-put merged 'local-id local-id)
            merged (zulip-state-object-put merged 'pending nil)))
    (when server-message
      (zulip-state--remove-from-dm! next server-message))
    (puthash id merged (zulip-state-messages next))
    (zulip-state--remember-unread-details! next id merged)
    (when (zulip-state-object-has-key-p incoming 'flags)
      (zulip-state--sync-flags! next id (zulip-state--flags incoming)))
    (zulip-state--reindex-message! next merged narrow-keys)
    (zulip-state--add-to-dm! next merged)
    next))

(defalias 'zulip-state-put-message #'zulip-state-upsert-message)

(defun zulip-state-index-message (state message-or-id narrow-key)
  "Return STATE with MESSAGE-OR-ID indexed under NARROW-KEY."
  (if (or (hash-table-p message-or-id)
          (and (consp message-or-id) (consp (car message-or-id))))
      (zulip-state-upsert-message state message-or-id (list narrow-key))
    (let* ((next (zulip-state-copy state))
           (id (zulip-state-message-id message-or-id)))
      (when (zulip-state-message next id)
        (puthash narrow-key
                 (zulip-state--sort-message-ids
                  next (cons id (gethash narrow-key
                                         (zulip-state-narrows next))))
                 (zulip-state-narrows next)))
      next)))

(defun zulip-state-merge-messages (state messages narrow-key)
  "Return STATE with MESSAGES upserted and indexed for NARROW-KEY."
  (let ((next state))
    (dolist (message (zulip-state--as-list messages))
      (setq next (zulip-state-upsert-message next message (list narrow-key))))
    next))

(defun zulip-state-replace-narrow-messages (state messages narrow-key)
  "Return STATE with NARROW-KEY's exact index replaced by MESSAGES."
  (let* ((next (zulip-state-merge-messages state messages narrow-key))
         (ids (mapcar #'zulip-state-message-id
                      (zulip-state--as-list messages))))
    (puthash narrow-key (zulip-state--sort-message-ids next ids)
             (zulip-state-narrows next))
    next))

(defun zulip-state-delete-message (state message-id)
  "Return STATE without MESSAGE-ID or any of its index entries."
  (let* ((next (zulip-state-copy state))
         (id (zulip-state-message-id message-id))
         (message (zulip-state-message next id)))
    (when message
      (zulip-state--remove-from-dm! next message))
    (remhash id (zulip-state-messages next))
    (maphash (lambda (key ids)
               (puthash key (delete id (copy-sequence ids))
                        (zulip-state-narrows next)))
             (zulip-state-narrows next))
    (zulip-state--set-unread! next id nil)
    (remhash id (zulip-state-unread-mentions next))
    next))

(defun zulip-state-update-message (state message-id patch)
  "Return STATE with cached MESSAGE-ID updated by PATCH."
  (let* ((id (zulip-state-message-id message-id))
         (message (zulip-state-message state id)))
    (if (null message)
        state
      (let ((patch (zulip-state-normalize-object patch)))
        (when (zulip-state-object-has-key-p patch 'subject)
          (setq patch
                (zulip-state-object-put
                 patch 'topic (zulip-state-object-get patch 'subject))))
        (when (zulip-state-object-has-key-p patch 'new_stream_id)
          (let* ((channel-id
                  (zulip-state-normalize-id
                   (zulip-state-object-get patch 'new_stream_id)))
                 (channel (zulip-state-channel state channel-id))
                 (channel-name (and channel
                                    (zulip-state-object-get channel 'name))))
            ;; `stream_id' in an update event is the pre-edit channel;
            ;; `new_stream_id' is the post-edit value cached by messages.
            (setq patch
                  (zulip-state-object-put patch 'channel-id channel-id)
                  patch
                  (zulip-state-object-put patch 'stream_id channel-id)
                  patch
                  ;; Clearing an unknown new name is safer than retaining the
                  ;; pre-edit display recipient and matching the old narrow.
                  (zulip-state-object-put
                   patch 'display_recipient channel-name))))
        (when (zulip-state-object-has-key-p patch 'rendered_content)
          (when (zulip-state-object-has-key-p patch 'content)
            (setq patch
                  (zulip-state-object-put
                   patch 'raw-content
                   (zulip-state-object-get patch 'content))))
          (setq patch
                (zulip-state-object-put
                 patch 'content
                 (zulip-state-object-get patch 'rendered_content))))
        ;; Event type identifies the envelope, not the message's channel/DM type.
        (setq patch
              (cl-remove-if
               (lambda (entry)
                 (memq (car entry)
                       '(type id message_id message_ids local_message_id)))
               patch))
        (zulip-state-upsert-message
         state (zulip-state--merge-objects message patch))))))

(defun zulip-state--normalize-user (user)
  "Return normalized USER with an opaque string ID."
  (let* ((user (zulip-state-normalize-object user))
         (id (or (zulip-state-object-get user 'user_id)
                 (zulip-state-object-get user 'id))))
    (when id
      (setq user (zulip-state-object-put user 'id
                                         (zulip-state-normalize-id id))))
    user))

(defun zulip-state--normalize-channel (channel &optional subscribed-p)
  "Return normalized CHANNEL, optionally recording SUBSCRIBED-P."
  (let* ((channel (zulip-state-normalize-object channel))
         (id (or (zulip-state-object-get channel 'stream_id)
                 (zulip-state-object-get channel 'id))))
    (when id
      (setq channel
            (zulip-state-object-put channel 'id
                                    (zulip-state-normalize-id id))))
    (when (not (null subscribed-p))
      (setq channel
            (zulip-state-object-put channel 'subscribed subscribed-p)))
    channel))

(defun zulip-state--put-user! (state user)
  "Insert USER into reducer-owned STATE."
  (let* ((user (zulip-state--normalize-user user))
         (id (zulip-state-object-get user 'id)))
    (when id (puthash id user (zulip-state-users state)))))

(defun zulip-state--put-channel! (state channel subscribed-p)
  "Insert CHANNEL into reducer-owned STATE with SUBSCRIBED-P."
  (let* ((channel (zulip-state--normalize-channel channel subscribed-p))
         (id (zulip-state-object-get channel 'id)))
    (when id
      (puthash id channel (zulip-state-channels state))
      (if subscribed-p
          (puthash id channel (zulip-state-subscriptions state))
        (remhash id (zulip-state-subscriptions state))))))

(defun zulip-state--put-user-topic! (state raw-topic)
  "Store RAW-TOPIC visibility metadata in reducer-owned STATE."
  (let* ((topic (zulip-state-normalize-object raw-topic))
         (channel-id (zulip-state-object-get topic 'stream_id))
         (topic-name (zulip-state-object-get topic 'topic_name))
         (policy (zulip-state-object-get topic 'visibility_policy))
         (key (zulip-state--topic-key channel-id topic-name)))
    (when key
      (if (or (null policy) (eq policy 0))
          (remhash key (zulip-state-user-topics state))
        (setq topic
              (zulip-state-object-put
               topic 'stream_id (zulip-state-normalize-id channel-id)))
        (puthash key topic (zulip-state-user-topics state))))))

(defun zulip-state--comma-separated-ids (value)
  "Return normalized IDs encoded by comma-separated VALUE."
  (mapcar #'zulip-state-normalize-id
          (if (stringp value)
              (split-string value "," t "[[:space:]]+")
            (zulip-state--as-list value))))

(defun zulip-state--snapshot-unread-records (state unread)
  "Return a table mapping UNREAD IDs to wire context using STATE."
  (let ((records (zulip-state--new-table)))
    (cl-labels
        ((record (id details)
           (when id
             (let* ((id (zulip-state-message-id id))
                    (old (gethash id records)))
               (puthash id (zulip-state--merge-objects old details) records)))))
      (dolist (group (zulip-state--as-list
                      (zulip-state-object-get unread 'pms)))
        (if (or (hash-table-p group)
                (and (consp group) (consp (car group))))
            (let ((other-id
                   (or (zulip-state-object-get group 'other_user_id)
                       (zulip-state-object-get group 'sender_id))))
              (dolist (id (zulip-state--as-list
                           (or (zulip-state-object-get
                                group 'unread_message_ids)
                               (zulip-state-object-get group 'message_ids))))
                (record id
                        `((kind . direct)
                          (participant-ids
                           . ,(delq nil
                                    (list (zulip-state-self-user-id state)
                                          (and other-id
                                               (zulip-state-normalize-id
                                                other-id)))))))))
          ;; Legacy servers exposed bare PM message IDs.
          (record group nil)))
      (dolist (group (zulip-state--as-list
                      (zulip-state-object-get unread 'streams)))
        (let ((channel-id (zulip-state-object-get group 'stream_id))
              (topic (zulip-state-object-get group 'topic)))
          (dolist (id (zulip-state--as-list
                       (or (zulip-state-object-get group 'unread_message_ids)
                           (zulip-state-object-get group 'message_ids))))
            (record id `((kind . channel)
                         (channel-id
                          . ,(zulip-state-normalize-id channel-id))
                         (topic . ,topic))))))
      (dolist (group (zulip-state--as-list
                      (zulip-state-object-get unread 'huddles)))
        (let ((participants
               (zulip-state--comma-separated-ids
                (or (zulip-state-object-get group 'user_ids_string)
                    (zulip-state-object-get group 'user_ids)))))
          (dolist (id (zulip-state--as-list
                       (or (zulip-state-object-get group 'unread_message_ids)
                           (zulip-state-object-get group 'message_ids))))
            (record id `((kind . direct)
                         (participant-ids . ,participants))))))
      (dolist (id (zulip-state--as-list
                   (zulip-state-object-get unread 'mentions)))
        (record id nil)))
    records))

(defun zulip-state--snapshot-unread-ids (unread)
  "Return exact message IDs listed in register UNREAD data."
  (let ((state (zulip-state-create)) ids)
    (maphash (lambda (id _details) (push id ids))
             (zulip-state--snapshot-unread-records state unread))
    ids))

(defun zulip-state-from-register (register-data)
  "Build a fresh state from REGISTER-DATA.

The returned snapshot shares no mutable index with a prior account state and
is therefore suitable for one atomic account/Appkit state replacement."
  (let* ((data (zulip-state-normalize-object register-data))
         (state (zulip-state-create))
         (self-id (or (zulip-state-object-get data 'user_id)
                      (zulip-state-object-get data 'self_user_id)))
         (unread (zulip-state-object-get data 'unread_msgs)))
    (setf (zulip-state-self-user-id state)
          (zulip-state-normalize-id self-id)
          (zulip-state-register-data state) data)
    (dolist (user (append
                   (zulip-state--as-list
                    (zulip-state-object-get data 'realm_users))
                   (zulip-state--as-list
                    (zulip-state-object-get data 'users))))
      (zulip-state--put-user! state user))
    (dolist (channel (zulip-state--as-list
                      (zulip-state-object-get data 'subscriptions)))
      (zulip-state--put-channel! state channel t))
    (dolist (channel (append
                      (zulip-state--as-list
                       (zulip-state-object-get data 'unsubscribed))
                      (zulip-state--as-list
                       (zulip-state-object-get data 'streams))))
      (let ((id (or (zulip-state-object-get channel 'stream_id)
                    (zulip-state-object-get channel 'id))))
        (unless (and id (zulip-state-channel state id))
          (zulip-state--put-channel! state channel nil))))
    (dolist (topic (zulip-state--as-list
                    (zulip-state-object-get data 'user_topics)))
      (zulip-state--put-user-topic! state topic))
    (dolist (message (zulip-state--as-list
                      (zulip-state-object-get data 'messages)))
      (setq state (zulip-state-upsert-message state message)))
    (dolist (recent
             (zulip-state--as-list
              (zulip-state-object-get data 'recent_private_conversations)))
      (let* ((participants
              (zulip-state--as-list
               (or (zulip-state-object-get recent 'user_ids)
                   (zulip-state-object-get recent 'participant_ids))))
             (participants
              (delete-dups
               (delq nil
                     (mapcar #'zulip-state-normalize-id
                             (cons (zulip-state-self-user-id state)
                                   participants)))))
             (max-id (zulip-state-object-get recent 'max_message_id)))
        (when participants
          (zulip-state--touch-dm-conversation!
           state participants (and max-id (zulip-state-message-id max-id))))))
    (clrhash (zulip-state-unread state))
    (clrhash (zulip-state-unread-counted state))
    (clrhash (zulip-state-unread-details state))
    (setf (zulip-state-unread-count state) 0)
    (maphash
     (lambda (id details)
       (when details
         (puthash id details (zulip-state-unread-details state)))
       (zulip-state--set-unread! state id t)
       (when (eq (zulip-state-object-get details 'kind) 'direct)
         (zulip-state--touch-dm-conversation!
          state (zulip-state-object-get details 'participant-ids) id)))
     (zulip-state--snapshot-unread-records state unread))
    (dolist (id (zulip-state--as-list
                 (zulip-state-object-get unread 'mentions)))
      (puthash (zulip-state-message-id id) t
               (zulip-state-unread-mentions state)))
    (setf (zulip-state-unread-count state)
          (or (zulip-state-object-get unread 'count)
              (hash-table-count (zulip-state-unread state))))
    state))

(defalias 'zulip-state-register-snapshot #'zulip-state-from-register)

(provide 'zulip-state)

;;; zulip-state.el ends here
