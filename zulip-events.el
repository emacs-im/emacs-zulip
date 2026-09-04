;;; zulip-events.el --- Zulip event reducer and queue loop -*- lexical-binding: t; -*-

;;; Commentary:

;; Recognized events return a new zulip-state value.  The queue controller is
;; the only layer that mutates account runtime slots.  A monotonic generation
;; makes callbacks harmless after restart, re-registration, or shutdown.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'appkit-core)
(require 'zulip-customize)
(require 'zulip-runtime)
(require 'zulip-state)
(require 'zulip-api)

(declare-function zulip-api-result-ok-p "zulip-http" result)
(declare-function zulip-api-result-data "zulip-http" result)
(declare-function zulip-api-result-code "zulip-http" result)
(declare-function zulip-http-cancel-request "zulip-http" request)
(declare-function zulip-api-register "zulip-api" account callback &optional types)
(declare-function zulip-api-get-events
                  "zulip-api" account queue-id last-event-id callback)

(defun zulip-events--type (event)
  "Return EVENT's wire type as a string."
  (let ((type (zulip-state-object-get event 'type)))
    (and type (format "%s" type))))

(defun zulip-events--ids (event)
  "Return EVENT's affected message IDs as opaque strings."
  (let ((ids (or (zulip-state-object-get event 'message_ids)
                 (zulip-state-object-get event 'messages))))
    (unless ids
      (setq ids (list (zulip-state-object-get event 'message_id))))
    (delete-dups
     (delq nil
           (mapcar (lambda (id)
                     (unless (or (hash-table-p id)
                                 (and (consp id) (consp (car id))))
                       (zulip-state-message-id id)))
                   (zulip-state--as-list ids))))))

(defun zulip-events--message (state event)
  "Reduce a message EVENT into STATE."
  (when-let* ((raw (zulip-state-object-get event 'message)))
    (let ((message (zulip-state-normalize-object raw))
          (local-id (zulip-state-object-get event 'local_message_id)))
      (setq message (zulip-state-object-put message 'authoritative t))
      (when local-id
        (setq message
              (zulip-state-object-put message 'local-id
                                      (zulip-state-normalize-id local-id))
              message
              (zulip-state-object-put message 'local_message_id
                                      (zulip-state-normalize-id local-id))))
      (when (zulip-state-object-has-key-p event 'flags)
        (setq message
              (zulip-state-object-put
               message 'flags (zulip-state-object-get event 'flags))))
      (zulip-state-upsert-message state message))))

(defun zulip-events--update-message-move-patch (event)
  "Return only EVENT fields shared by every moved message."
  (let (patch)
    (dolist (key '(subject topic_links new_stream_id))
      (when (zulip-state-object-has-key-p event key)
        (push (cons key (zulip-state-object-get event key)) patch)))
    (nreverse patch)))

(defun zulip-events--update-message (state event)
  "Reduce an update_message EVENT into STATE.

Channel/topic fields apply to every `message_ids' member.  Content and personal
flags apply only to the singular `message_id' anchor."
  (let* ((next state)
         (move-patch (zulip-events--update-message-move-patch event))
         (primary-raw (zulip-state-object-get event 'message_id))
         (primary-id (and primary-raw
                          (zulip-state-message-id primary-raw))))
    (when move-patch
      (dolist (id (zulip-events--ids event))
        ;; The unread register can contain messages absent from the cache; its
        ;; stream/topic context must move with those IDs as well.
        (setq next (zulip-state-update-unread-context next id move-patch)
              next (zulip-state-update-message next id move-patch))))
    (when primary-id
      (setq next (zulip-state-update-message next primary-id event))
      (when (and (not (zulip-state-message next primary-id))
                 (zulip-state-object-has-key-p event 'flags))
        (setq next
              (zulip-state-sync-message-flags
               next primary-id (zulip-state-object-get event 'flags)
               event))))
    next))

(defun zulip-events--delete-message (state event)
  "Reduce a legacy or bulk delete_message EVENT into STATE."
  (let ((next state))
    (dolist (id (zulip-events--ids event))
      (setq next (zulip-state-delete-message next id)))
    next))

(defun zulip-events--reaction-fields (event)
  "Return canonical reaction object represented by EVENT."
  (let ((source (or (zulip-state-object-get event 'reaction) event))
        result)
    (dolist (key '(emoji_name emoji_code reaction_type user_id user user_email))
      (when (zulip-state-object-has-key-p source key)
        (push (cons key (zulip-state-object-get source key)) result)))
    (nreverse result)))

(defun zulip-events--same-reaction-p (left right)
  "Return non-nil when reaction LEFT is identified by RIGHT."
  (cl-every
   (lambda (key)
     (or (not (zulip-state-object-has-key-p right key))
         (equal (zulip-state-object-get left key)
                (zulip-state-object-get right key))))
   '(user_id emoji_code reaction_type emoji_name)))

(defun zulip-events--reaction (state event)
  "Reduce a reaction EVENT into STATE."
  (let* ((raw-id (zulip-state-object-get event 'message_id))
         (id (and raw-id (zulip-state-message-id raw-id)))
         (message (and id (zulip-state-message state id))))
    (if (null message)
        state
      (let* ((reaction (zulip-events--reaction-fields event))
             (reactions
              (copy-sequence
               (zulip-state--as-list
                (zulip-state-object-get message 'reactions))))
             (add-p (string= (format "%s"
                                     (zulip-state-object-get event 'op))
                             "add")))
        (setq reactions
              (if add-p
                  (if (seq-some
                       (lambda (existing)
                         (zulip-events--same-reaction-p existing reaction))
                       reactions)
                      reactions
                    (append reactions (list reaction)))
                (cl-remove-if
                 (lambda (existing)
                   (zulip-events--same-reaction-p existing reaction))
                 reactions)))
        (zulip-state-update-message
         state id (list (cons 'reactions reactions)))))))

(defun zulip-events--flag-list (message)
  "Return MESSAGE flags as a list of strings."
  (mapcar (lambda (flag) (format "%s" flag))
          (zulip-state--as-list
           (zulip-state-object-get message 'flags))))

(defun zulip-events--json-true-p (value)
  "Return non-nil only when JSON VALUE represents true."
  (and value
       (not (memq value '(:false :json-false json-false false)))))

(defun zulip-events--update-message-flags (state event)
  "Reduce an update_message_flags EVENT into STATE."
  (let ((next state)
        (flag (format "%s" (zulip-state-object-get event 'flag)))
        (message-details
         (zulip-state-object-get event 'message_details))
        (add-p (string= (format "%s"
                                (or (zulip-state-object-get event 'op)
                                    (zulip-state-object-get event 'operation)))
                        "add")))
    (when (and add-p
               (string= flag "read")
               (zulip-events--json-true-p
                (zulip-state-object-get event 'all))
               (null (zulip-events--ids event)))
      (setq next (zulip-state-copy next))
      (clrhash (zulip-state-unread next))
      (clrhash (zulip-state-unread-counted next))
      (clrhash (zulip-state-unread-details next))
      (clrhash (zulip-state-unread-mentions next))
      (setf (zulip-state-unread-count next) 0))
    (dolist (id (zulip-events--ids event))
      (let ((details (and message-details
                          (zulip-state-object-get message-details id))))
        (if-let* ((message (zulip-state-message next id)))
            (let ((flags (zulip-events--flag-list message)))
              (setq flags
                    (if add-p
                        (cons flag (delete flag flags))
                      (delete flag flags))
                    next
                    (zulip-state-update-message
                     next id (list (cons 'flags flags)))))
          (when (string= flag "read")
            (setq next
                  (zulip-state-set-message-unread
                   next id (not add-p) (and (not add-p) details)))))))
    next))

(defun zulip-events--subscription-objects (event)
  "Return subscription objects carried by EVENT."
  (let ((objects (or (zulip-state-object-get event 'subscriptions)
                     (zulip-state-object-get event 'streams)
                     (zulip-state-object-get event 'subscription))))
    (if objects (zulip-state--as-list objects) nil)))

(defun zulip-events--subscription (state event)
  "Reduce a subscription EVENT into STATE."
  (let* ((next (zulip-state-copy state))
         (operation (format "%s" (zulip-state-object-get event 'op)))
         (objects (zulip-events--subscription-objects event))
         (stream-id (zulip-state-object-get event 'stream_id)))
    (when (and (null objects) stream-id)
      (setq objects (list (list (cons 'stream_id stream-id)))))
    (dolist (raw objects)
      (let* ((incoming (zulip-state--normalize-channel raw))
             (id (or (zulip-state-object-get incoming 'id)
                     (and stream-id (zulip-state-normalize-id stream-id))))
             (current (and id (zulip-state-channel next id)))
             (channel (zulip-state--merge-objects current incoming)))
        (when id
          (setq channel (zulip-state-object-put channel 'id id))
          (cond
           ((string= operation "remove")
            (setq channel (zulip-state-object-put channel 'subscribed nil))
            (puthash id channel (zulip-state-channels next))
            (remhash id (zulip-state-subscriptions next)))
           ((string= operation "update")
            (when-let* ((property (zulip-state-object-get event 'property)))
              (setq channel
                    (zulip-state-object-put
                     channel property
                     (zulip-state-object-get event 'value))))
            (setq channel (zulip-state-object-put channel 'subscribed t))
            (puthash id channel (zulip-state-channels next))
            (puthash id channel (zulip-state-subscriptions next)))
           (t
            (setq channel (zulip-state-object-put channel 'subscribed t))
            (puthash id channel (zulip-state-channels next))
            (puthash id channel (zulip-state-subscriptions next)))))))
    (zulip-state--refresh-unread-counted! next)
    next))

(defun zulip-events--user-topic (state event)
  "Reduce a user_topic visibility EVENT into STATE."
  (let ((next (zulip-state-copy state)))
    (zulip-state--put-user-topic! next event)
    (zulip-state--refresh-unread-counted! next)
    next))

(defun zulip-events--realm-user (state event)
  "Reduce a realm_user EVENT into STATE."
  (let* ((next (zulip-state-copy state))
         (operation (format "%s" (zulip-state-object-get event 'op)))
         (raw (or (zulip-state-object-get event 'person)
                  (zulip-state-object-get event 'user)
                  event))
         (incoming (zulip-state--normalize-user raw))
         (id (or (zulip-state-object-get incoming 'id)
                 (when-let* ((user-id
                              (zulip-state-object-get event 'user_id)))
                   (zulip-state-normalize-id user-id))))
         (current (and id (zulip-state-user next id))))
    (when id
      (let ((user (zulip-state--merge-objects current incoming)))
        (setq user (zulip-state-object-put user 'id id))
        (when (string= operation "remove")
          (setq user (zulip-state-object-put user 'is_active nil)))
        (puthash id user (zulip-state-users next))))
    next))

(defun zulip-events--stream (state event)
  "Reduce a stream metadata EVENT into STATE."
  (let* ((next (zulip-state-copy state))
         (operation (format "%s" (zulip-state-object-get event 'op)))
         (objects (or (zulip-state-object-get event 'streams)
                      (zulip-state-object-get event 'stream)))
         (objects (zulip-state--as-list objects)))
    (dolist (raw objects)
      (let* ((incoming (zulip-state--normalize-channel raw))
             (id (zulip-state-object-get incoming 'id))
             (current (and id (zulip-state-channel next id))))
        (when id
          (if (string= operation "delete")
              (remhash id (zulip-state-channels next))
            (puthash id (zulip-state--merge-objects current incoming)
                     (zulip-state-channels next))))))
    next))

(defun zulip-events-reduce (state raw-event)
  "Purely reduce RAW-EVENT into STATE and return the resulting state."
  (let* ((event (zulip-state-normalize-object raw-event))
         (type (zulip-events--type event)))
    (pcase type
      ("message" (or (zulip-events--message state event) state))
      ("update_message" (zulip-events--update-message state event))
      ("delete_message" (zulip-events--delete-message state event))
      ("reaction" (zulip-events--reaction state event))
      ("update_message_flags"
       (zulip-events--update-message-flags state event))
      ("subscription" (zulip-events--subscription state event))
      ("user_topic" (zulip-events--user-topic state event))
      ("realm_user" (zulip-events--realm-user state event))
      ("stream" (zulip-events--stream state event))
      (_ state))))

(defun zulip-events--active-p (account generation)
  "Return non-nil when ACCOUNT still owns GENERATION."
  (and (zulip-account-p account)
       (= generation (or (zulip-account-generation account) 0))
       (let ((app (zulip-account-app account)))
         (and (appkit-app-p app) (appkit-app-live-p app)))))

(defun zulip-events--result-code (result)
  "Return RESULT's stable Zulip error code."
  (and (fboundp 'zulip-api-result-code)
       (zulip-api-result-code result)))

(defun zulip-events--bad-queue-p (result)
  "Return non-nil when RESULT reports an invalid event queue."
  (member (zulip-events--result-code result)
          '("BAD_EVENT_QUEUE_ID" "BAD_EVENT_QUEUE")))

(defun zulip-events-start (account)
  "Enable ACCOUNT's App-owned registration and consecutive poll Source."
  (unless (and (zulip-account-p account)
               (appkit-app-live-p (zulip-account-app account)))
    (error "Zulip event processing requires a live account"))
  (appkit-app-send (zulip-account-app account) '(events-start))
  account)

(defun zulip-events-stop (account)
  "Revoke ACCOUNT's exact Source epoch and its transport/retry capability."
  (when (and (zulip-account-p account)
             (appkit-app-live-p (zulip-account-app account)))
    (appkit-app-send (zulip-account-app account) '(events-stop))))

(cl-defstruct (zulip-events--transport
               (:constructor zulip-events--transport-create))
  account app generation emit request request-token timer active-p)

(defun zulip-events--transport-current-p (transport)
  "Test the exact App, account, and epoch captured by TRANSPORT."
  (let ((account (zulip-events--transport-account transport)))
    (and (zulip-events--transport-active-p transport)
         (eq (zulip-events--transport-app transport) (zulip-account-app account))
         (eq transport (zulip-account-event-transport account))
         (zulip-events--active-p
          account (zulip-events--transport-generation transport)))))

(defun zulip-events--transport-cancel (transport)
  "Revoke TRANSPORT before cancelling its actual process and retry timer."
  (setf (zulip-events--transport-active-p transport) nil
        (zulip-events--transport-request-token transport) nil)
  (when-let* ((timer (zulip-events--transport-timer transport)))
    (cancel-timer timer))
  (when-let* ((request (zulip-events--transport-request transport)))
    (zulip-http-cancel-request request))
  (let ((account (zulip-events--transport-account transport)))
    (when (eq transport (zulip-account-event-transport account))
      (setf (zulip-account-event-transport account) nil
            (zulip-account-poll-process account) nil
            (zulip-account-retry-timer account) nil
            (zulip-account-retry-handle account) nil)))
  (setf (zulip-events--transport-request transport) nil
        (zulip-events--transport-timer transport) nil))

(defun zulip-events--request (transport operation)
  "Start one exact OPERATION, emitting only its first current response."
  (when (zulip-events--transport-current-p transport)
    (let* ((account (zulip-events--transport-account transport))
           (zulip-http--source-request-p t)
           (token (make-symbol "zulip-queue-request-"))
           (completed nil)
           (_ (setf (zulip-events--transport-request-token transport) token))
           (callback
            (lambda (result)
              (when (and (not completed)
                         (eq token (zulip-events--transport-request-token transport))
                         (zulip-events--transport-current-p transport))
                (setq completed t)
                (setf (zulip-events--transport-request-token transport) nil
                      (zulip-events--transport-request transport) nil
                      (zulip-account-poll-process account) nil)
                (funcall (zulip-events--transport-emit transport) operation result))))
           (request
             (pcase operation
               ('register (zulip-api-register account callback))
               ('poll
                (zulip-api-get-events account
                                      (zulip-account-queue-id account)
                                      (zulip-account-last-event-id account) callback))
               (_ (error "Unknown Zulip queue operation: %S" operation)))))
      (unless (or completed (processp request) (appkit-handle-p request))
        (error "Zulip Source returned no pending transport capability"))
      (unless completed
        (setf (zulip-events--transport-request transport) request
              (zulip-account-poll-process account) request))
      request)))

(defun zulip-events--source-start (_context input emit _closed)
  "Start one real register/poll transport after the account App is linked."
  (pcase-let ((`(,account ,app ,generation) input))
    (unless (and (eq app (zulip-account-app account))
                 (zulip-events--active-p account generation))
      (error "Zulip Source started with a stale account"))
    (let ((transport
           (zulip-events--transport-create
            :account account :app app :generation generation
            :emit emit :active-p t)))
      (setf (zulip-account-event-transport account) transport)
      (condition-case condition
          (zulip-events--request transport 'register)
        (error
         (zulip-events--transport-cancel transport)
         (signal (car condition) (cdr condition))))
      (appkit-source-cancellation-create
       :kind 'transport
       :cancel (lambda () (zulip-events--transport-cancel transport))))))

(defun zulip-events--source-outbound (_context input payload _complete)
  "Run committed queue OPERATION in PAYLOAD, preserving the existing delay."
  (pcase-let* ((`(,account ,app ,generation) input)
               (`(,operation ,retry-p) payload)
               (transport (zulip-account-event-transport account)))
    (if (not (and transport (eq app (zulip-account-app account))
                  (= generation (zulip-account-generation account))
                  (zulip-events--transport-current-p transport)))
        'stale
      (if retry-p
          (let (timer)
            (setq timer
                  (run-at-time
                   zulip-event-retry-delay nil
                   (lambda ()
                     (when (and (zulip-events--transport-current-p transport)
                                (eq timer (zulip-events--transport-timer transport)))
                       (setf (zulip-events--transport-timer transport) nil
                             (zulip-account-retry-timer account) nil)
                       (zulip-events--request transport operation)))))
            (setf (zulip-events--transport-timer transport) timer
                  (zulip-account-retry-timer account) timer))
        (zulip-events--request transport operation))
      'accepted)))

(defun zulip-events--source (account)
  "Declare ACCOUNT's current exact protocol epoch as an App Source."
  (appkit-source-spec-create
   :key 'zulip-events :identity (zulip-account-generation account)
   :input (list account (zulip-account-app account)
                (zulip-account-generation account))
   :start #'zulip-events--source-start
   :event (lambda (input operation result)
            (list 'events-result input operation result))
   :closed (lambda (input reason) (list 'events-closed input reason))
   :outbound #'zulip-events--source-outbound :outbound-pending-limit 1
   :emission-policy 'lossless :pending-limit 64
   :cancellation-requirement 'transport))

(defun zulip-events--intent-result (_payload outcome)
  "Return a protocol delivery outcome from the exact Source adapter."
  (list 'events-intent outcome))

(defun zulip-events--continue (account operation &optional retry-p)
  "Stage OPERATION after ACCOUNT's accepted state transition."
  (push (appkit-command-source-intent
         :key 'zulip-events :expected-identity (zulip-account-generation account)
         :payload (list operation retry-p)
         :result-mapper #'zulip-events--intent-result)
        zulip-runtime--commands))

(defun zulip-events--update (account message)
  "Commit Source MESSAGE into ACCOUNT and stage its next queue operation."
  (pcase message
    ((or '(events-start) '(events-stop))
     (zulip-events--begin-epoch account (eq (car message) 'events-start)))
    (`(events-result (,captured ,app ,generation) ,operation ,result)
     (when (and (eq captured account) (eq app (zulip-account-app account))
                (= generation (zulip-account-generation account))
                (zulip-account-events-enabled-p account))
       (pcase operation
         ('register
          (if (not (zulip-api-result-ok-p result))
              (zulip-events--continue account 'register t)
            (let* ((data (zulip-api-result-data result))
                   (queue-id (zulip-state-object-get data 'queue_id))
                   (timeout (zulip-state-object-get
                             data 'event_queue_longpoll_timeout_seconds)))
              (if (null queue-id)
                  (zulip-events--continue account 'register t)
                (let* ((old (zulip-account-state account))
                       (state (zulip-state-from-register data)))
                  (when (fboundp 'zulip-feed--rebase-pending)
                    (setq state (zulip-feed--rebase-pending account state)))
                  (setf (zulip-account-queue-id account) queue-id
                        (zulip-account-last-event-id account)
                        (zulip-state-object-get data 'last_event_id)
                        (zulip-account-feature-level account)
                        (zulip-state-object-get data 'zulip_feature_level)
                        (zulip-account-server-version account)
                        (zulip-state-object-get data 'zulip_version)
                        (zulip-account-longpoll-timeout account)
                        (if (and (numberp timeout) (> timeout 0))
                            timeout zulip-event-long-poll-timeout)
                        (zulip-account-connected-p account) t)
                  (zulip-runtime-publish-state account state)
                  (zulip-runtime--fanout account '((type . "register")) old state)
                  (zulip-events--continue account 'poll))))))
         ('poll
          (cond
           ((zulip-api-result-ok-p result)
            (setf (zulip-account-connected-p account) t)
            (let* ((data (zulip-api-result-data result))
                   (old (zulip-account-state account))
                   (state old)
                   accepted)
              (dolist (raw (zulip-state--as-list
                            (or (zulip-state-object-get data 'events) data)))
                (let* ((event (zulip-state-normalize-object raw))
                       (id (zulip-state-object-get event 'id))
                       (next (zulip-events-reduce state event)))
                  (when id (setf (zulip-account-last-event-id account) id))
                  (unless (eq state next)
                    (push event accepted)
                    (setq state next))))
              (unless (eq old state)
                (zulip-runtime-publish-state account state)
                (zulip-runtime--fanout
                 account (list (cons 'type "event_batch")
                               (cons 'events (nreverse accepted))) old state)))
            (zulip-events--continue account 'poll))
           ((zulip-events--bad-queue-p result)
            (zulip-events--begin-epoch account t))
           (t
            (setf (zulip-account-connected-p account) nil)
            (zulip-events--continue account 'poll t)))))))
    (`(events-closed (,captured ,app ,generation) ,_reason)
     (when (and (eq captured account) (eq app (zulip-account-app account))
                (= generation (zulip-account-generation account)))
       (setf (zulip-account-connected-p account) nil
             (zulip-account-events-enabled-p account) nil)))))

(defun zulip-events--begin-epoch (account enabled)
  "Revoke protocol-dependent work before changing ACCOUNT's Source epoch."
  (cl-incf (zulip-account-generation account))
  (setf (zulip-account-events-enabled-p account) enabled
        (zulip-account-connected-p account) nil
        (zulip-account-queue-id account) nil
        (zulip-account-last-event-id account) nil)
  (zulip-runtime--fanout account '((type . epoch))
                         (zulip-account-state account) (zulip-account-state account)))

(provide 'zulip-events)

;;; zulip-events.el ends here
