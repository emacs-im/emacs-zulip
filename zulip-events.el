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

(defalias 'zulip-event-reduce #'zulip-events-reduce)
(defalias 'zulip-state-reduce-event #'zulip-events-reduce)

(defun zulip-events--active-p (account generation)
  "Return non-nil when ACCOUNT still owns GENERATION."
  (and (zulip-account-p account)
       (= generation (or (zulip-account-generation account) 0))
       (let ((app (zulip-account-app account)))
         (and (appkit-app-p app) (appkit-app-live-p app)))))

(defun zulip-events--cancel-retry (account)
  "Cancel and forget ACCOUNT's current retry timer exactly once."
  (let ((timer (zulip-account-retry-timer account))
        (handle (zulip-account-retry-handle account)))
    (setf (zulip-account-retry-timer account) nil
          (zulip-account-retry-handle account) nil)
    (cond
     ((and (appkit-handle-p handle) (appkit-handle-alive-p handle))
      (appkit-cancel-handle handle))
     ;; A dead Appkit handle has already cancelled its timer.  The raw-timer
     ;; fallback preserves compatibility with accounts created before retry
     ;; handles were introduced and with transport test doubles.
     ((appkit-handle-p handle))
     ((timerp timer) (cancel-timer timer)))))

(defun zulip-events--cancel-inflight (account)
  "Cancel ACCOUNT's current queue request and retry timer."
  (zulip-events--cancel-retry account)
  (when-let* ((request (zulip-account-poll-process account)))
    (zulip-http-cancel-request request))
  (setf (zulip-account-poll-process account) nil))

(defalias 'zulip-events--install-state #'zulip-runtime-publish-state
  "Compatibility alias for `zulip-runtime-publish-state'.")

(defun zulip-events--change (account event old-state new-state)
  "Return ACCOUNT UI change for EVENT from OLD-STATE to NEW-STATE."
  (let* ((message (zulip-state-object-get event 'message))
         (message-id
          (or (and message (zulip-state-object-get message 'id))
              (zulip-state-object-get event 'message_id)
              (car (zulip-state--as-list
                    (zulip-state-object-get event 'message_ids)))))
         (local-id
          (or (zulip-state-object-get event 'local_message_id)
              (and message
                   (or (zulip-state-object-get message 'local-id)
                       (zulip-state-object-get message 'local_message_id))))))
    (list :type (and (zulip-events--type event)
                     (intern (zulip-events--type event)))
          :message-id (and message-id
                           (zulip-state-message-id message-id))
          :local-id (and local-id (zulip-state-normalize-id local-id))
          :account account
          :event event
          :old-state old-state
          :state new-state)))

(defun zulip-events--emit-event (account event old-state new-state)
  "Publish EVENT and its OLD-STATE to NEW-STATE transition for ACCOUNT."
  (when-let* ((app (zulip-account-app account)))
    (appkit-app-emit app 'zulip-event account event old-state new-state)
    (appkit-app-emit
     app 'zulip-state-changed
     (zulip-events--change account event old-state new-state))))

(defun zulip-events--schedule-retry (account generation operation)
  "Schedule ACCOUNT GENERATION to retry OPERATION."
  (when (zulip-events--active-p account generation)
    (let ((app (zulip-account-app account))
          timer handle)
      (zulip-events--cancel-retry account)
      (setq
       timer
       (run-at-time
        zulip-event-retry-delay nil
        (lambda ()
          ;; A cancelled or superseded timer may already be queued.  Only the
          ;; account's current handle may consume the retry operation.
          (when (and (eq timer (zulip-account-retry-timer account))
                     (eq handle (zulip-account-retry-handle account)))
            (setf (zulip-account-retry-timer account) nil
                  (zulip-account-retry-handle account) nil)
            (when (appkit-handle-p handle)
              (appkit-cancel-handle handle))
            (when (zulip-events--active-p account generation)
              (funcall operation account generation))))))
      (setq
       handle
       (appkit-register-handle
        app 'timer timer
        (lambda (owned-timer)
          (when (eq handle (zulip-account-retry-handle account))
            (setf (zulip-account-retry-timer account) nil
                  (zulip-account-retry-handle account) nil))
          (when (timerp owned-timer)
            (cancel-timer owned-timer)))))
      (setf (zulip-account-retry-timer account) timer
            (zulip-account-retry-handle account) handle)
      timer)))

(defun zulip-events--result-code (result)
  "Return RESULT's stable Zulip error code."
  (and (fboundp 'zulip-api-result-code)
       (zulip-api-result-code result)))

(defun zulip-events--bad-queue-p (result)
  "Return non-nil when RESULT reports an invalid event queue."
  (member (zulip-events--result-code result)
          '("BAD_EVENT_QUEUE_ID" "BAD_EVENT_QUEUE")))

(defun zulip-events--track-request (account request callback-ran-p)
  "Track REQUEST on ACCOUNT unless CALLBACK-RAN-P is non-nil."
  (unless callback-ran-p
    (setf (zulip-account-poll-process account) request))
  request)

(defun zulip-events--register (account generation)
  "Register ACCOUNT's event queue for GENERATION."
  (when (zulip-events--active-p account generation)
    (let ((callback-ran-p nil)
          request)
      (setq
       request
       (zulip-api-register
        account
        (lambda (result)
          (setq callback-ran-p t)
          (when (zulip-events--active-p account generation)
            (setf (zulip-account-poll-process account) nil)
            (if (zulip-api-result-ok-p result)
                (let* ((data (zulip-api-result-data result))
                       (queue-id (zulip-state-object-get data 'queue_id))
                       (last-event-id
                        (zulip-state-object-get data 'last_event_id))
                       (server-timeout
                        (zulip-state-object-get
                         data 'event_queue_longpoll_timeout_seconds))
                       (state (zulip-state-from-register data)))
                  (if (null queue-id)
                      (zulip-events--schedule-retry
                       account generation #'zulip-events--register)
                    (setf (zulip-account-queue-id account) queue-id
                          (zulip-account-last-event-id account) last-event-id
                          (zulip-account-feature-level account)
                          (zulip-state-object-get data 'zulip_feature_level)
                          (zulip-account-server-version account)
                          (zulip-state-object-get data 'zulip_version)
                          (zulip-account-longpoll-timeout account)
                          (if (and (numberp server-timeout)
                                   (> server-timeout 0))
                              server-timeout
                            zulip-event-long-poll-timeout)
                          (zulip-account-connected-p account) t)
                    (zulip-runtime-publish-state account state)
                    (when-let* ((app (zulip-account-app account)))
                      (appkit-app-emit app 'zulip-register account state))
                    (zulip-events--poll account generation)))
              (zulip-events--schedule-retry
               account generation #'zulip-events--register))))))
      (zulip-events--track-request account request callback-ran-p))))

(defun zulip-events--re-register (account generation)
  "Replace ACCOUNT's invalid queue owned by GENERATION."
  (when (zulip-events--active-p account generation)
    (zulip-events--cancel-inflight account)
    (let ((next-generation (1+ generation)))
      (setf (zulip-account-generation account) next-generation
            (zulip-account-connected-p account) nil
            (zulip-account-queue-id account) nil
            (zulip-account-last-event-id account) nil)
      (zulip-events--register account next-generation))))

(defun zulip-events--poll (account generation)
  "Issue ACCOUNT's next consecutive long-poll for GENERATION."
  (when (and (zulip-events--active-p account generation)
             (zulip-account-queue-id account))
    (let ((callback-ran-p nil)
          request)
      (setq
       request
       (zulip-api-get-events
        account
        (zulip-account-queue-id account)
        (zulip-account-last-event-id account)
        (lambda (result)
          (setq callback-ran-p t)
          (when (zulip-events--active-p account generation)
            (setf (zulip-account-poll-process account) nil)
            (cond
             ((zulip-api-result-ok-p result)
              (setf (zulip-account-connected-p account) t)
              (let* ((data (zulip-api-result-data result))
                     (events
                      (zulip-state--as-list
                       (or (zulip-state-object-get data 'events) data))))
                (dolist (raw-event events)
                  (let* ((event (zulip-state-normalize-object raw-event))
                         (event-id (zulip-state-object-get event 'id))
                         (old-state (zulip-account-state account))
                         (new-state (zulip-events-reduce old-state event)))
                    (when event-id
                      (setf (zulip-account-last-event-id account) event-id))
                    (unless (eq old-state new-state)
                      (zulip-runtime-publish-state account new-state)
                      (zulip-events--emit-event
                       account event old-state new-state))))
                (zulip-events--poll account generation)))
             ((zulip-events--bad-queue-p result)
              (zulip-events--re-register account generation))
             (t
              (setf (zulip-account-connected-p account) nil)
              (zulip-events--schedule-retry
               account generation #'zulip-events--poll)))))))
      (zulip-events--track-request account request callback-ran-p))))

(defun zulip-events-start (account)
  "Start register then consecutive long-poll processing for ACCOUNT."
  (unless (zulip-account-p account)
    (error "Not a Zulip account: %S" account))
  (zulip-events--cancel-inflight account)
  (let ((generation (1+ (or (zulip-account-generation account) 0))))
    (setf (zulip-account-generation account) generation
          (zulip-account-queue-id account) nil
          (zulip-account-last-event-id account) nil
          (zulip-account-connected-p account) nil)
    (unless (zulip-account-longpoll-timeout account)
      (setf (zulip-account-longpoll-timeout account)
            zulip-event-long-poll-timeout))
    (zulip-events--register account generation)
    account))

(defun zulip-events-stop (account)
  "Stop ACCOUNT's event loop and invalidate all pending callbacks."
  (when (zulip-account-p account)
    (setf (zulip-account-generation account)
          (1+ (or (zulip-account-generation account) 0)))
    (zulip-events--cancel-inflight account)
    (setf (zulip-account-connected-p account) nil
          (zulip-account-queue-id account) nil
          (zulip-account-last-event-id account) nil)
    t))

(provide 'zulip-events)

;;; zulip-events.el ends here
