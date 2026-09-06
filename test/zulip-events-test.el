;;; zulip-events-test.el --- Tests for Zulip state and events -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'zulip-runtime-test)
(require 'cl-lib)
(require 'zulip-events)

(defun zulip-events-test--hash (&rest pairs)
  "Return an equal hash table populated from alternating PAIRS."
  (let ((table (make-hash-table :test #'equal)))
    (while pairs
      (puthash (pop pairs) (pop pairs) table))
    table))

(defun zulip-events-test--register (&rest extra)
  "Return representative register data with EXTRA alternating fields."
  (let* ((unread
          (zulip-events-test--hash
           "pms" (vector
                  (zulip-events-test--hash
                   "sender_id" 2 "unread_message_ids" [7 8]))
           "mentions" [8]
           "streams" (vector
                      (zulip-events-test--hash
                       "stream_id" 5 "topic" "One"
                       "unread_message_ids" [9]))
           "huddles" (vector
                      (zulip-events-test--hash
                       "user_ids_string" "2,3"
                       "unread_message_ids" [10]))
           "count" 4))
         (data
          (zulip-events-test--hash
           "queue_id" "queue-a"
           "last_event_id" 3
           "user_id" 1
           "realm_users"
           (vector (zulip-events-test--hash "user_id" 1 "full_name" "Me")
                   (zulip-events-test--hash "user_id" 2 "full_name" "Two"))
           "subscriptions"
           (vector (zulip-events-test--hash
                    "stream_id" 5 "name" "general"))
           "unread_msgs" unread
           "recent_private_conversations"
           (vector (zulip-events-test--hash
                    "user_ids" [2] "max_message_id" 50)))))
    (while extra
      (puthash (pop extra) (pop extra) data))
    data))

(defun zulip-events-test--message
    (id &optional type subject stream-id flags)
  "Return a raw message fixture."
  (zulip-events-test--hash
   "id" id
   "type" (or type "stream")
   "subject" (or subject "One")
   "stream_id" (or stream-id 5)
   "display_recipient" "general"
   "sender_id" 2
   "timestamp" id
   "content" (format "message-%s" id)
   "flags" (or flags [])))

(ert-deftest zulip-state-register-normalizes-entities-unread-and-dms ()
  (zulip-runtime-test--isolated
    (let* ((old (zulip-state-upsert-message
                 (zulip-state-create) (zulip-events-test--message 1)))
           (state (zulip-state-from-register
                   (zulip-events-test--register)))
           (conversation (zulip-state-dm-conversation state '("1" "2"))))
      (should (zulip-state-message old "1"))
      (should-not (zulip-state-message state "1"))
      (should (equal (zulip-state-object-get
                      (zulip-state-user state 2) 'full_name)
                     "Two"))
      (should (equal (zulip-state-object-get
                      (zulip-state-channel state 5) 'name)
                     "general"))
      (dolist (id '("7" "8" "9" "10"))
        (should (zulip-state-unread-message-p state id)))
      (should (= (zulip-state-unread-count state) 4))
      (should conversation)
      (should (equal (zulip-dm-conversation-participant-ids conversation)
                     '("1" "2")))
      (should (equal (zulip-dm-conversation-max-message-id conversation)
                     "50")))))

(ert-deftest zulip-events-message-normalizes-id-unread-and-direct-narrow ()
  (zulip-runtime-test--isolated
    (let* ((state (zulip-state-from-register
                   (zulip-events-test--register)))
           (direct-two '(direct "2"))
           (direct-three '(direct "3"))
           (message
            (zulip-events-test--hash
             "id" 12 "type" "private" "sender_id" 2 "timestamp" 12
             "display_recipient"
             (vector (zulip-events-test--hash "id" 1)
                     (zulip-events-test--hash "id" 2))))
           next)
      (puthash direct-two nil (zulip-state-narrows state))
      (puthash direct-three nil (zulip-state-narrows state))
      (setq next
            (zulip-events-reduce
             state
             (zulip-events-test--hash
              "type" "message" "message" message "flags" [])))
      (should-not (zulip-state-message state "12"))
      (should (equal (zulip-state-message-id
                      (zulip-state-message next "12"))
                     "12"))
      (should (equal (zulip-state-message-ids next nil) '("12")))
      (should (equal (zulip-state-message-ids next direct-two) '("12")))
      (should-not (zulip-state-message-ids next direct-three))
      (should (zulip-state-unread-message-p next "12"))
      (should (zulip-state-dm-conversation next '("1" "2"))))))

(ert-deftest zulip-events-message-rekeys-pending-placeholder-idempotently ()
  (zulip-runtime-test--isolated
    (let* ((topic '(topic "general" "One"))
           (pending
            '((id . "local-1") (local-id . "local-1") (pending . t)
              (type . "stream") (stream_id . 5) (subject . "One")
              (timestamp . 20) (content . "pending")))
           (state (zulip-state-upsert-message
                   (zulip-state-create) pending (list topic)))
           (event
            (zulip-events-test--hash
             "type" "message"
             "local_message_id" "local-1"
             "flags" ["read"]
             "message" (zulip-events-test--message 21)))
           (next (zulip-events-reduce state event))
           (again nil))
      (setq again (zulip-events-reduce next event))
      (should-not (zulip-state-message next "local-1"))
      (should (zulip-state-message next "21"))
      (should (equal (zulip-state-object-get
                      (zulip-state-message next "21") 'local-id)
                     "local-1"))
      (should-not (zulip-state-object-get
                   (zulip-state-message next "21") 'pending))
      (should (equal (zulip-state-message-ids next topic) '("21")))
      (should (equal (zulip-state-message-ids again topic) '("21"))))))

(ert-deftest zulip-events-update-reaction-and-bulk-delete-are-pure ()
  (zulip-runtime-test--isolated
    (let* ((old-key '(topic "general" "One"))
           (new-key '(topic "general" "Two"))
           (state (zulip-state-create)))
      (puthash old-key nil (zulip-state-narrows state))
      (puthash new-key nil (zulip-state-narrows state))
      (setq state (zulip-state-upsert-message
                   state (zulip-events-test--message 30)))
      (setq state (zulip-state-upsert-message
                   state (zulip-events-test--message 31)))
      (let ((moved
             (zulip-events-reduce
              state
              (zulip-events-test--hash
               "type" "update_message"
               "message_id" 30 "message_ids" [30 31]
               "subject" "Two"))))
        (should (equal (zulip-state-object-get
                        (zulip-state-message state "30") 'topic)
                       "One"))
        (should (equal (zulip-state-message-ids moved new-key)
                       '("30" "31")))
        (should-not (zulip-state-message-ids moved old-key))
        (let* ((reaction
                (zulip-events-test--hash
                 "type" "reaction" "op" "add" "message_id" 30
                 "user_id" 2 "emoji_name" "thumbs_up"
                 "emoji_code" "1f44d" "reaction_type" "unicode_emoji"))
               (reacted (zulip-events-reduce moved reaction))
               (deleted
                (zulip-events-reduce
                 reacted
                 (zulip-events-test--hash
                  "type" "delete_message" "message_ids" [30 31]))))
          (should (= (length
                      (zulip-state-object-get
                       (zulip-state-message reacted "30") 'reactions))
                     1))
          (should-not (zulip-state-message deleted "30"))
          (should-not (zulip-state-message deleted "31")))))))

(ert-deftest zulip-events-update-message-separates-anchor-and-move-fields ()
  (zulip-runtime-test--isolated
    (let* ((old-key '(topic "5" "One"))
           (new-key '(topic "6" "Two"))
           (state
            (zulip-state-from-register
             (zulip-events-test--register
              "subscriptions"
              (vector (zulip-events-test--hash
                       "stream_id" 5 "name" "general")
                      (zulip-events-test--hash
                       "stream_id" 6 "name" "new")))))
           moved)
      (puthash old-key nil (zulip-state-narrows state))
      (puthash new-key nil (zulip-state-narrows state))
      (setq state
            (zulip-state-upsert-message
             state (zulip-events-test--message 30 nil nil nil [])))
      (setq state
            (zulip-state-upsert-message
             state
             (zulip-events-test--message
              31 nil nil nil ["read" "starred"])))
      (setq moved
            (zulip-events-reduce
             state
             (zulip-events-test--hash
              "type" "update_message"
              "message_id" 30
              "message_ids" [30 31]
              "stream_id" 5
              "new_stream_id" 6
              "orig_subject" "One"
              "subject" "Two"
              "content" "**raw edit**"
              "rendered_content" "<p><strong>raw edit</strong></p>"
              "flags" ["mentioned"])))
      (dolist (id '("30" "31"))
        (should (equal
                 (zulip-state-object-get
                  (zulip-state-message moved id) 'channel-id)
                 "6"))
        (should (equal
                 (zulip-state-object-get
                  (zulip-state-message moved id) 'stream_id)
                 "6"))
        (should (equal
                 (zulip-state-object-get
                  (zulip-state-message moved id) 'topic)
                 "Two")))
      (should-not (zulip-state-message-ids moved old-key))
      (should (equal (zulip-state-message-ids moved new-key) '("30" "31")))
      (should (equal
               (zulip-state-object-get
                (zulip-state-message moved "30") 'content)
               "<p><strong>raw edit</strong></p>"))
      (should (equal
               (zulip-state-object-get
                (zulip-state-message moved "30") 'raw-content)
               "**raw edit**"))
      (should (equal
               (zulip-state-object-get
                (zulip-state-message moved "31") 'content)
               "message-31"))
      (should (equal
               (zulip-state--flags (zulip-state-message moved "31"))
               '("read" "starred")))
      (should (gethash "30" (zulip-state-unread-mentions moved)))
      (should-not (gethash "31" (zulip-state-unread-mentions moved)))
      (should-not (zulip-state-unread-message-p moved "31"))
      ;; Reducers never mutate the prior state.
      (should (equal
               (zulip-state-object-get
                (zulip-state-message state "30") 'topic)
               "One")))))

(ert-deftest zulip-state-muted-unreads-do-not-drift-wire-count ()
  (zulip-runtime-test--isolated
    (let* ((unread
            (zulip-events-test--hash
             "pms" [] "huddles" [] "mentions" []
             "streams"
             (vector
              (zulip-events-test--hash
               "stream_id" 5 "topic" "Muted"
               "unread_message_ids" [60])
              (zulip-events-test--hash
               "stream_id" 5 "topic" "Loud"
               "unread_message_ids" [61]))
             "count" 1))
           (state
            (zulip-state-from-register
             (zulip-events-test--register
              "subscriptions"
              (vector (zulip-events-test--hash
                       "stream_id" 5 "name" "general"
                       "is_muted" :json-false))
              "user_topics"
              (vector (zulip-events-test--hash
                       "stream_id" 5 "topic_name" "Muted"
                       "visibility_policy" 1))
              "unread_msgs" unread)))
           (read-muted
            (zulip-events-reduce
             state
             (zulip-events-test--hash
              "type" "update_message_flags" "op" "add"
              "flag" "read" "messages" [60])))
           (read-loud
            (zulip-events-reduce
             state
             (zulip-events-test--hash
              "type" "update_message_flags" "op" "add"
              "flag" "read" "messages" [61])))
           next)
      (should (zulip-state-unread-message-p state "60"))
      (should (zulip-state-unread-message-p state "61"))
      (should (= (zulip-state-unread-count state) 1))
      ;; Muted IDs exist in register unread data but are absent from `count'.
      (should (= (zulip-state-unread-count read-muted) 1))
      (should (= (zulip-state-unread-count read-loud) 0))
      (setq next
            (zulip-events-reduce
             state
             (zulip-events-test--hash
              "type" "message" "flags" []
              "message" (zulip-events-test--message 62 nil "Muted" 5 []))))
      (should (= (zulip-state-unread-count next) 1))
      (setq next
            (zulip-events-reduce
             next
             (zulip-events-test--hash
              "type" "message" "flags" []
              "message" (zulip-events-test--message 63 nil "Loud" 5 []))))
      (should (= (zulip-state-unread-count next) 2))
      ;; Live visibility changes recompute only known ID contributions.
      (setq next
            (zulip-events-reduce
             next
             (zulip-events-test--hash
              "type" "user_topic" "stream_id" 5 "topic_name" "Loud"
              "visibility_policy" 1 "last_updated" 10)))
      (should (= (zulip-state-unread-count next) 0))
      (setq next
            (zulip-events-reduce
             next
             (zulip-events-test--hash
              "type" "user_topic" "stream_id" 5 "topic_name" "Muted"
              "visibility_policy" 0 "last_updated" 11)))
      (should (= (zulip-state-unread-count next) 2)))))

(ert-deftest zulip-events-flags-maintain-mentions-and-unknown-dm-context ()
  (zulip-runtime-test--isolated
    (let ((state (zulip-state-create)))
      (setf (zulip-state-self-user-id state) "1")
      (setq state
            (zulip-events-reduce
             state
             (zulip-events-test--hash
              "type" "update_message_flags" "op" "remove"
              "flag" "read" "messages" [70]
              "message_details"
              (zulip-events-test--hash
               "70"
               (zulip-events-test--hash
                "type" "private" "user_ids" [2 3]
                "mentioned" t)))))
      (should (zulip-state-unread-message-p state "70"))
      (should (= (zulip-state-unread-count state) 1))
      (should (gethash "70" (zulip-state-unread-mentions state)))
      (let ((conversation
             (zulip-state-dm-conversation state '("1" "2" "3"))))
        (should conversation)
        (should (equal
                 (zulip-dm-conversation-max-message-id conversation)
                 "70")))
      (setq state
            (zulip-events-reduce
             state
             (zulip-events-test--hash
              "type" "update_message_flags" "op" "add"
              "flag" "read" "messages" [70])))
      (should-not (zulip-state-unread-message-p state "70"))
      (should-not (gethash "70" (zulip-state-unread-mentions state)))
      (should (= (zulip-state-unread-count state) 0))
      ;; Reading the only locally known message cannot prove server history empty.
      (should (zulip-state-dm-conversation state '("1" "2" "3")))
      ;; The server's exact count contribution wins for uncached stream messages.
      (setq state
            (zulip-events-reduce
             state
             (zulip-events-test--hash
              "type" "update_message_flags" "op" "remove"
              "flag" "read" "messages" [71]
              "message_details"
              (zulip-events-test--hash
               "71"
               (zulip-events-test--hash
                "type" "stream" "stream_id" 5 "topic" "Muted"
                "unmuted_stream_msg" :json-false "mentioned" t)))))
      (should (zulip-state-unread-message-p state "71"))
      (should (gethash "71" (zulip-state-unread-mentions state)))
      (should (= (zulip-state-unread-count state) 0)))))

(ert-deftest zulip-state-dm-recent-self-narrow-and-explicit-participants ()
  (zulip-runtime-test--isolated
    (let* ((state (zulip-state-from-register
                   (zulip-events-test--register)))
           (self-key '(direct "1"))
           (direct
            (zulip-events-test--hash
             "id" 50 "type" "private" "sender_id" 2 "flags" ["read"]
             "display_recipient"
             (vector (zulip-events-test--hash "id" 1)
                     (zulip-events-test--hash "id" 2))))
           next)
      (puthash self-key nil (zulip-state-narrows state))
      (setq state (zulip-state-upsert-message state direct))
      (setq next
            (zulip-events-reduce
             state
             (zulip-events-test--hash
              "type" "reaction" "op" "add" "message_id" 50
              "user_id" 2 "emoji_name" "wave" "emoji_code" "1f44b"
              "reaction_type" "unicode_emoji")))
      (let ((conversation (zulip-state-dm-conversation next '("1" "2"))))
        (should conversation)
        (should (equal (zulip-dm-conversation-message-ids conversation)
                       '("50")))
        (should (equal (zulip-dm-conversation-max-message-id conversation)
                       "50")))
      (setq next
            (zulip-events-reduce
             next
             (zulip-events-test--hash
              "type" "delete_message" "message_id" 50)))
      (let ((conversation (zulip-state-dm-conversation next '("1" "2"))))
        (should conversation)
        (should-not (zulip-dm-conversation-message-ids conversation))
        (should (equal (zulip-dm-conversation-max-message-id conversation)
                       "50")))
      (let ((newer (copy-hash-table direct))
            (older (copy-hash-table direct)))
        (puthash "id" 60 newer)
        (puthash "id" 55 older)
        (setq next (zulip-state-upsert-message next newer)
              next (zulip-state-upsert-message next older))
        (should (equal
                 (zulip-dm-conversation-max-message-id
                  (zulip-state-dm-conversation next '("1" "2")))
                 "60")))
      ;; State-side optimistic DM contract: callers may provide participant_ids.
      (setq next
            (zulip-state-upsert-message
             next
             '((id . "local-explicit") (type . "direct")
               (participant_ids . [3]) (content . "pending"))))
      (should (zulip-state-dm-conversation next '("1" "3")))
      (should-not (zulip-state-dm-conversation next '("1")))
      (setq next
            (zulip-state-upsert-message
             next
             '((id . 51) (type . "private") (sender_id . 1)
               (display_recipient . [((id . 1))]) (flags . ["read"]))))
      (should (equal (zulip-state-message-ids next self-key) '("51"))))))

(ert-deftest zulip-events-modern-read-op-and-all-read ()
  (zulip-runtime-test--isolated
    (let* ((state (zulip-state-upsert-message
                   (zulip-state-create)
                   (zulip-events-test--message 40 nil nil nil [])))
           (read
            (zulip-events-reduce
             state
             (zulip-events-test--hash
              "type" "update_message_flags" "op" "add"
              "flag" "read" "messages" [40])))
           (unread
            (zulip-events-reduce
             read
             (zulip-events-test--hash
              "type" "update_message_flags" "op" "remove"
              "flag" "read" "messages" [40])))
           (false-all
            (zulip-events-reduce
             unread
             (zulip-events-test--hash
              "type" "update_message_flags" "op" "add"
              "flag" "read" "all" :json-false "messages" [])))
           (all-read
            (zulip-events-reduce
             false-all
             (zulip-events-test--hash
              "type" "update_message_flags" "op" "add"
              "flag" "read" "all" t "messages" []))))
      (should (zulip-state-unread-message-p state "40"))
      (should-not (zulip-state-unread-message-p read "40"))
      (should (zulip-state-unread-message-p unread "40"))
      (should (zulip-state-unread-message-p false-all "40"))
      (should-not (zulip-state-unread-message-p all-read "40"))
      (should (= (zulip-state-unread-count all-read) 0)))))

(ert-deftest zulip-events-subscription-and-realm-user-reducers ()
  (zulip-runtime-test--isolated
    (let* ((state (zulip-state-create))
           (subscribed
            (zulip-events-reduce
             state
             (zulip-events-test--hash
              "type" "subscription" "op" "add"
              "subscriptions"
              (vector (zulip-events-test--hash
                       "stream_id" 9 "name" "new")))))
           (user-added
            (zulip-events-reduce
             subscribed
             (zulip-events-test--hash
              "type" "realm_user" "op" "add"
              "person" (zulip-events-test--hash
                        "user_id" 7 "full_name" "Seven"))))
           (removed
            (zulip-events-reduce
             user-added
             (zulip-events-test--hash
              "type" "subscription" "op" "remove"
              "subscriptions"
              (vector (zulip-events-test--hash "stream_id" 9))))))
      (should-not (zulip-state-channel state 9))
      (should (zulip-state-channel subscribed 9))
      (should (gethash "9" (zulip-state-subscriptions subscribed)))
      (should (equal (zulip-state-object-get
                      (zulip-state-user user-added 7) 'full_name)
                     "Seven"))
      (should-not (gethash "9" (zulip-state-subscriptions removed)))
      (should-not (zulip-state-object-get
                   (zulip-state-channel removed 9) 'subscribed)))))

(ert-deftest zulip-events-loop-registers-polls-and-ignores-stale-callback ()
  (zulip-runtime-test--isolated
    (let* ((account
            (zulip-runtime-create-account
             :server "https://events-test.invalid"
             :email "events@example.invalid"
             :api-key "secret"
             :state (zulip-state-create)))
           register-callback
           poll-callback
           poll-arguments
           poll-timeout)
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'zulip-api-register)
                       (lambda (_account callback &rest _options)
                         (let* ((owner (zulip-account-app _account))
                                (process (make-pipe-process :name "zulip-events-test-transport"
                                                            :buffer nil :noquery t))
                                (handle (appkit-register-handle owner 'process process))
                                (deliver callback)
                                (callback (lambda (result)
                                            (appkit-retire-handle handle)
                                            (when (process-live-p process) (delete-process process))
                                            ;; Let the Source fence cancelled and duplicate callbacks.
                                            (funcall deliver result))))
                           (should (eq account _account))
                           (setq register-callback callback)

                           handle)))
                      ((symbol-function 'zulip-api-get-events)
                       (lambda (poll-account queue-id last-id callback &rest _options)
                         (let* ((owner (zulip-account-app poll-account))
                                (process (make-pipe-process :name "zulip-events-test-transport"
                                                            :buffer nil :noquery t))
                                (handle (appkit-register-handle owner 'process process))
                                (deliver callback)
                                (callback (lambda (result)
                                            (appkit-retire-handle handle)
                                            (when (process-live-p process) (delete-process process))
                                            ;; Let the Source fence cancelled and duplicate callbacks.
                                            (funcall deliver result))))
                           (should (eq account poll-account))
                           (setq poll-arguments (list queue-id last-id)
                                 poll-timeout
                                 (zulip-account-longpoll-timeout poll-account)
                                 poll-callback callback)

                           handle))))
              (zulip-events-start account)
              (zulip-runtime-test--drain account)
              (funcall
               register-callback
               (zulip-api-result--create
                :ok-p t
                :data
                (zulip-events-test--register
                 "event_queue_longpoll_timeout_seconds" 137)))
              (zulip-runtime-test--drain account)
              (should (equal poll-arguments '("queue-a" 3)))
              (should (= poll-timeout 137))
              (should (= (zulip-account-longpoll-timeout account) 137))
              (let ((first-poll poll-callback))
                (funcall
                 first-poll
                 (zulip-api-result--create
                  :ok-p t
                  :data
                  (zulip-events-test--hash
                   "events"
                   (vector
                    (zulip-events-test--hash
                     "id" 4 "type" "message" "flags" ["read"]
                     "message" (zulip-events-test--message 51))))))
                (zulip-runtime-test--drain account)
                (should (zulip-state-message
                         (zulip-account-state account) "51"))
                (should (= (zulip-account-last-event-id account) 4))
                (let ((pending (appkit-handle-object
                                (zulip-account-poll-process account)))
                      (stopped-poll poll-callback))
                  (should (process-live-p pending))
                  (zulip-events-stop account)
                  (zulip-runtime-test--drain account)
                  (should-not (process-live-p pending))
                  (funcall stopped-poll
                           (zulip-api-result--create
                            :ok-p t :data
                            (zulip-events-test--hash
                             "events"
                             (vector (zulip-events-test--hash
                                      "id" 5 "type" "delete_message"
                                      "message_id" 51)))))
                  (zulip-runtime-test--drain account)
                  (should (zulip-state-message
                           (zulip-account-state account) "51")))
                (funcall
                 first-poll
                 (zulip-api-result--create
                  :ok-p t
                  :data
                  (zulip-events-test--hash
                   "events"
                   (vector
                    (zulip-events-test--hash
                     "id" 5 "type" "delete_message"
                     "message_id" 51)))))
                (zulip-runtime-test--drain account)
                (should (zulip-state-message
                         (zulip-account-state account) "51")))))
        (zulip-runtime-stop-account account)))))

(ert-deftest zulip-events-bad-queue-reregisters-with-new-generation ()
  (zulip-runtime-test--isolated
    (let* ((account
            (zulip-runtime-create-account
             :server "https://bad-queue-test.invalid"
             :email "bad-queue@example.invalid"
             :api-key "secret"
             :state (zulip-state-create)))
           (register-count 0)
           register-callback
           poll-callback
           poll-arguments)
      (unwind-protect
          (cl-letf (((symbol-function 'zulip-api-register)
                     (lambda (_account callback &rest _options)
                       (let* ((owner (zulip-account-app _account))
                              (process (make-pipe-process :name "zulip-events-test-transport"
                                                          :buffer nil :noquery t))
                              (handle (appkit-register-handle owner 'process process))
                              (deliver callback)
                              (callback (lambda (result)
                                          (appkit-retire-handle handle)
                                          (when (process-live-p process) (delete-process process))
                                          ;; Let the Source fence cancelled and duplicate callbacks.
                                          (funcall deliver result))))
                         (should (eq account _account))
                         (cl-incf register-count)
                         (setq register-callback callback)

                         handle)))
                    ((symbol-function 'zulip-api-get-events)
                     (lambda (_account _queue-id _last-id callback &rest _options)
                       (let* ((owner (zulip-account-app _account))
                              (process (make-pipe-process :name "zulip-events-test-transport"
                                                          :buffer nil :noquery t))
                              (handle (appkit-register-handle owner 'process process))
                              (deliver callback)
                              (callback (lambda (result)
                                          (appkit-retire-handle handle)
                                          (when (process-live-p process) (delete-process process))
                                          ;; Let the Source fence cancelled and duplicate callbacks.
                                          (funcall deliver result))))
                         (should (eq account _account))
                         (setq poll-callback callback
                               poll-arguments (list _queue-id _last-id))

                         handle))))
            (zulip-events-start account)
            (zulip-runtime-test--drain account)
            (funcall register-callback
                     (zulip-api-result--create
                      :ok-p t :data (zulip-events-test--register)))
            (zulip-runtime-test--drain account)
            (let ((generation (zulip-account-generation account))
                  (expired-poll poll-callback))
              (funcall poll-callback
                       (zulip-api-result--create
                        :ok-p nil :code "BAD_EVENT_QUEUE_ID"))
              (zulip-runtime-test--drain account)
              (should (= register-count 2))
              (should (> (zulip-account-generation account) generation))
              (should-not (zulip-account-queue-id account))
              (funcall register-callback
                       (zulip-api-result--create
                        :ok-p t :data (zulip-events-test--register
                                       "queue_id" "queue-b" "last_event_id" 20)))
              (zulip-runtime-test--drain account)
              (should (equal poll-arguments '("queue-b" 20)))
              (funcall expired-poll
                       (zulip-api-result--create :ok-p nil :code "BAD_EVENT_QUEUE_ID"))
              (zulip-runtime-test--drain account)
              (should (= register-count 2))
              (should (equal (zulip-account-queue-id account) "queue-b"))
              (should (= (zulip-account-last-event-id account) 20))))
        (zulip-runtime-stop-account account)))))

(ert-deftest zulip-events-retry-timers-are-app-owned-and-forgotten-once ()
  (zulip-runtime-test--isolated
    (let ((account (zulip-runtime-create-account
                    :server "https://retry.example.test" :email "retry@example.test"
                    :api-key "secret" :state (zulip-state-create)))
          (zulip-event-retry-delay 120)
          (calls 0) callback timer)
      (cl-letf (((symbol-function 'zulip-api-register)
                 (lambda (_account response &rest _options)
                   (let* ((owner (zulip-account-app _account))
                          (process (make-pipe-process :name "zulip-events-test-transport"
                                                      :buffer nil :noquery t))
                          (handle (appkit-register-handle owner 'process process))
                          (deliver response)
                          (response (lambda (result)
                                      (appkit-retire-handle handle)
                                      (when (process-live-p process) (delete-process process))
                                      ;; Let the Source fence cancelled and duplicate callbacks.
                                      (funcall deliver result))))
                     (should (eq account _account))
                     (cl-incf calls)
                     (setq callback response)
                     handle))))
        (zulip-events-start account)
        (zulip-runtime-test--drain account)
        (funcall callback (zulip-api-result--create :ok-p nil :message "offline"))
        (zulip-runtime-test--drain account)
        (setq timer (zulip-account-retry-timer account))
        (should (timerp timer))
        (apply (timer--function timer) (timer--args timer))
        (zulip-runtime-test--drain account)
        (should (= calls 2))
        (should-not (zulip-account-retry-timer account))
        ;; Redelivery of the retired retry cannot start another registration.
        (apply (timer--function timer) (timer--args timer))
        (zulip-runtime-test--drain account)
        (should (= calls 2))
        (funcall callback (zulip-api-result--create :ok-p nil :message "offline"))
        (zulip-runtime-test--drain account)
        (setq timer (zulip-account-retry-timer account))
        (zulip-events-stop account)
        (zulip-runtime-test--drain account)
        (should-not (memq timer timer-list))
        (apply (timer--function timer) (timer--args timer))
        (zulip-runtime-test--drain account)
        (should (= calls 2))))))

(ert-deftest zulip-events-reconnect-and-stop-forget-owned-http-handles ()
  (zulip-runtime-test--isolated
    (let* ((account (zulip-runtime-create-account
                     :server "https://source.example.test" :email "source@example.test"
                     :api-key "secret" :state (zulip-state-create)))
           requests canceled callbacks)
      (cl-letf (((symbol-function 'zulip-api-register)
                 (lambda (_account callback &rest _options)
                   (let* ((token (make-symbol "register"))
                          (handle (appkit-register-handle
                                   (zulip-account-app account) 'function token
                                   (lambda (object) (push object canceled)))))
                     (push callback callbacks)
                     (push handle requests)
                     handle))))
        (zulip-events-start account)
        (let ((old-handle (car requests))
              (old-callback (car callbacks)))
          (zulip-events-start account)
          (should (= 2 (length requests)))
          (should-not (appkit-handle-alive-p old-handle))
          (should (appkit-handle-alive-p (car requests)))
          (funcall old-callback
                   (zulip-api-result--create :ok-p t :data (zulip-events-test--register)))
          (zulip-runtime-test--drain account)
          (should-not (zulip-account-queue-id account))
          (zulip-events-stop account)
          (should-not (appkit-handle-alive-p (car requests)))
          (should (= 2 (length canceled)))
          (funcall (car callbacks)
                   (zulip-api-result--create :ok-p t :data (zulip-events-test--register)))
          (zulip-runtime-test--drain account)
          (should-not (zulip-account-queue-id account)))))))

(provide 'zulip-events-test)

;;; zulip-events-test.el ends here
