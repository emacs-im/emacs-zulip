;;; zulip-events-test.el --- Tests for Zulip state and events -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
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
                   "50"))))

(ert-deftest zulip-events-message-normalizes-id-unread-and-direct-narrow ()
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
    (should (zulip-state-dm-conversation next '("1" "2")))))

(ert-deftest zulip-events-message-rekeys-pending-placeholder-idempotently ()
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
    (should (equal (zulip-state-message-ids again topic) '("21")))))

(ert-deftest zulip-events-update-reaction-and-bulk-delete-are-pure ()
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
        (should-not (zulip-state-message deleted "31"))))))

(ert-deftest zulip-events-update-message-separates-anchor-and-move-fields ()
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
             "One"))))

(ert-deftest zulip-state-muted-unreads-do-not-drift-wire-count ()
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
    (should (= (zulip-state-unread-count next) 2))))

(ert-deftest zulip-events-flags-maintain-mentions-and-unknown-dm-context ()
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
    (should (= (zulip-state-unread-count state) 0))))

(ert-deftest zulip-state-dm-recent-self-narrow-and-explicit-participants ()
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
    (should (equal (zulip-state-message-ids next self-key) '("51")))))

(ert-deftest zulip-events-modern-read-op-and-all-read ()
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
    (should (= (zulip-state-unread-count all-read) 0))))

(ert-deftest zulip-events-subscription-and-realm-user-reducers ()
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
                 (zulip-state-channel removed 9) 'subscribed))))

(ert-deftest zulip-events-loop-registers-polls-and-ignores-stale-callback ()
  (let* ((account
          (zulip-runtime-create-account
           :server "https://events-test.invalid"
           :email "events@example.invalid"
           :api-key "secret"
           :state (zulip-state-create)))
         register-callback
         poll-callback
         poll-arguments
         poll-timeout
         emitted)
    (unwind-protect
        (progn
          (appkit-app-on
           (zulip-account-app account) 'zulip-event
           (lambda (&rest arguments) (setq emitted arguments)))
          (cl-letf (((symbol-function 'zulip-api-register)
                     (lambda (_account callback &optional _types)
                       (setq register-callback callback)
                       nil))
                    ((symbol-function 'zulip-api-get-events)
                     (lambda (poll-account queue-id last-id callback)
                       (setq poll-arguments (list queue-id last-id)
                             poll-timeout
                             (zulip-account-longpoll-timeout poll-account)
                             poll-callback callback)
                       nil)))
            (zulip-events-start account)
            (funcall
             register-callback
             (zulip-api-result--create
              :ok-p t
              :data
              (zulip-events-test--register
               "event_queue_longpoll_timeout_seconds" 137)))
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
              (should (zulip-state-message
                       (zulip-account-state account) "51"))
              (should (= (zulip-account-last-event-id account) 4))
              (should emitted)
              (zulip-events-stop account)
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
              (should (zulip-state-message
                       (zulip-account-state account) "51")))))
      (zulip-runtime-stop-account account))))

(ert-deftest zulip-events-bad-queue-reregisters-with-new-generation ()
  (let* ((account
          (zulip-runtime-create-account
           :server "https://bad-queue-test.invalid"
           :email "bad-queue@example.invalid"
           :api-key "secret"
           :state (zulip-state-create)))
         (register-count 0)
         register-callback
         poll-callback)
    (unwind-protect
        (cl-letf (((symbol-function 'zulip-api-register)
                   (lambda (_account callback &optional _types)
                     (cl-incf register-count)
                     (setq register-callback callback)
                     nil))
                  ((symbol-function 'zulip-api-get-events)
                   (lambda (_account _queue-id _last-id callback)
                     (setq poll-callback callback)
                     nil)))
          (zulip-events-start account)
          (funcall register-callback
                   (zulip-api-result--create
                    :ok-p t :data (zulip-events-test--register)))
          (let ((generation (zulip-account-generation account)))
            (funcall poll-callback
                     (zulip-api-result--create
                      :ok-p nil :code "BAD_EVENT_QUEUE_ID"))
            (should (= register-count 2))
            (should (> (zulip-account-generation account) generation))
            (should-not (zulip-account-queue-id account))))
      (zulip-runtime-stop-account account))))

(ert-deftest zulip-events-stop-cancels-process-and-timer ()
  (let* ((account
          (zulip-runtime-create-account
           :server "https://stop-test.invalid"
           :email "stop@example.invalid"
           :api-key "secret"
           :state (zulip-state-create)))
         (process (make-pipe-process :name "zulip-events-test-pipe"
                                     :noquery t))
         (timer (run-at-time 120 nil #'ignore)))
    (unwind-protect
        (progn
          (setf (zulip-account-poll-process account) process
                (zulip-account-retry-timer account) timer)
          (zulip-events-stop account)
          (should-not (process-live-p process))
          (should-not (memq timer timer-list))
          (should-not (zulip-account-poll-process account))
          (should-not (zulip-account-retry-timer account)))
      (when (process-live-p process) (delete-process process))
      (when (timerp timer) (cancel-timer timer))
      (zulip-runtime-stop-account account))))

(ert-deftest zulip-events-retry-timers-are-app-owned-and-forgotten-once ()
  (let* ((account
          (zulip-runtime-create-account
           :server "https://retry-handle-test.invalid"
           :email "retry-handle@example.invalid"
           :api-key "secret"
           :state (zulip-state-create)))
         (app (zulip-account-app account))
         (generation (zulip-account-generation account))
         (zulip-event-retry-delay 120)
         (calls 0)
         first-timer first-handle second-timer second-handle third-handle)
    (unwind-protect
        (progn
          (zulip-events--schedule-retry
           account generation
           (lambda (candidate candidate-generation)
             (should (eq candidate account))
             (should (= candidate-generation generation))
             (cl-incf calls)))
          (setq first-timer (zulip-account-retry-timer account)
                first-handle (zulip-account-retry-handle account))
          (should (timerp first-timer))
          (should (appkit-handle-alive-p first-handle))
          (should (eq app (appkit-handle-owner first-handle)))
          (should (eq first-timer (appkit-handle-object first-handle)))

          ;; Replacing a retry retires its timer handle before installing one
          ;; new owner entry.
          (zulip-events--schedule-retry
           account generation
           (lambda (_candidate _candidate-generation) (cl-incf calls)))
          (setq second-timer (zulip-account-retry-timer account)
                second-handle (zulip-account-retry-handle account))
          (should-not (appkit-handle-alive-p first-handle))
          (should-not (memq first-timer timer-list))
          (should (equal (appkit-app-handles app) (list second-handle)))

          ;; Invoke the timer callback directly so the test covers its fire
          ;; path without waiting for wall-clock time.
          (apply (timer--function second-timer) (timer--args second-timer))
          (should (= calls 1))
          (should-not (zulip-account-retry-timer account))
          (should-not (zulip-account-retry-handle account))
          (should-not (appkit-handle-alive-p second-handle))
          (should-not (appkit-app-handles app))
          (should-not (memq second-timer timer-list))
          ;; A stale queued invocation cannot consume the operation twice.
          (apply (timer--function second-timer) (timer--args second-timer))
          (should (= calls 1))

          (zulip-events--schedule-retry account generation #'ignore)
          (setq third-handle (zulip-account-retry-handle account))
          (zulip-events-stop account)
          (should-not (appkit-handle-alive-p third-handle))
          (should-not (zulip-account-retry-timer account))
          (should-not (zulip-account-retry-handle account))
          (should-not (appkit-app-handles app)))
      (zulip-runtime-stop-account account))))

(ert-deftest zulip-events-reconnect-and-stop-forget-owned-http-handles ()
  (let* ((account
          (zulip-runtime-create-account
           :server "https://handle-stop-test.invalid"
           :email "handle-stop@example.invalid"
           :api-key "secret"
           :state (zulip-state-create)))
         (app (zulip-account-app account))
         processes
         first-process
         first-handle
         second-process
         second-handle)
    (unwind-protect
        (cl-letf (((symbol-function 'plz)
                   (lambda (&rest _arguments)
                     (let ((process
                            (make-pipe-process
                             :name "zulip-events-owned-http-test-pipe"
                             :noquery t)))
                       (push process processes)
                       process))))
          (zulip-events-start account)
          (setq first-process (zulip-account-poll-process account)
                first-handle (car (appkit-app-handles app)))
          (should (process-live-p first-process))
          (should (appkit-handle-alive-p first-handle))

          ;; Starting again is a soft reconnect and must retire the old owner
          ;; handle before registering the replacement request.
          (zulip-events-start account)
          (setq second-process (zulip-account-poll-process account)
                second-handle (car (appkit-app-handles app)))
          (should-not (process-live-p first-process))
          (should-not (appkit-handle-alive-p first-handle))
          (should (process-live-p second-process))
          (should (appkit-handle-alive-p second-handle))
          (should (equal (appkit-app-handles app) (list second-handle)))

          (zulip-events-stop account)
          (should-not (process-live-p second-process))
          (should-not (appkit-handle-alive-p second-handle))
          ;; Neither live nor already-cancelled process handles may remain on
          ;; the still-live Appkit application after a soft stop.
          (should-not (appkit-app-handles app))
          (should-not (zulip-account-poll-process account)))
      (dolist (process processes)
        (when (process-live-p process) (delete-process process)))
      (zulip-runtime-stop-account account))))

(provide 'zulip-events-test)

;;; zulip-events-test.el ends here
