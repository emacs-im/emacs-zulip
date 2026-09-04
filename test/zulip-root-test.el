;;; zulip-root-test.el --- Tests for the Zulip navigator -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'zulip-runtime-test)
(require 'cl-lib)
(require 'seq)
(require 'zulip-root)

(defun zulip-root-test--message
    (id type content &optional topic stream-id display-recipient)
  "Return a raw message fixture with ID, TYPE, and CONTENT.

TOPIC, STREAM-ID, and DISPLAY-RECIPIENT describe channel or direct context."
  (append
   `((id . ,id)
     (type . ,type)
     (sender_id . 2)
     (sender_full_name . "Two")
     (timestamp . ,id)
     (content . ,content))
   (and topic `((subject . ,topic)))
   (and stream-id `((stream_id . ,stream-id)))
   (and display-recipient `((display_recipient . ,display-recipient)))))

(defun zulip-root-test--register ()
  "Return representative register data for root projection tests."
  (let ((direct-recipients
         [((id . 1) (full_name . "Me"))
          ((id . 2) (full_name . "Two"))]))
    `((user_id . 1)
      (realm_users
       . [((user_id . 1) (full_name . "Me") (email . "me@example.test"))
          ((user_id . 2) (full_name . "Two") (email . "two@example.test"))])
      (subscriptions
       . [((stream_id . 5) (name . "general") (in_home_view . t))])
      (messages
       . ,(vector
           (zulip-root-test--message
            91 "stream" "first channel message" "One" 5 "general")
           (append
            (zulip-root-test--message
             92 "stream" "latest channel message" "One" 5 "general")
            '((flags . ["starred"])))
           (zulip-root-test--message
            81 "private" "direct hello" nil nil direct-recipients)))
      (recent_private_conversations
       . [((user_ids . [2]) (max_message_id . 81))])
      (unread_msgs
       . ((streams
           . [((stream_id . 5)
               (topic . "One")
               (unread_message_ids . [91 92]))])
          (pms
           . [((sender_id . 2) (unread_message_ids . [81]))])
          (mentions . [92 81])
          (count . 3))))))

(defun zulip-root-test--register-with-streams (count)
  "Return register data with COUNT subscribed channels starting at ID 5."
  (let ((data (copy-tree (zulip-root-test--register))))
    (setcdr
     (assq 'subscriptions data)
     (vconcat
      (cl-loop for offset below count
               for stream-id = (+ 5 offset)
               collect `((stream_id . ,stream-id)
                         (name . ,(format "channel-%03d" stream-id))
                         (in_home_view . t)))))
    data))

(defmacro zulip-root-test--with-account (binding &rest body)
  "Create isolated account BINDING and retire only its fixture buffers."
  (declare (indent 1) (debug (symbolp body)))
  `(let* ((zulip-runtime--accounts (make-hash-table :test #'equal))
          (zulip-runtime-change-hook nil)
          (state (zulip-state-from-register (zulip-root-test--register)))
          (,binding (zulip-runtime-create-account
                     :server "https://root.example.test/"
                     :email "me@example.test" :api-key "secret" :state state))
          (zulip-root-test--open-function (symbol-function 'zulip-root--open-buffer))
          zulip-root-test--buffers
          zulip-root-test--processes)
     (setf (zulip-account-connected-p ,binding) t)
     (unwind-protect
         (cl-letf (((symbol-function 'zulip-api-get-topics)
                    (lambda (_account stream-id _callback &rest options)
                      (let* ((owner (plist-get options :owner))
                             (process (make-pipe-process :name "zulip-root-test-transport"
                                                         :buffer nil :noquery t))
                             (handle (appkit-register-handle owner 'process process))
                             (response _callback)
                             (_callback (lambda (result)
                                          (appkit-retire-handle handle)
                                          (when (process-live-p process) (delete-process process))
                                         ;; Deliver even after cancellation to exercise runtime fencing.
                                          (funcall response result))))
                        (push process zulip-root-test--processes)
                        (should (eq (zulip-account-app _account) (appkit-owner-app owner)))
                        handle)))
                   ((symbol-function 'zulip-root--open-buffer)
                    (lambda (&rest arguments)
                      (let ((buffer (apply zulip-root-test--open-function arguments)))
                        (cl-pushnew buffer zulip-root-test--buffers)
                        buffer))))
           ,@body)
       (zulip-runtime-stop-all)
       (dolist (process zulip-root-test--processes)
         (when (process-live-p process) (delete-process process)))
       (dolist (buffer zulip-root-test--buffers)
         (when (buffer-live-p buffer) (kill-buffer buffer))))))

(defun zulip-root-test--entry (entries key)
  "Return from ENTRIES the projected root entry identified by KEY."
  (seq-find (lambda (entry)
              (equal key (zulip-root--entry-key entry)))
            entries))

(defun zulip-root-test--topic-entries (entries)
  "Return only topic rows from projected ENTRIES."
  (seq-filter (lambda (entry)
                (eq (zulip-root--entry-type entry) 'topic))
              entries))

(defun zulip-root-test--row-position (key)
  "Return the current root buffer position carrying stable row KEY."
  (appkit-position-find-property-value
   (point-min) (point-max) appkit-directory-key-property key))

(defun zulip-root-test--topics-result (&rest topics)
  "Return one successful API result containing TOPICS."
  (zulip-api-result--create
   :ok-p t :status 200 :data `((topics . ,(vconcat topics)))))

(ert-deftest zulip-root-projects-stable-all-channel-topic-and-dm-rows ()
  (zulip-root-test--with-account account
    (let ((buffer (zulip-root--open-buffer account)))
      (with-current-buffer buffer
        (let* ((first (zulip-root--project-entries))
               (second (zulip-root--project-entries))
               (actionable-keys
                (mapcar #'zulip-root--entry-key
                        (seq-filter #'zulip-root--entry-target first))))
          (should (equal actionable-keys
                         '((feed all)
                           (feed mentioned)
                           (feed starred)
                           (channel . "5")
                           (topic "5" "one")
                           (dm "1" "2"))))
          (should (equal (mapcar #'zulip-root--entry-key first)
                         (mapcar #'zulip-root--entry-key second)))
          (should
           (equal
            (zulip-narrow-key
             (zulip-root--entry-target
              (zulip-root-test--entry first '(feed all))))
            nil))
          (should
           (equal
            (zulip-narrow-key
             (zulip-root--entry-target
              (zulip-root-test--entry first '(feed mentioned))))
            '((is . "mentioned"))))
          (should
           (equal
            (zulip-narrow-key
             (zulip-root--entry-target
              (zulip-root-test--entry first '(feed starred))))
            '((is . "starred"))))
          (should
           (equal
            (zulip-narrow-key
             (zulip-root--entry-target
              (zulip-root-test--entry first '(channel . "5"))))
            '((channel . "general"))))
          (should
           (equal
            (zulip-narrow-key
             (zulip-root--entry-target
              (zulip-root-test--entry first '(topic "5" "one"))))
            '((channel . "general") (topic . "One"))))
          (should
           (equal
            (zulip-narrow-key
             (zulip-root--entry-target
              (zulip-root-test--entry first '(dm "1" "2"))))
            '((dm 2))))
          (zulip-root--request-render)
          (should (equal actionable-keys
                         (mapcar #'zulip-root--entry-key
                                 (seq-filter #'zulip-root--entry-target
                                             (zulip-root--project-entries))))))))))

(ert-deftest zulip-root-projects-exact-unread-and-mention-counts ()
  (zulip-root-test--with-account account
    (with-temp-buffer
      (setq-local zulip-root--account account)
      (setq-local zulip-root--fill-column 80)
      (let* ((entries (zulip-root--project-entries))
             (all (zulip-root-test--entry entries '(feed all)))
             (mentioned (zulip-root-test--entry entries '(feed mentioned)))
             (starred (zulip-root-test--entry entries '(feed starred)))
             (channel (zulip-root-test--entry entries '(channel . "5")))
             (topic (zulip-root-test--entry entries '(topic "5" "one")))
             (dm (zulip-root-test--entry entries '(dm "1" "2"))))
        (should (= 3 (zulip-root--entry-unread-count all)))
        (should (= 2 (zulip-root--entry-mention-count all)))
        (should (= 2 (zulip-root--entry-unread-count mentioned)))
        (should (= 2 (zulip-root--entry-mention-count mentioned)))
        (should (= 1 (zulip-root--entry-unread-count starred)))
        (should (= 1 (zulip-root--entry-mention-count starred)))
        (should (= 2 (zulip-root--entry-unread-count channel)))
        (should (= 1 (zulip-root--entry-mention-count channel)))
        (should (= 2 (zulip-root--entry-unread-count topic)))
        (should (= 1 (zulip-root--entry-mention-count topic)))
        (should (= 1 (zulip-root--entry-unread-count dm)))
        (should (= 1 (zulip-root--entry-mention-count dm)))))))

(ert-deftest zulip-root-caps-topics-by-usefulness-and-opaque-recency ()
  (let ((zulip-root-visible-topics-per-channel 2))
    (zulip-root-test--with-account account
      (puthash
       "5"
       (list
        '(:name "Archive" :max-message-id "3")
        '(:name "Near" :max-message-id "90071992547409931234")
        '(:name "Newest" :max-message-id "90071992547409931235"))
       (zulip-root--topic-cache account))
      (with-temp-buffer
        (setq-local zulip-root--account account)
        (setq-local zulip-root--fill-column 80)
        (let* ((first (zulip-root--project-entries))
               (second (zulip-root--project-entries))
               (topics (zulip-root-test--topic-entries first))
               (hidden (zulip-root-test--entry
                        first '(topics-hidden "5"))))
          ;; The mentioned/unread topic is protected; the sole remaining slot
          ;; goes to the newest opaque decimal max-message-id.
          (should (equal '("One" "Newest")
                         (mapcar #'zulip-root--entry-title topics)))
          (should (zulip-root--entry-p hidden))
          (should (string-match-p "2 older topics hidden"
                                  (zulip-root--entry-title hidden)))
          (should
           (equal (mapcar #'zulip-root--entry-key first)
                  (mapcar #'zulip-root--entry-key second))))))))

(ert-deftest zulip-root-topic-cap-never-hides-unread-topics ()
  (let ((zulip-root-visible-topics-per-channel 0))
    (zulip-root-test--with-account account
      (let* ((state (zulip-account-state account))
             (next
              (zulip-state-upsert-message
               state
               (zulip-root-test--message
                93 "stream" "overflow unread" "Overflow" 5 "general"))))
        (setq next
              (zulip-state-set-message-unread
               next "93" t
               '((type . "stream")
                 (stream_id . 5)
                 (topic . "Overflow")
                 (mentioned . :false)
                 (unmuted_stream_msg . t))))
        (zulip-runtime-publish-state account next))
      (with-temp-buffer
        (setq-local zulip-root--account account)
        (setq-local zulip-root--fill-column 80)
        (let ((keys
               (mapcar #'zulip-root--entry-key
                       (zulip-root-test--topic-entries
                        (zulip-root--project-entries)))))
          ;; Both protected rows exceed the configured zero ordinary rows.
          (should (equal '((topic "5" "one")
                           (topic "5" "overflow"))
                         keys)))))))

(ert-deftest zulip-root-hidden-topics-remain-discoverable ()
  (let ((zulip-root-visible-topics-per-channel 1))
    (zulip-root-test--with-account account
      (puthash
       "5"
       (list '(:name "Hidden topic" :max-message-id "100")
             '(:name "Another hidden topic" :max-message-id "99"))
       (zulip-root--topic-cache account))
      (with-temp-buffer
        (setq-local zulip-root--account account)
        (setq-local zulip-root--fill-column 80)
        (should-not
         (zulip-root-test--entry
          (zulip-root--project-entries) '(topic "5" "hidden topic")))
        (should
         (zulip-root-test--entry
          (zulip-root--project-entries t) '(topic "5" "hidden topic")))
        (should
         (assoc "Topic: general > Hidden topic"
                (zulip-root--completion-choices)))
        (let (opened-narrow)
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (prompt collection &rest _arguments)
                       (if (string-prefix-p "Channel" prompt)
                           "general"
                         (should (member "Hidden topic" collection))
                         "Hidden topic")))
                    ((symbol-function 'zulip-feed-open)
                     (lambda (_account narrow)
                       (setq opened-narrow narrow))))
            (call-interactively #'zulip-root-open-topic))
          (should (equal '((channel . "general")
                           (topic . "Hidden topic"))
                         (zulip-narrow-key opened-narrow))))))))

(ert-deftest zulip-root-large-realm-bounds-default-rendered-topic-rows ()
  (let ((zulip-root-visible-topics-per-channel 25)
        (channel-count 32)
        (topics-per-channel 400))
    (zulip-root-test--with-account account
      (zulip-runtime-publish-state
       account
       (zulip-state-from-register
        (zulip-root-test--register-with-streams channel-count)))
      (dotimes (offset channel-count)
        (let ((channel-id (format "%d" (+ 5 offset))))
          (puthash
           channel-id
           (cl-loop for index below topics-per-channel
                    collect
                    (list :name (format "Topic %03d" index)
                          :max-message-id
                          (format "%d"
                                  (+ 1000000
                                     (* offset topics-per-channel)
                                     index))))
           (zulip-root--topic-cache account))))
      (let ((buffer (zulip-root--open-buffer account)))
        (with-current-buffer buffer
          (let* ((entries (zulip-root--project-entries))
                 (topic-count
                  (length (zulip-root-test--topic-entries entries)))
                 (hidden-note-count
                  (seq-count
                   (lambda (entry)
                     (eq (car-safe (zulip-root--entry-key entry))
                         'topics-hidden))
                   entries)))
            ;; This mirrors the observed 32-channel/12,800-topic realm.  The
            ;; complete cache is processed, but only the bounded root projection
            ;; becomes EWOC nodes and buffer lines.
            (should (= (* channel-count 25) topic-count))
            (should (= channel-count hidden-note-count))
            (should (< (length entries) 900))
            (should (< (line-number-at-pos (point-max)) 900))))))))

(ert-deftest zulip-root-message-preview-decodes-entities-and-flattens-blocks ()
  (let ((message
         (zulip-root-test--message
          12 "stream"
          (concat "<p>Hello &amp; &lt;friends&gt;</p>"
                  "<blockquote>Second <strong>block</strong></blockquote>")
          "One" 5 "general")))
    (should (equal "Two: Hello & <friends> Second block"
                   (zulip-root--message-preview message)))))

(ert-deftest zulip-root-reuses-an-account-scoped-generated-surface ()
  (zulip-root-test--with-account account
    (let* ((first-buffer (zulip-root--open-buffer account))
           (first-surface (with-current-buffer first-buffer (appkit-current-surface)))
           (second-buffer (zulip-root--open-buffer account))
           (other (zulip-runtime-create-account
                   :server "https://other.example.test/"
                   :email "me@example.test" :api-key "secret"
                   :state (zulip-state-from-register (zulip-root-test--register))))
           (other-buffer (zulip-root--open-buffer other)))
      (should (eq first-buffer second-buffer))
      (should (eq first-surface
                  (appkit-app-surface (zulip-account-app account)
                                      (zulip-root--view-id account))))
      (should-not (eq first-buffer other-buffer))
      (should-not (eq first-surface
                      (with-current-buffer other-buffer (appkit-current-surface)))))))

(ert-deftest zulip-root-state-change-refreshes-only-its-view-and-keeps-position ()
  (zulip-root-test--with-account account
    (let* ((other (zulip-runtime-create-account
                   :server "https://other.example.test/" :email "me@example.test"
                   :api-key "secret"
                   :state (zulip-state-from-register (zulip-root-test--register))))
           (buffer (zulip-root--open-buffer account))
           (other-buffer (zulip-root--open-buffer other))
           (other-content (with-current-buffer other-buffer (buffer-string)))
           (state (zulip-account-state account))
           (next (zulip-state-upsert-message
                  state (zulip-root-test--message
                         93 "stream" "new unread" "One" 5 "general")))
           old-column)
      (setq next (zulip-state-set-message-unread
                  next "93" t
                  '((type . "stream") (stream_id . 5) (topic . "One")
                    (mentioned . :false) (unmuted_stream_msg . t))))
      (with-current-buffer buffer
        (goto-char (zulip-root-test--row-position '(topic "5" "one")))
        (move-to-column 6)
        (setq old-column (current-column)))
      (zulip-runtime-publish-state account next)
      (zulip-runtime-test--drain account)
      (with-current-buffer buffer
        (should (equal (get-text-property (point) appkit-directory-key-property)
                       '(topic "5" "one")))
        (should (= old-column (current-column)))
        (should (= 3 (get-text-property (point) 'zulip-root-unread-count))))
      (should (equal other-content
                     (with-current-buffer other-buffer (buffer-string)))))))

(ert-deftest zulip-root-hydrates-server-topics-once-with-view-ownership ()
  (zulip-root-test--with-account account
    (let (calls buffer view)
      (cl-letf (((symbol-function 'zulip-api-get-topics)
                 (lambda (candidate stream-id callback &rest options)
                   (let* ((owner (plist-get options :owner))
                          (process (make-pipe-process :name "zulip-root-test-transport"
                                                      :buffer nil :noquery t))
                          (handle (appkit-register-handle owner 'process process))
                          (response callback)
                          (callback (lambda (result)
                                      (appkit-retire-handle handle)
                                      (when (process-live-p process) (delete-process process))
                                      ;; Deliver even after cancellation to exercise runtime fencing.
                                      (funcall response result))))
                     (push process zulip-root-test--processes)
                     (should (eq (zulip-account-app candidate) (appkit-owner-app owner)))
                     (push (list candidate stream-id callback
                                 (plist-get options :owner))
                           calls)
                     handle))))
        (setq buffer (zulip-root--open-buffer account)
              view (with-current-buffer buffer (appkit-current-surface)))
        (should (= 1 (length calls)))
        (pcase-let ((`(,candidate ,stream-id ,_callback ,owner)
                     (car calls)))
          (should (eq account candidate))
          (should (= 5 stream-id))
          (should (eq view owner)))
        ;; Reprojection and repeated hydration cannot duplicate an in-flight
        ;; per-channel request.
        (with-current-buffer buffer
          (should (= 0 (zulip-root--hydrate-topics)))
          (zulip-root--request-render))
        (should (= 1 (length calls)))
        (funcall
         (nth 2 (car calls))
         (zulip-root-test--topics-result
          '((name . "Server only")
            (max_id . "90071992547409931234"))
          '((name . "server ONLY")
            (max_id . "90071992547409939999"))))
        (zulip-runtime-test--drain account)
        (let* ((cached (gethash "5" (zulip-root--topic-cache account)))
               (model (car cached)))
          (should (= 1 (length cached)))
          (should (equal "Server only" (plist-get model :name)))
          (should (equal "90071992547409931234"
                         (plist-get model :max-message-id))))
        (with-current-buffer buffer
          (let ((entry
                 (zulip-root-test--entry
                  (zulip-root--project-entries)
                  '(topic "5" "server only"))))
            (should (zulip-root--entry-p entry))
            (should (equal "Server only" (zulip-root--entry-title entry)))
            (should (equal '((channel . "general")
                             (topic . "Server only"))
                           (zulip-narrow-key
                            (zulip-root--entry-target entry))))))))))

(ert-deftest zulip-root-topic-hydration-is-fifo-and-concurrency-limited ()
  (let ((zulip-root-topic-hydration-concurrency 2))
    (zulip-root-test--with-account account
      (zulip-runtime-publish-state
       account (zulip-state-from-register (zulip-root-test--register-with-streams 5)))
      (let (calls buffer)
        (cl-letf (((symbol-function 'zulip-api-get-topics)
                   (lambda (_account stream-id callback &rest options)
                     (let* ((owner (plist-get options :owner))
                            (process (make-pipe-process :name "zulip-root-test-transport"
                                                        :buffer nil :noquery t))
                            (handle (appkit-register-handle owner 'process process))
                            (response callback)
                            (callback (lambda (result)
                                        (appkit-retire-handle handle)
                                        (when (process-live-p process) (delete-process process))
                                        ;; Deliver even after cancellation to exercise runtime fencing.
                                        (funcall response result))))
                       (push process zulip-root-test--processes)
                       (should (eq (zulip-account-app _account) (appkit-owner-app owner)))
                       (setq calls (append calls (list (list stream-id callback
                                                             (plist-get options :owner)))))
                       handle))))
          (setq buffer (zulip-root--open-buffer account))
          (should (equal '(5 6) (mapcar #'car calls)))
          (funcall (nth 1 (car calls))
                   (zulip-root-test--topics-result '((name . "first-result"))))
          (zulip-runtime-test--drain account)
          (should (equal '(5 6 7) (mapcar #'car calls)))
          (funcall (nth 1 (car calls))
                   (zulip-root-test--topics-result '((name . "must-not-land"))))
          (zulip-runtime-test--drain account)
          (should (equal '(5 6 7) (mapcar #'car calls)))
          (should (equal "first-result"
                         (plist-get (car (gethash "5" (zulip-root--topic-cache account)))
                                    :name))))))))

(ert-deftest zulip-root-topic-prune-does-not-start-discarded-waiting-work ()
  (let ((zulip-root-topic-hydration-concurrency 4))
    (zulip-root-test--with-account account
      (zulip-runtime-publish-state
       account
       (zulip-state-from-register
        (zulip-root-test--register-with-streams 32)))
      (let (calls buffer)
        (cl-letf (((symbol-function 'zulip-api-get-topics)
                   (lambda (_account stream-id callback &rest options)
                     (let* ((owner (plist-get options :owner))
                            (process (make-pipe-process :name "zulip-root-test-transport"
                                                        :buffer nil :noquery t))
                            (handle (appkit-register-handle owner 'process process))
                            (response callback)
                            (callback (lambda (result)
                                        (appkit-retire-handle handle)
                                        (when (process-live-p process) (delete-process process))
                                        ;; Deliver even after cancellation to exercise runtime fencing.
                                        (funcall response result))))
                       (push process zulip-root-test--processes)
                       (should (eq (zulip-account-app _account) (appkit-owner-app owner)))
                       (setq calls
                             (append calls (list (cons stream-id callback))))
                       handle))))
          (setq buffer (zulip-root--open-buffer account))
          (should (= 4 (length calls)))
          (should (cl-every #'process-live-p zulip-root-test--processes))
          (with-current-buffer buffer
            (zulip-root--prune-topic-tasks
             (make-hash-table :test #'equal))
            (should (= 0 (appkit-task-queue-total-count
                          zulip-root--topic-tasks))))
          ;; Batch retirement removes every waiting task before active
          ;; cancellation can pump the queue.
          (should (= 4 (length calls)))
          (should-not (cl-some #'process-live-p zulip-root-test--processes))
          (dolist (call calls)
            (funcall (cdr call)
                     (zulip-root-test--topics-result '((name . "discarded")))))
          (zulip-runtime-test--drain account)
          (should (= 4 (length calls)))
          (should (= 0 (hash-table-count (zulip-root--topic-cache account)))))))))

(ert-deftest zulip-root-topic-hydration-force-deduplicates-active-and-queue ()
  (let ((zulip-root-topic-hydration-concurrency 2))
    (zulip-root-test--with-account account
      (zulip-runtime-publish-state
       account
       (zulip-state-from-register
        (zulip-root-test--register-with-streams 5)))
      (dolist (stream-id '("5" "6" "7" "8" "9"))
        (puthash stream-id (list (list :name "already cached"))
                 (zulip-root--topic-cache account)))
      (let (calls buffer)
        (cl-letf (((symbol-function 'zulip-api-get-topics)
                   (lambda (_account stream-id callback &rest options)
                     (let* ((owner (plist-get options :owner))
                            (process (make-pipe-process :name "zulip-root-test-transport"
                                                        :buffer nil :noquery t))
                            (handle (appkit-register-handle owner 'process process))
                            (response callback)
                            (callback (lambda (result)
                                        (appkit-retire-handle handle)
                                        (when (process-live-p process) (delete-process process))
                                        ;; Deliver even after cancellation to exercise runtime fencing.
                                        (funcall response result))))
                       (push process zulip-root-test--processes)
                       (should (eq (zulip-account-app _account) (appkit-owner-app owner)))
                       (setq calls
                             (append calls
                                     (list (cons stream-id callback))))
                       handle))))
          (setq buffer (zulip-root--open-buffer account))
          (should-not calls)
          (with-current-buffer buffer
            (should (= 0 (zulip-root--hydrate-topics)))
            (should (= 5 (zulip-root--hydrate-topics t)))
            (should (= 0 (zulip-root--hydrate-topics t)))
            (should (= 2 (appkit-task-queue-active-count
                          zulip-root--topic-tasks)))
            (should (= 3 (appkit-task-queue-queued-count
                          zulip-root--topic-tasks))))
          (should (equal '(5 6) (mapcar #'car calls)))
          (progn
            ;; The callback for each live request starts the next FIFO item.
            ;; CALLS grows while this loop advances through all five channels.
            (let ((index 0))
              (while (< index 5)
                (funcall (cdr (nth index calls))
                         (zulip-root-test--topics-result
                          `((name . ,(format "fresh-%d" index)))))
                (zulip-runtime-test--drain account)
                (cl-incf index))))
          (should (equal '(5 6 7 8 9) (mapcar #'car calls)))
          (with-current-buffer buffer
            (should-not
             (appkit-task-queue-pending-p zulip-root--topic-tasks))
            ;; Once the previous forced generation is complete, a new force
            ;; refresh schedules every subscribed channel again.
            (should (= 5 (zulip-root--hydrate-topics t)))
            (should (= 2 (appkit-task-queue-active-count
                          zulip-root--topic-tasks)))
            (should (= 3 (appkit-task-queue-queued-count
                          zulip-root--topic-tasks))))
          (should (equal '(5 6 7 8 9 5 6) (mapcar #'car calls))))))))

(ert-deftest zulip-root-killed-topic-scheduler-cannot-land-or-pump ()
  (let ((zulip-root-topic-hydration-concurrency 1))
    (zulip-root-test--with-account account
      (zulip-runtime-publish-state
       account
       (zulip-state-from-register
        (zulip-root-test--register-with-streams 3)))
      (let (calls buffer old-view new-view old-callback)
        (cl-letf (((symbol-function 'zulip-api-get-topics)
                   (lambda (_account stream-id callback &rest options)
                     (let* ((owner (plist-get options :owner))
                            (process (make-pipe-process :name "zulip-root-test-transport"
                                                        :buffer nil :noquery t))
                            (handle (appkit-register-handle owner 'process process))
                            (response callback)
                            (callback (lambda (result)
                                        (appkit-retire-handle handle)
                                        (when (process-live-p process) (delete-process process))
                                        ;; Deliver even after cancellation to exercise runtime fencing.
                                        (funcall response result))))
                       (push process zulip-root-test--processes)
                       (should (eq (zulip-account-app _account) (appkit-owner-app owner)))
                       (setq calls
                             (append calls
                                     (list (list :stream-id stream-id
                                                 :callback callback
                                                 :owner (plist-get options
                                                                   :owner)))))
                       handle))))
          (setq buffer (zulip-root--open-buffer account)
                old-view (with-current-buffer buffer (appkit-current-surface))
                old-callback (plist-get (car calls) :callback))
          (should (= 1 (length calls)))
          (should (cl-every #'process-live-p zulip-root-test--processes))
          (appkit-surface-stop old-view)
          (should-not (cl-some #'process-live-p zulip-root-test--processes))
          (funcall old-callback
                   (zulip-root-test--topics-result
                    '((name . "old-view-result"))))
          (zulip-runtime-test--drain account)
          (should (= 1 (length calls)))
          (should-not (gethash "5" (zulip-root--topic-cache account)))
          ;; Reattaching resets both the active set and FIFO.  A completion
          ;; carrying the detached view's token remains inert afterward.
          (setq buffer (zulip-root--open-buffer account)
                new-view (with-current-buffer buffer (appkit-current-surface)))
          (should-not (eq old-view new-view))
          (should (= 2 (length calls)))
          (should (eq new-view (plist-get (nth 1 calls) :owner)))
          (funcall old-callback
                   (zulip-root-test--topics-result
                    '((name . "still-must-not-land"))))
          (zulip-runtime-test--drain account)
          (should (= 2 (length calls)))
          (should-not (gethash "5" (zulip-root--topic-cache account)))
          (progn
            (funcall (plist-get (nth 1 calls) :callback)
                     (zulip-root-test--topics-result
                      '((name . "new-view-result"))))
            (zulip-runtime-test--drain account))
          (should (= 3 (length calls)))
          (should (equal "new-view-result"
                         (plist-get
                          (car (gethash "5"
                                        (zulip-root--topic-cache account)))
                          :name))))))))

(ert-deftest zulip-root-killed-view-cannot-land-topic-response ()
  (zulip-root-test--with-account account
    (let (callback buffer view)
      (cl-letf (((symbol-function 'zulip-api-get-topics)
                 (lambda (_account _stream-id candidate &rest options)
                   (let* ((owner (plist-get options :owner))
                          (process (make-pipe-process :name "zulip-root-test-transport"
                                                      :buffer nil :noquery t))
                          (handle (appkit-register-handle owner 'process process))
                          (response candidate)
                          (candidate (lambda (result)
                                       (appkit-retire-handle handle)
                                       (when (process-live-p process) (delete-process process))
                                      ;; Deliver even after cancellation to exercise runtime fencing.
                                       (funcall response result))))
                     (push process zulip-root-test--processes)
                     (should (eq (zulip-account-app _account) (appkit-owner-app owner)))
                     (setq callback candidate)
                     handle))))
        (setq buffer (zulip-root--open-buffer account)
              view (with-current-buffer buffer (appkit-current-surface)))
        (should (functionp callback))
        (should (cl-every #'process-live-p zulip-root-test--processes))
        (appkit-surface-stop view)
        (should-not (cl-some #'process-live-p zulip-root-test--processes))
        (funcall callback
                 (zulip-root-test--topics-result
                  '((name . "Must not land")
                    (max_id . "90071992547409931234"))))
        (zulip-runtime-test--drain account)
        (should-not (gethash "5" (zulip-root--topic-cache account)))))))

(ert-deftest zulip-root-refresh-refetches-and-errors-preserve-cache ()
  (zulip-root-test--with-account account
    (let (calls buffer)
      (cl-letf (((symbol-function 'zulip-api-get-topics)
                 (lambda (_account stream-id callback &rest options)
                   (let* ((owner (plist-get options :owner))
                          (process (make-pipe-process :name "zulip-root-test-transport"
                                                      :buffer nil :noquery t))
                          (handle (appkit-register-handle owner 'process process))
                          (response callback)
                          (callback (lambda (result)
                                      (appkit-retire-handle handle)
                                      (when (process-live-p process) (delete-process process))
                                      ;; Deliver even after cancellation to exercise runtime fencing.
                                      (funcall response result))))
                     (push process zulip-root-test--processes)
                     (should (eq (zulip-account-app _account) (appkit-owner-app owner)))
                     (push (list stream-id callback
                                 (plist-get options :owner))
                           calls)
                     handle))))
        (setq buffer (zulip-root--open-buffer account))
        (should (= 1 (length calls)))
        (funcall (nth 1 (car calls))
                 (zulip-root-test--topics-result
                  '((name . "Cached from server")
                    (max_id . "90071992547409931234"))))
        (zulip-runtime-test--drain account)
        (with-current-buffer buffer
          ;; A populated cache does not suppress an explicit `g' refresh.
          (zulip-root-refresh))
        (should (= 2 (length calls)))
        (funcall (nth 1 (car calls))
                 (zulip-api-result--create
                  :ok-p nil :status 503 :code "SERVICE_UNAVAILABLE"))
        (zulip-runtime-test--drain account)
        (should (equal "Cached from server"
                       (plist-get
                        (car (gethash "5"
                                      (zulip-root--topic-cache account)))
                        :name)))
        (with-current-buffer buffer
          (let* ((entries (zulip-root--project-entries))
                 (topic (zulip-root-test--entry
                         entries '(topic "5" "cached from server")))
                 (note (zulip-root-test--entry
                        entries 'topic-hydration-note)))
            (should (zulip-root--entry-p topic))
            (should (string-match-p "failed"
                                    (zulip-root--entry-title note)))))))))

(ert-deftest zulip-root-topic-cache-is-account-scoped ()
  (zulip-root-test--with-account account
    (let ((other
           (zulip-runtime-create-account
            :server "https://other-root.example.test/"
            :email "me@example.test"
            :api-key "secret"
            :state (zulip-state-from-register
                    (zulip-root-test--register)))))
      (puthash "5" (list (list :name "First"))
               (zulip-root--topic-cache account))
      (puthash "5" (list (list :name "Second"))
               (zulip-root--topic-cache other))
      (should (equal "First"
                     (plist-get (car (gethash
                                      "5" (zulip-root--topic-cache account)))
                                :name)))
      (should (equal "Second"
                     (plist-get (car (gethash
                                      "5" (zulip-root--topic-cache other)))
                                :name))))))

(ert-deftest zulip-root-opens-readable-new-dm-and-search-narrows ()
  (zulip-root-test--with-account account
    (let (opened-account opened-narrow)
      (cl-letf (((symbol-function 'zulip-feed-open)
                 (lambda (candidate narrow)
                   (setq opened-account candidate
                         opened-narrow narrow)))
                ((symbol-function 'completing-read-multiple)
                 (lambda (&rest _arguments) '("Two"))))
        (with-temp-buffer
          (setq-local zulip-root--account account)
          (zulip-root-open-new-direct-message))
        (should (eq opened-account account))
        (should (equal (zulip-narrow-recipient-ids opened-narrow) '(2)))
        (should (equal (zulip-narrow-title opened-narrow) "Two"))
        (with-temp-buffer
          (setq-local zulip-root--account account)
          (zulip-root-search-messages "  release plan  "))
        (should (eq (zulip-narrow-kind opened-narrow) 'search))
        (should (equal (zulip-narrow-search-query opened-narrow)
                       "release plan"))))))

(ert-deftest zulip-root-destination-discovery-includes-new-dm-and-search ()
  (zulip-root-test--with-account account
    (let (opened-narrow)
      (cl-letf (((symbol-function 'zulip-feed-open)
                 (lambda (_account narrow) (setq opened-narrow narrow)))
                ((symbol-function 'completing-read)
                 (lambda (_prompt collection &rest _arguments)
                   (should (member "New direct message…" collection))
                   (should (member "Search messages…" collection))
                   "Search messages…"))
                ((symbol-function 'read-string)
                 (lambda (&rest _arguments) "needle")))
        (with-temp-buffer
          (setq-local zulip-root--account account)
          (call-interactively #'zulip-root-open-destination))
        (should (eq (zulip-narrow-kind opened-narrow) 'search))
        (should (equal (zulip-narrow-search-query opened-narrow)
                       "needle"))))))

(provide 'zulip-root-test)

;;; zulip-root-test.el ends here
