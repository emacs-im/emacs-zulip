;;; zulip-feed-test.el --- Tests for Zulip narrow/feed slice -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'zulip-runtime-test)
(require 'cl-lib)
(require 'zulip-http)
(require 'zulip-feed)

(defun zulip-feed-test--message (id content &optional local-id)
  "Return a small canonical channel message."
  (append
   (list (cons 'id id)
         (cons 'type "stream")
         (cons 'stream_id 7)
         (cons 'display_recipient "engineering")
         (cons 'subject "client")
         (cons 'sender_full_name "Ada")
         (cons 'content content)
         (cons 'timestamp 1))
   (and local-id (list (cons 'local_message_id local-id)))))

(defmacro zulip-feed-test--with-account (binding &rest body)
  "Create isolated account BINDING and retire only its fixture buffers."
  (declare (indent 1) (debug (symbolp body)))
  `(let* ((zulip-runtime--accounts (make-hash-table :test #'equal))
          (zulip-runtime-change-hook nil)
          (state (zulip-state-create))
          (,binding (zulip-runtime-create-account
                     :server "https://chat.example.test/"
                     :email "ada@example.test" :api-key "secret" :state state))
          (zulip-feed-test--open-function (symbol-function 'zulip-feed--open-buffer))
          zulip-feed-test--buffers)
     (setf (zulip-account-queue-id ,binding) "queue-1")
     (unwind-protect
         (cl-letf (((symbol-function 'zulip-feed--open-buffer)
                    (lambda (&rest arguments)
                      (let ((buffer (apply zulip-feed-test--open-function arguments)))
                        (cl-pushnew buffer zulip-feed-test--buffers)
                        buffer))))
           ,@body)
       (zulip-runtime-stop-all)
       (dolist (buffer zulip-feed-test--buffers)
         (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest zulip-narrow-has-stable-key-and-wire-json ()
  (let ((all (zulip-narrow-all))
        (channel (zulip-narrow-channel 7))
        (topic (zulip-narrow-topic 7 "client"))
        (direct (zulip-narrow-direct '(9 3 9) "Ada, Grace"))
        (mentioned (zulip-narrow-mentioned))
        (starred (zulip-narrow-starred))
        (search (zulip-narrow-search "release notes")))
    (should-not (zulip-narrow-key all))
    (should (equal (zulip-narrow-key channel) '((channel . 7))))
    (should (equal (zulip-narrow-key topic)
                   '((channel . 7) (topic . "client"))))
    (should (equal (zulip-narrow-key direct) '((dm 3 9))))
    (should (equal (zulip-narrow-key mentioned) '((is . "mentioned"))))
    (should (equal (zulip-narrow-key starred) '((is . "starred"))))
    (should (equal (zulip-narrow-key search) '((search . "release notes"))))
    (should (equal (zulip-narrow-api-json all) "[]"))
    (should (equal (zulip-narrow-api-json channel)
                   "[{\"operator\":\"channel\",\"operand\":7}]"))
    (should
     (equal (zulip-narrow-api-json topic)
            "[{\"operator\":\"channel\",\"operand\":7},{\"operator\":\"topic\",\"operand\":\"client\"}]"))
    (should (equal (zulip-narrow-api-json direct)
                   "[{\"operator\":\"dm\",\"operand\":[3,9]}]"))
    (should (equal (zulip-narrow-api-json mentioned)
                   "[{\"operator\":\"is\",\"operand\":\"mentioned\"}]"))
    (should (equal (zulip-narrow-api-json starred)
                   "[{\"operator\":\"is\",\"operand\":\"starred\"}]"))
    (should (equal (zulip-narrow-api-json search)
                   "[{\"operator\":\"search\",\"operand\":\"release notes\"}]"))
    (should (equal (zulip-narrow-title direct) "Ada, Grace"))
    (should-error (zulip-narrow-search "   "))))

(ert-deftest zulip-narrow-json-does-not-require-native-json ()
  (cl-letf (((symbol-function 'json-serialize)
             (lambda (&rest _arguments)
               (error "native JSON unavailable"))))
    (should
     (equal (zulip-narrow-api-json (zulip-narrow-topic 7 "client"))
            "[{\"operator\":\"channel\",\"operand\":7},{\"operator\":\"topic\",\"operand\":\"client\"}]"))))

(ert-deftest zulip-feed-special-narrows-choose-relevant-unread-anchor ()
  (zulip-feed-test--with-account account
    (let* ((mentioned
            (append (zulip-feed-test--message "11" "ping")
                    '((flags . ["mentioned"]))))
           (starred
            (append (zulip-feed-test--message "20" "saved")
                    '((flags . ["starred"]))))
           (state (zulip-state-merge-messages
                   (zulip-account-state account)
                   (list mentioned starred) nil))
           anchors)
      (dolist (id '("11" "20"))
        (setq state
              (zulip-state-set-message-unread
               state id t
               '((kind . channel) (channel-id . "7")
                 (topic . "client")
                 (mentioned . t)))))
      (zulip-runtime-publish-state account state)
      (with-temp-buffer
        (setq-local zulip-feed--account account)
        (cl-letf (((symbol-function 'zulip-feed--load-history)
                   (lambda (_kind anchor _before _after)
                     (push anchor anchors))))
          (dolist (narrow (list (zulip-narrow-mentioned)
                                (zulip-narrow-starred)
                                (zulip-narrow-search "needle")))
            (setq-local zulip-feed--narrow narrow)
            (zulip-feed-load-initial)))
        (should (equal (nreverse anchors)
                       '("first_unread" "first_unread" "newest")))))))

(ert-deftest zulip-feed-case-insensitive-comparison-supports-emacs-27 ()
  (should (zulip-feed--string-equal-ignore-case "Engineering" "engineering"))
  (should-not (zulip-feed--string-equal-ignore-case "one" "two"))
  (should-not (zulip-feed--string-equal-ignore-case nil "nil")))

(ert-deftest zulip-feed-direct-context-uses-human-readable-title ()
  (zulip-feed-test--with-account account
    (let* ((state
            (zulip-state-from-register
             '((user_id . 1)
               (realm_users
                . [((user_id . 1) (full_name . "Me"))
                   ((user_id . 2) (full_name . "Two"))]))))
           (recipients
            [((id . 1) (full_name . "Me"))
             ((id . 2) (full_name . "Two"))])
           (message
            `((id . 90) (type . "private") (sender_id . 2)
              (display_recipient . ,recipients)))
           opened)
      (zulip-runtime-publish-state account state)
      (with-temp-buffer
        (setq-local zulip-feed--account account)
        (setq-local zulip-feed--narrow (zulip-narrow-all))
        (should (equal (zulip-feed--message-context-label message) "Two"))
        (cl-letf (((symbol-function 'zulip-feed-open)
                   (lambda (_account narrow) (setq opened narrow))))
          (zulip-feed-open-message-context message))
        (should (equal (zulip-narrow-recipient-ids opened) '(2)))
        (should (equal (zulip-narrow-title opened) "Two"))))))

(ert-deftest zulip-feed-projects-exact-state-window-into-timeline ()
  (zulip-feed-test--with-account account
    (let* ((narrow (zulip-narrow-topic 7 "client"))
           (key (zulip-narrow-key narrow))
           (messages (list (zulip-feed-test--message "11" "one")
                           (zulip-feed-test--message "20" "two")))
           (state (zulip-state-merge-messages
                   (zulip-account-state account) messages key)))
      (zulip-runtime-publish-state account state)
      (let ((buffer (zulip-feed--open-buffer account narrow)))
        (with-current-buffer buffer
          (appkit-chat-history-window-set "11" nil)
          (zulip-feed-render)
          (should (equal (appkit-chat-timeline-keys) '("11" "20")))
          (should (string-match-p "chat\\.example\\.test" (buffer-name)))
          (should (string-match-p "Ada" (buffer-string)))
          (should (appkit-chatbuf-input-start-position)))))))

(ert-deftest zulip-feed-same-title-direct-narrows-coexist-by-stable-key ()
  (zulip-feed-test--with-account account
    (let* ((alex-two (zulip-narrow-direct '(2) "Alex"))
           (alex-three (zulip-narrow-direct '(3) "Alex"))
           (two-buffer (zulip-feed--open-buffer account alex-two))
           (three-buffer (zulip-feed--open-buffer account alex-three)))
      ;; The human title is presentation only.  Appkit identity and its
      ;; persistent fingerprint continue to use the exact narrow key.
      (should (equal (zulip-narrow-title alex-two)
                     (zulip-narrow-title alex-three)))
      (should-not (eq two-buffer three-buffer))
      (with-current-buffer two-buffer
        (should (equal (appkit-surface-identity (appkit-current-surface))
                       (zulip-feed--view-id account alex-two)))
        (should (equal (zulip-narrow-key zulip-feed--narrow)
                       '((dm 2)))))
      (with-current-buffer three-buffer
        (should (equal (appkit-surface-identity (appkit-current-surface))
                       (zulip-feed--view-id account alex-three)))
        (should (equal (zulip-narrow-key zulip-feed--narrow)
                       '((dm 3))))))))

(ert-deftest zulip-feed-history-load-uses-anchor-window-and-narrow-json ()
  (zulip-feed-test--with-account account
    (let* ((narrow (zulip-narrow-channel 7))
           (buffer (zulip-feed--open-buffer account narrow))
           call)
      (cl-letf (((symbol-function 'zulip-api-get-messages)
                 (lambda (_account wire anchor before after callback &rest _options)
                   (setq call (list wire anchor before after))
                   (funcall callback
                            (zulip-api-result--create
                             :ok-p t
                             :data
                             (list (cons 'messages
                                         (vector (zulip-feed-test--message "31" "history")))
                                   (cons 'found_oldest t)
                                   (cons 'found_newest t)))))))
        (with-current-buffer buffer
          (setq-local zulip-feed--pending-jump-id "31")
          (zulip-feed-load-latest)
          (zulip-runtime-test--drain account)
          (should (equal call
                         (list "[{\"operator\":\"channel\",\"operand\":7}]"
                               "newest" zulip-history-page-size 0)))
          (should (appkit-chat-history-window-known-p))
          (should (appkit-chat-history-older-loaded-p))
          (should (equal (appkit-chat-timeline-keys) '("31")))
          (should-not zulip-feed--pending-jump-id)
          (should (equal (zulip-feed-message-id-at-point) "31")))))))

(ert-deftest zulip-feed-defers-initial-history-until-register ()
  (zulip-feed-test--with-account account
    (let ((narrow (zulip-narrow-all))
          (calls 0)
          buffer)
      (cl-letf (((symbol-function 'zulip-api-get-messages)
                 (lambda (&rest _arguments)
                   (cl-incf calls))))
        (save-window-excursion
          (setq buffer (zulip-feed-open account narrow)))
        (should (= calls 0))
        (setf (zulip-account-connected-p account) t)
        (zulip-feed--publish-event
         account '((type . "register"))
         (zulip-account-state account) (zulip-state-create))
        (zulip-runtime-test--drain account)
        (should (= calls 1))
        (with-current-buffer buffer
          (should (eq (appkit-chat-history-loading) 'latest)))))))

(ert-deftest zulip-feed-stop-reopen-preserves-draft-and-fences-old-work ()
  (zulip-feed-test--with-account account
    (setf (zulip-account-connected-p account) t)
    (let* ((narrow (zulip-narrow-topic 7 "client"))
           (message (zulip-feed-test--message "20" "existing"))
           (draft (concat (appkit-chatbuf-input-object-string
                           "@Ada"
                           '(:kind zulip-mention :user-id "42" :full-name "Ada"
                             :wire "@**Ada|42**"))
                          " protected draft"))
           history old-history old-surface edit-response
           history-canceled edit-canceled buffer)
      (zulip-runtime-publish-state
       account (zulip-state-merge-messages
                (zulip-account-state account) (list message) (zulip-narrow-key narrow)))
      (cl-letf (((symbol-function 'zulip-api-get-messages)
                 (lambda (_account _narrow _anchor _before _after callback &rest options)
                   (let ((handle (appkit-register-handle
                                  (plist-get options :owner) 'function
                                  (lambda () (setq history-canceled t)))))
                     (push (lambda (result)
                             (appkit-retire-handle handle)
                             (funcall callback result)) history)
                     handle)))
                ((symbol-function 'zulip-api-get-message)
                 (lambda (_account _id callback &rest options)
                   (let ((handle (appkit-register-handle
                                  (plist-get options :owner) 'function
                                  (lambda () (setq edit-canceled t)))))
                     (setq edit-response
                           (lambda (result)
                             (appkit-retire-handle handle)
                             (funcall callback result)))
                     handle))))
        (setq buffer (zulip-feed--open-buffer account narrow))
        (with-current-buffer buffer
          (setq old-surface (appkit-current-surface))
          (appkit-chat-history-window-set "20" nil)
          (zulip-feed-render)
          (appkit-chatbuf-input-set-text draft)
          (zulip-feed--load-history 'older "20" 10 0)
          (setq old-history (car history))
          (zulip-feed-edit-message message))
        (zulip-runtime-test--drain account)
        (appkit-surface-stop old-surface)
        (should history-canceled)
        (should edit-canceled)
        (with-current-buffer buffer
          (should-not (appkit-current-surface))
          (should-not buffer-read-only)
          (should (equal (appkit-chatbuf-input-string) draft))
          (goto-char (point-max))
          (insert " continued"))
        (save-window-excursion
          (should (eq buffer (zulip-feed-open account narrow))))
        ;; A transport may already have queued either old completion.
        (funcall edit-response
                 (zulip-api-result--create :ok-p t :data '((raw_content . "stale edit"))))
        (funcall old-history
                 (zulip-api-result--create
                  :ok-p t :data (list (cons 'messages
                                            (list (zulip-feed-test--message "99" "stale page")))
                                      '(found_newest . t))))
        (zulip-runtime-test--drain account)
        (with-current-buffer buffer
          (should (equal (appkit-chatbuf-input-string) (concat draft " continued")))
          (should (eq (plist-get
                       (get-text-property 0 appkit-chatbuf-input-object-property
                                          (appkit-chatbuf-input-string)) :kind)
                      'zulip-mention)))
        (funcall (car history)
                 (zulip-api-result--create
                  :ok-p t :data (list (cons 'messages
                                            (list (zulip-feed-test--message "30" "current page")))
                                      '(found_newest . t))))
        (zulip-runtime-test--drain account)
        (with-current-buffer buffer
          (should-not (appkit-chat-history-loading-p))
          (should (equal (appkit-chat-timeline-keys) '("30")))
          (should (equal (appkit-chatbuf-input-string) (concat draft " continued"))))
        (should-not (zulip-state-message (zulip-account-state account) "99"))))))

(ert-deftest zulip-feed-event-first-send-rekeys-once ()
  (zulip-feed-test--with-account account
    (let* ((narrow (zulip-narrow-topic 7 "client"))
           (buffer (zulip-feed--open-buffer account narrow))
           send-callback
           send-options)
      (cl-letf (((symbol-function 'zulip-api-send-message)
                 (lambda (_account type to topic content callback &rest options)
                   (setq send-callback callback
                         send-options
                         (list type to topic content options)))))
        (with-current-buffer buffer
          (appkit-chat-history-window-establish-empty)
          (zulip-feed-render)
          (appkit-chatbuf-input-set-text "hello")
          (let* ((local-id (zulip-feed-send-message))
                 (server-id "90071992547409931234")
                 (server-message
                  (append
                   (zulip-feed-test--message server-id "hello" local-id)
                   (list (cons 'narrow-key (zulip-narrow-key narrow)))))
                 (old-state (zulip-account-state account))
                 (new-state
                  (zulip-state-upsert-message old-state server-message)))
            (should (equal (appkit-chat-timeline-keys) (list local-id)))
            (should (equal (nth 0 send-options) "stream"))
            (should (equal (nth 1 send-options) 7))
            (should (equal (nth 2 send-options) "client"))
            (should (equal (nth 3 send-options) "hello"))
            (should (equal (plist-get (nth 4 send-options) :local-id)
                           local-id))
            (should (equal (plist-get (nth 4 send-options) :queue-id)
                           "queue-1"))
            ;; The websocket event wins.  Its local_message_id replaces the
            ;; optimistic cache entry and drives one explicit Appkit rekey.
            (zulip-feed--publish-event
             account
             (list (cons 'type "message")
                   (cons 'message server-message)
                   (cons 'local_message_id local-id))
             old-state new-state)
            (zulip-runtime-test--drain account)
            (should (equal (appkit-chat-timeline-keys) (list server-id)))
            (should-not (zulip-state-message
                         (zulip-account-state account) local-id))
            (should (zulip-state-message
                     (zulip-account-state account) server-id))
            ;; The HTTP response is now stale but successful.  It must not
            ;; recreate either the local row or a duplicate server row.
            (funcall send-callback
                     (zulip-api-result--create
                      :ok-p t :data (list (cons 'id server-id))))
            (zulip-runtime-test--drain account)
            (should (equal (appkit-chat-timeline-keys) (list server-id)))
            (let ((entries
                   (zulip-state-messages-for-narrow
                    (zulip-account-state account)
                    (zulip-narrow-key narrow))))
              (should (= 1 (length entries))))))))))

(ert-deftest zulip-feed-response-first-send-keeps-authoritative-row ()
  (zulip-feed-test--with-account account
    (let* ((narrow (zulip-narrow-topic 7 "client"))
           (buffer (zulip-feed--open-buffer account narrow))
           send-callback)
      (cl-letf (((symbol-function 'zulip-api-send-message)
                 (lambda (_account _type _to _topic _content callback
                                   &rest _options)
                   (setq send-callback callback))))
        (with-current-buffer buffer
          (appkit-chat-history-window-establish-empty)
          (zulip-feed-render)
          (appkit-chatbuf-input-set-text
           "**bold** [link](https://example.com)")
          (let* ((local-id (zulip-feed-send-message))
                 (server-id "90071992547409931235")
                 (local-node (appkit-chat-timeline-node local-id)))
            (should (string-match-p
                     (regexp-quote "bold link")
                     (buffer-string)))
            ;; The HTTP response may win the race and promote the optimistic
            ;; object before the richer authoritative event arrives.
            (funcall send-callback
                     (zulip-api-result--create
                      :ok-p t :data (list (cons 'id server-id))))
            (zulip-runtime-test--drain account)
            (should (eq local-node
                        (appkit-chat-timeline-node server-id)))
            (should-not (zulip-state-message
                         (zulip-account-state account) local-id))
            (let* ((old-state (zulip-account-state account))
                   (server-message
                    (append
                     (zulip-feed-test--message server-id "bold link" local-id)
                     '((rendered_content
                        . "<p><strong>bold</strong> <a href=\"https://example.com\">link</a></p>")
                       (authoritative . t))))
                   (new-state
                    (zulip-state-upsert-message old-state server-message)))
              (zulip-feed--publish-event
               account
               (list (cons 'type "message")
                     (cons 'message server-message)
                     (cons 'local_message_id local-id))
               old-state new-state)
              (zulip-runtime-test--drain account))
            (should (eq local-node
                        (appkit-chat-timeline-node server-id)))
            (should (equal (appkit-chat-timeline-keys) (list server-id)))
            (should-not (zulip-state-message
                         (zulip-account-state account) local-id))
            (let* ((entries
                    (zulip-state-messages-for-narrow
                     (zulip-account-state account)
                     (zulip-narrow-key narrow)))
                   (message (car entries))
                   (bold-position
                    (save-excursion
                      (goto-char (point-min))
                      (search-forward "bold" nil t)))
                   (link-position
                    (save-excursion
                      (goto-char (point-min))
                      (search-forward "link" nil t))))
              (should (= 1 (length entries)))
              (should (zulip-state-object-get message 'authoritative))
              (should bold-position)
              (should link-position)
              (should (get-text-property (1- bold-position) 'face))
              (should
               (functionp
                (get-text-property
                 (1- link-position) appkit-ui-action-property)))
              (should (equal
                       (get-text-property
                        (1- link-position) zulip-feed--anchor-property)
                       server-id))
              (should (get-text-property
                       (1- link-position) 'read-only)))))))))

(ert-deftest zulip-feed-direct-pending-preserves-exact-participants ()
  (zulip-feed-test--with-account account
    (setf (zulip-state-self-user-id (zulip-account-state account)) "1")
    (let* ((narrow (zulip-narrow-direct '(2 3)))
           (key (zulip-narrow-key narrow))
           (buffer (zulip-feed--open-buffer account narrow)))
      (cl-letf (((symbol-function 'zulip-api-send-message)
                 (lambda (&rest _arguments) nil)))
        (with-current-buffer buffer
          (appkit-chat-history-window-establish-empty)
          (zulip-feed-render)
          (appkit-chatbuf-input-set-text "hello group")
          (let* ((local-id (zulip-feed-send-message))
                 (state (zulip-account-state account))
                 (message (zulip-state-message state local-id))
                 (conversation
                  (zulip-state-dm-conversation state '("1" "2" "3"))))
            (should (equal (zulip-state-object-get message 'recipients)
                           '(2 3)))
            (should (equal (zulip-state-message-ids state key)
                           (list local-id)))
            (should conversation)
            (should (equal
                     (zulip-dm-conversation-message-ids conversation)
                     (list local-id)))
            (should-not (zulip-state-dm-conversation state '("1")))))))))

(ert-deftest zulip-feed-send-response-survives-origin-view-kill ()
  (zulip-feed-test--with-account account
    (let* ((narrow (zulip-narrow-topic 7 "client"))
           (key (zulip-narrow-key narrow))
           (buffer (zulip-feed--open-buffer account narrow))
           callback
           local-id)
      (cl-letf (((symbol-function 'zulip-api-send-message)
                 (lambda (_account _type _to _topic _content then &rest _options)
                   (setq callback then))))
        (with-current-buffer buffer
          (appkit-chat-history-window-establish-empty)
          (zulip-feed-render)
          (appkit-chatbuf-input-set-text "survive")
          (setq local-id (zulip-feed-send-message)))
        (kill-buffer buffer)
        (funcall callback
                 (zulip-api-result--create
                  :ok-p t :data '((id . "91"))))
        (zulip-runtime-test--drain account)
        (should-not (zulip-state-message
                     (zulip-account-state account) local-id))
        (should (zulip-state-message (zulip-account-state account) "91"))
        (should (equal
                 (zulip-state-message-ids (zulip-account-state account) key)
                 '("91")))
        (let ((reopened (zulip-feed--open-buffer account narrow)))
          (with-current-buffer reopened
            (appkit-chat-history-window-set "91" nil)
            (zulip-feed-render)
            (should (equal (appkit-chat-timeline-keys) '("91")))))))))

(ert-deftest zulip-feed-send-failure-survives-origin-view-kill ()
  (zulip-feed-test--with-account account
    (let* ((narrow (zulip-narrow-topic 7 "client"))
           (buffer (zulip-feed--open-buffer account narrow))
           callback
           local-id)
      (cl-letf (((symbol-function 'zulip-api-send-message)
                 (lambda (_account _type _to _topic _content then &rest _options)
                   (setq callback then))))
        (with-current-buffer buffer
          (appkit-chat-history-window-establish-empty)
          (zulip-feed-render)
          (appkit-chatbuf-input-set-text "will fail")
          (setq local-id (zulip-feed-send-message)))
        (kill-buffer buffer)
        (funcall callback
                 (zulip-api-result--create
                  :ok-p nil :message "denied"))
        (zulip-runtime-test--drain account)
        (let ((message
               (zulip-state-message (zulip-account-state account) local-id)))
          (should message)
          (should-not (zulip-state-object-get message 'pending))
          (should (equal (zulip-state-object-get message 'failed)
                         "denied")))))))

(ert-deftest zulip-feed-failed-send-retries-with-same-local-id-and-rekeys ()
  (zulip-feed-test--with-account account
    (let* ((narrow (zulip-narrow-topic 7 "client"))
           (buffer (zulip-feed--open-buffer account narrow))
           callbacks calls local-id)
      (cl-letf (((symbol-function 'zulip-api-send-message)
                 (lambda (_account _type _to _topic content callback
                                   &rest options)
                   (push callback callbacks)
                   (push (list content
                               (plist-get options :local-id)
                               (plist-get options :queue-id))
                         calls))))
        (with-current-buffer buffer
          (appkit-chat-history-window-establish-empty)
          (zulip-feed-render)
          (appkit-chatbuf-input-set-text "retry me")
          (setq local-id (zulip-feed-send-message))
          (funcall (car callbacks)
                   (zulip-api-result--create :ok-p nil :message "temporary"))
          (zulip-runtime-test--drain account)
          (goto-char (appkit-chat-timeline-key-position local-id))
          (should (search-forward
                   "click or R to retry"
                   (appkit-chat-timeline-footer-start-position) t))
          (goto-char (appkit-chat-timeline-key-position local-id))
          (should (equal (zulip-feed-retry-send) local-id))
          (let ((retry (zulip-state-message
                        (zulip-account-state account) local-id)))
            (should (zulip-state-object-get retry 'pending))
            (should-not (zulip-state-object-get retry 'failed)))
          (should (= (length calls) 2))
          (should (equal (mapcar #'cadr calls)
                         (list local-id local-id)))
          (funcall (car callbacks)
                   (zulip-api-result--create
                    :ok-p t :data '((id . "90071992547409931234"))))
          (zulip-runtime-test--drain account)
          (should-not (zulip-state-message
                       (zulip-account-state account) local-id))
          (should (zulip-state-message
                   (zulip-account-state account) "90071992547409931234"))
          (should (equal (appkit-chat-timeline-keys)
                         '("90071992547409931234"))))))))

(ert-deftest zulip-feed-send-canonicalizes-tree-sitter-block-semantics ()
  (zulip-feed-test--with-account account
    (let* ((narrow (zulip-narrow-topic 7 "client"))
           (buffer (zulip-feed--open-buffer account narrow))
           (content "    indented code\nline with break  \n")
           (wire-content "```\nindented code\n```\n\nline with break")
           (local-content "indented code\nline with break")
           sent-content)
      (cl-letf (((symbol-function 'zulip-api-send-message)
                 (lambda (_account _type _to _topic wire-content
                                   _callback &rest _options)
                   (setq sent-content wire-content))))
        (with-current-buffer buffer
          (appkit-chat-history-window-establish-empty)
          (zulip-feed-render)
          (appkit-chatbuf-input-set-text content)
          (let* ((local-id (zulip-feed-send-message))
                 (message
                  (zulip-state-message
                   (zulip-account-state account) local-id)))
            (should (equal sent-content wire-content))
            (should (equal (zulip-state-object-get message 'content)
                           wire-content))
            (should (equal (zulip-state-object-get message 'local-content)
                           local-content))
            (should (string-match-p (regexp-quote local-content)
                                    (buffer-string)))))))))

(ert-deftest zulip-feed-local-echo-respects-markdown-html-block-semantics ()
  (zulip-feed-test--with-account account
    (let* ((narrow (zulip-narrow-topic 7 "client"))
           (buffer (zulip-feed--open-buffer account narrow))
           (content "<script>alert(1)</script> &amp; **bold**"))
      (cl-letf (((symbol-function 'zulip-api-send-message)
                 (lambda (&rest _arguments) nil)))
        (with-current-buffer buffer
          (appkit-chat-history-window-establish-empty)
          (zulip-feed-render)
          (appkit-chatbuf-input-set-text content)
          (zulip-feed-send-message)
          (should (string-match-p
                   (regexp-quote
                    "<script>alert(1)</script> &amp; **bold**")
                   (buffer-string)))
          (should-not
           (string-match-p
            (regexp-quote "<script>alert(1)</script> &amp; bold")
            (buffer-string))))))))

(ert-deftest zulip-feed-prefix-selects-org-and-encodes-markdown ()
  (zulip-feed-test--with-account account
    (let* ((narrow (zulip-narrow-topic 7 "client"))
           (buffer (zulip-feed--open-buffer account narrow))
           sent-content)
      (cl-letf (((symbol-function 'zulip-api-send-message)
                 (lambda (_account _type _to _topic content
                                   _callback &rest _options)
                   (setq sent-content content))))
        (with-current-buffer buffer
          (appkit-chat-history-window-establish-empty)
          (zulip-feed-render)
          (appkit-chatbuf-input-set-text "*bold* and /italic/")
          (zulip-feed-send-message '(4))
          (should (equal sent-content "**bold** and *italic*"))
          (should (string-match-p "bold and italic" (buffer-string))))))))

(ert-deftest zulip-feed-plain-prefix-escapes-markup-on-wire ()
  (zulip-feed-test--with-account account
    (let* ((narrow (zulip-narrow-topic 7 "client"))
           (buffer (zulip-feed--open-buffer account narrow))
           sent-content)
      (cl-letf (((symbol-function 'zulip-api-send-message)
                 (lambda (_account _type _to _topic content
                                   _callback &rest _options)
                   (setq sent-content content))))
        (with-current-buffer buffer
          (appkit-chat-history-window-establish-empty)
          (zulip-feed-render)
          (appkit-chatbuf-input-set-text "**literal**")
          (zulip-feed-send-message '(16))
          (should (equal sent-content "\\*\\*literal\\*\\*")))))))

(ert-deftest zulip-feed-compose-preview-is-pure-and-loss-is-rejected ()
  (zulip-feed-test--with-account account
    (let* ((narrow (zulip-narrow-topic 7 "client"))
           (buffer (zulip-feed--open-buffer account narrow))
           (send-count 0))
      (cl-letf (((symbol-function 'zulip-api-send-message)
                 (lambda (&rest _arguments) (cl-incf send-count))))
        (with-current-buffer buffer
          (appkit-chat-history-window-establish-empty)
          (zulip-feed-render)
          (appkit-chatbuf-input-set-text "*bold*")
          (let ((preview (zulip-feed-preview-message '(4))))
            (unwind-protect
                (with-current-buffer preview
                  (should (equal (buffer-string) "bold\n"))
                  (should buffer-read-only))
              (kill-buffer preview)))
          (should (= send-count 0))
          (appkit-chatbuf-input-set-text "_underline_")
          (let ((preview (zulip-feed-preview-message '(4))))
            (unwind-protect
                (with-current-buffer preview
                  (should (equal (buffer-string) "underline\n")))
              (kill-buffer preview)))
          (should-error (zulip-feed-send-message '(4)) :type 'user-error)
          (should (= send-count 0))
          (should (equal (appkit-chatbuf-input-state) "_underline_")))))))

(ert-deftest zulip-feed-register-invalidates-old-history-owner ()
  (zulip-feed-test--with-account account
    (let* ((narrow (zulip-narrow-channel 7))
           (key (zulip-narrow-key narrow))
           (state (zulip-state-merge-messages
                   (zulip-account-state account)
                   (list (zulip-feed-test--message "11" "old edge"))
                   key))
           (buffer (zulip-feed--open-buffer account narrow))
           calls old-callback
           old-owner)
      (zulip-runtime-publish-state account state)
      (cl-letf (((symbol-function 'zulip-api-get-messages)
                 (lambda (_account _wire anchor before after callback
                                   &rest options)
                   (let ((handle (appkit-register-handle
                                  (plist-get options :owner) 'function #'ignore)))
                     (push (list anchor before after
                                 (lambda (result)
                                   (appkit-retire-handle handle)
                                   (funcall callback result))) calls)
                     handle))))
        (with-current-buffer buffer
          (appkit-chat-history-window-set "11" nil)
          (zulip-feed-load-older)
          (setq old-owner (appkit-chat-history-request-owner)
                old-callback (nth 3 (car calls))))
        (zulip-feed--publish-event
         account '((type . "register"))
         (zulip-account-state account) (zulip-state-create))
        (zulip-runtime-test--drain account)
        (with-current-buffer buffer
          (should-not
           (appkit-chat-history-request-current-p old-owner))
          (should (eq (appkit-chat-history-loading) 'latest)))
        (let ((latest-callback (nth 3 (car calls))))
          (funcall
           old-callback
           (zulip-api-result--create
            :ok-p t
            :data (list
                   (cons 'messages
                         (vector (zulip-feed-test--message "5" "stale")))
                   (cons 'found_oldest t)
                   (cons 'found_newest t))))
          (zulip-runtime-test--drain account)
          (should-not (zulip-state-message
                       (zulip-account-state account) "5"))
          (funcall
           latest-callback
           (zulip-api-result--create
            :ok-p t
            :data (list
                   (cons 'messages
                         (vector (zulip-feed-test--message "20" "fresh")))
                   (cons 'found_oldest t)
                   (cons 'found_newest t))))
          (zulip-runtime-test--drain account)
          (should (zulip-state-message
                   (zulip-account-state account) "20"))
          (with-current-buffer buffer
            ;; History completion requests projection; this explicit flush
            ;; represents the scheduler firing after the callback returns.
            (zulip-runtime-test--drain account)
            (should (equal (appkit-chat-timeline-keys) '("20")))))))))

(ert-deftest zulip-feed-register-rebases-inflight-send ()
  (zulip-feed-test--with-account account
    (let* ((narrow (zulip-narrow-topic 7 "client"))
           (buffer (zulip-feed--open-buffer account narrow))
           send-callback
           history-callback
           local-id)
      (cl-letf (((symbol-function 'zulip-api-send-message)
                 (lambda (_account _type _to _topic _content callback
                                   &rest _options)
                   (setq send-callback callback)))
                ((symbol-function 'zulip-api-get-messages)
                 (lambda (_account _wire _anchor _before _after callback
                                   &rest _options)
                   (setq history-callback callback))))
        (with-current-buffer buffer
          (appkit-chat-history-window-establish-empty)
          (zulip-feed-render)
          (appkit-chatbuf-input-set-text "cross queue")
          (setq local-id (zulip-feed-send-message)))
        (zulip-feed--publish-event
         account '((type . "register"))
         (zulip-account-state account) (zulip-state-create))
        (zulip-runtime-test--drain account)
        (should (zulip-state-message
                 (zulip-account-state account) local-id))
        (with-current-buffer buffer
          (should (equal (appkit-chat-timeline-keys) (list local-id))))
        (funcall
         history-callback
         (zulip-api-result--create
          :ok-p t
          :data '((messages . [])
                  (found_oldest . t)
                  (found_newest . t))))
        (zulip-runtime-test--drain account)
        (with-current-buffer buffer
          (should (equal (appkit-chat-timeline-keys) (list local-id))))
        (funcall
         send-callback
         (zulip-api-result--create :ok-p t :data '((id . "101"))))
        (zulip-runtime-test--drain account)
        (should-not (zulip-state-message
                     (zulip-account-state account) local-id))
        (should (zulip-state-message
                 (zulip-account-state account) "101"))))))

(ert-deftest zulip-feed-live-event-survives-older-empty-latest-response ()
  (zulip-feed-test--with-account account
    (let* ((narrow (zulip-narrow-all))
           (buffer (zulip-feed--open-buffer account narrow))
           history-callback
           (message (zulip-feed-test--message "60" "live")))
      (cl-letf (((symbol-function 'zulip-api-get-messages)
                 (lambda (_account _wire _anchor _before _after callback
                                   &rest _options)
                   (setq history-callback callback))))
        (with-current-buffer buffer
          (zulip-feed-load-latest))
        (let* ((old-state (zulip-account-state account))
               (new-state (zulip-state-upsert-message old-state message)))
          (zulip-feed--publish-event
           account (list (cons 'type "message") (cons 'message message))
           old-state new-state)
          (zulip-runtime-test--drain account))
        (with-current-buffer buffer
          (zulip-runtime-test--drain account)
          (should (equal (appkit-chat-timeline-keys) '("60"))))
        (funcall
         history-callback
         (zulip-api-result--create
          :ok-p t
          :data '((messages . [])
                  (found_oldest . t)
                  (found_newest . t))))
        (zulip-runtime-test--drain account)
        (with-current-buffer buffer
          (should (appkit-chat-history-window-known-p))
          (should-not (appkit-chat-history-window-empty-p))
          (should (equal (appkit-chat-timeline-keys) '("60"))))))))

(ert-deftest zulip-feed-live-event-survives-failed-initial-latest ()
  (zulip-feed-test--with-account account
    (let* ((narrow (zulip-narrow-all))
           (buffer (zulip-feed--open-buffer account narrow))
           history-callback
           (message (zulip-feed-test--message "61" "live after failure")))
      (cl-letf (((symbol-function 'zulip-api-get-messages)
                 (lambda (_account _wire _anchor _before _after callback
                                   &rest _options)
                   (setq history-callback callback))))
        (with-current-buffer buffer
          (zulip-feed-load-latest))
        (let* ((old-state (zulip-account-state account))
               (new-state (zulip-state-upsert-message old-state message)))
          (zulip-feed--publish-event
           account (list (cons 'type "message") (cons 'message message))
           old-state new-state)
          (zulip-runtime-test--drain account))
        (with-current-buffer buffer
          (zulip-runtime-test--drain account))
        (funcall history-callback
                 (zulip-api-result--create
                  :ok-p nil :message "offline"))
        (zulip-runtime-test--drain account)
        (with-current-buffer buffer
          (should (appkit-chat-history-window-known-p))
          (should (equal (appkit-chat-timeline-keys) '("61"))))))))

(ert-deftest zulip-feed-partial-window-disables-send ()
  (zulip-feed-test--with-account account
    (let* ((narrow (zulip-narrow-topic 7 "client"))
           (key (zulip-narrow-key narrow))
           (state (zulip-state-merge-messages
                   (zulip-account-state account)
                   (list (zulip-feed-test--message "11" "one")
                         (zulip-feed-test--message "20" "two"))
                   key)))
      (zulip-runtime-publish-state account state)
      (let ((buffer (zulip-feed--open-buffer account narrow)))
        (with-current-buffer buffer
          (appkit-chat-history-window-set "11" "20")
          (zulip-feed-render)
          (should (appkit-chat-history-window-partial-p))
          (should-not (zulip-feed--composer-visible-p))
          (cl-letf (((symbol-function 'appkit-chatbuf-input-string)
                     (lambda () "must remain unsent"))
                    ((symbol-function 'zulip-api-send-message)
                     (lambda (&rest _arguments)
                       (ert-fail "partial history must not send"))))
            (should-error (zulip-feed-send-message) :type 'user-error)))))))

(ert-deftest zulip-feed-generated-content-is-read-only ()
  (zulip-feed-test--with-account account
    (let* ((narrow (zulip-narrow-topic 7 "client"))
           (key (zulip-narrow-key narrow))
           (state (zulip-state-merge-messages
                   (zulip-account-state account)
                   (list (zulip-feed-test--message "11" "one"))
                   key)))
      (zulip-runtime-publish-state account state)
      (let ((buffer (zulip-feed--open-buffer account narrow)))
        (with-current-buffer buffer
          (appkit-chat-history-window-set "11" nil)
          (zulip-feed-render)
          (let ((original (buffer-string)))
            (goto-char (point-min))
            (should-error (insert "corrupt") :type 'text-read-only)
            (should (equal (buffer-string) original)))
          ;; The ordinary editable tail continues to behave as a normal Emacs
          ;; input region and stays synchronized with Appkit's draft cache.
          (appkit-chatbuf-input-set-text "draft")
          (goto-char (appkit-chatbuf-input-start-position))
          (insert "live ")
          (should (equal (appkit-chatbuf-input-string) "live draft"))
          (should (equal (appkit-chatbuf-input-state) "live draft")))))))

(ert-deftest zulip-feed-empty-newer-response-attaches-live-edge ()
  (zulip-feed-test--with-account account
    (let* ((narrow (zulip-narrow-topic 7 "client"))
           (key (zulip-narrow-key narrow))
           (state (zulip-state-merge-messages
                   (zulip-account-state account)
                   (list (zulip-feed-test--message "11" "one")
                         (zulip-feed-test--message "20" "two"))
                   key)))
      (zulip-runtime-publish-state account state)
      (let ((buffer (zulip-feed--open-buffer account narrow)))
        (cl-letf (((symbol-function 'zulip-api-get-messages)
                   (lambda (_account _wire _anchor _before _after callback
                                     &rest _options)
                     (funcall
                      callback
                      (zulip-api-result--create
                       :ok-p t
                       :data '((messages . [])
                               (found_oldest . :json-false)
                               (found_newest . t)))))))
          (with-current-buffer buffer
            (appkit-chat-history-window-set "11" "20")
            (zulip-feed-render)
            (zulip-feed-load-newer)
            (should-not (appkit-chat-history-window-partial-p))
            (should-not (appkit-chat-history-window-last-key))))))))

(ert-deftest zulip-feed-empty-newer-response-records-stalled-edge ()
  (zulip-feed-test--with-account account
    (let* ((narrow (zulip-narrow-topic 7 "client"))
           (key (zulip-narrow-key narrow))
           (state (zulip-state-merge-messages
                   (zulip-account-state account)
                   (list (zulip-feed-test--message "11" "one")
                         (zulip-feed-test--message "20" "two"))
                   key)))
      (zulip-runtime-publish-state account state)
      (let ((buffer (zulip-feed--open-buffer account narrow)))
        (cl-letf (((symbol-function 'zulip-api-get-messages)
                   (lambda (_account _wire _anchor _before _after callback
                                     &rest _options)
                     (funcall
                      callback
                      (zulip-api-result--create
                       :ok-p t
                       :data '((messages . [])
                               (found_oldest . :json-false)
                               (found_newest . :json-false)))))))
          (with-current-buffer buffer
            (appkit-chat-history-window-set "11" "20")
            (zulip-feed-render)
            (zulip-feed-load-newer)
            (should (appkit-chat-history-window-partial-p))
            (should (equal (appkit-chat-history-window-last-key) "20"))
            (should (appkit-chat-history-newer-stalled-p))
            (should (equal (appkit-chat-timeline-keys) '("11" "20")))))))))

(ert-deftest zulip-feed-repairs-removed-history-boundaries ()
  (zulip-feed-test--with-account account
    (let* ((narrow (zulip-narrow-topic 7 "client"))
           (key (zulip-narrow-key narrow))
           (messages (list (zulip-feed-test--message "11" "one")
                           (zulip-feed-test--message "20" "two")
                           (zulip-feed-test--message "30" "three")
                           (zulip-feed-test--message "40" "four")))
           (state (zulip-state-merge-messages
                   (zulip-account-state account) messages key)))
      (zulip-runtime-publish-state account state)
      (let ((buffer (zulip-feed--open-buffer account narrow)))
        (with-current-buffer buffer
          (appkit-chat-history-window-set "11" nil)
          (zulip-feed-render))
        (let* ((old (zulip-account-state account))
               (next (zulip-state-delete-message old "11")))
          (zulip-feed--publish-event
           account '((type . delete_message) (message_id . "11")) old next)
          (zulip-runtime-test--drain account))
        (with-current-buffer buffer
          (should (equal (appkit-chat-history-window-first-key) "20"))
          (should (equal (appkit-chat-timeline-keys) '("20" "30" "40")))
          (appkit-chat-history-window-set "20" "40")
          (zulip-feed-render))
        (let* ((old (zulip-account-state account))
               (next (zulip-state-delete-message old "40")))
          (zulip-feed--publish-event
           account '((type . update_message) (message_id . "40")) old next)
          (zulip-runtime-test--drain account))
        (with-current-buffer buffer
          (should (equal (appkit-chat-history-window-last-key) "30"))
          (should (equal (appkit-chat-timeline-keys) '("20" "30"))))
        ;; An unexhausted window that disappears must acquire a new exact page,
        ;; even when the independent live-event Source is not connected.
        (let (complete handle)
          (cl-letf (((symbol-function 'zulip-api-get-messages)
                     (lambda (_account _narrow _anchor _before _after callback
                                       &rest options)
                       (setq handle
                             (appkit-register-handle
                              (plist-get options :owner) 'function
                              (lambda () (setq complete nil))))
                       (setq complete
                             (lambda (result)
                               (appkit-retire-handle handle)
                               (funcall callback result)))
                       handle)))
            (with-current-buffer buffer
              (appkit-chat-history-window-set "20" nil)
              (appkit-chat-history-older-loaded-set nil))
            (let* ((old (zulip-account-state account))
                   (next (zulip-state-delete-message
                          (zulip-state-delete-message old "20") "30")))
              (zulip-feed--publish-event
               account '((type . delete_message) (message_ids . ["20" "30"]))
               old next)
              (zulip-runtime-test--drain account))
            (with-current-buffer buffer
              (should (eq (appkit-chat-history-loading) 'latest))
              (should-not (appkit-chat-history-window-empty-p)))
            (funcall complete
                     (zulip-api-result--create
                      :ok-p t
                      :data (list (cons 'messages
                                        (list (zulip-feed-test--message "50" "recovered")))
                                  '(found_newest . t))))
            (zulip-runtime-test--drain account)
            (with-current-buffer buffer
              (should-not (appkit-chat-history-loading-p))
              (should (equal (appkit-chat-history-window-first-key) "50"))
              (should (equal (appkit-chat-timeline-keys) '("50"))))))))))

(ert-deftest zulip-feed-projects-telega-style-context-unread-and-reactions ()
  (zulip-feed-test--with-account account
    (setf (zulip-state-self-user-id (zulip-account-state account)) "1")
    (let* ((narrow (zulip-narrow-channel 7))
           (key (zulip-narrow-key narrow))
           (first
            (append
             (zulip-feed-test--message "11" "one")
             '((sender_id . 2)
               (reactions
                . (((emoji_name . "wave") (reaction_type . "unicode_emoji")
                    (user_id . 1))
                   ((emoji_name . "wave") (reaction_type . "unicode_emoji")
                    (user_id . 2)))))))
           (second
            (append (zulip-feed-test--message "20" "two")
                    '((sender_id . 2))))
           (state (zulip-state-merge-messages
                   (zulip-account-state account) (list first second) key)))
      (setq state
            (zulip-state-set-message-unread
             state "11" t
             '((kind . channel) (channel-id . "7") (topic . "client"))))
      (zulip-runtime-publish-state account state)
      (let ((buffer (zulip-feed--open-buffer account narrow)))
        (with-current-buffer buffer
          (appkit-chat-history-window-set "11" nil)
          (zulip-feed-render)
          (should (string-match-p "Unread" (buffer-string)))
          (should (string-match-p "1970-01-01" (buffer-string)))
          (should (string-match-p "client" (buffer-string)))
          (should (string-match-p ":wave: 2" (buffer-string)))
          ;; Consecutive messages by the same sender share one full heading.
          (should (= 1 (how-many "Ada" (point-min) (point-max))))
          (should (get-text-property
                   (appkit-chat-timeline-key-position "11")
                   zulip-feed--anchor-property)))))))

(ert-deftest zulip-feed-uses-appkit-history-autoload-gates ()
  (zulip-feed-test--with-account account
    (let* ((narrow (zulip-narrow-topic 7 "client"))
           (key (zulip-narrow-key narrow))
           (state (zulip-state-merge-messages
                   (zulip-account-state account)
                   (list (zulip-feed-test--message "11" "one")
                         (zulip-feed-test--message "20" "two"))
                   key))
           (buffer (zulip-feed--open-buffer account narrow))
           (older 0)
           (newer 0))
      (zulip-runtime-publish-state account state)
      (with-current-buffer buffer
        (appkit-chat-history-window-set "11" nil)
        (zulip-feed-render)
        (goto-char (point-min))
        (cl-letf (((symbol-function 'zulip-feed-load-older)
                   (lambda () (cl-incf older))))
          (zulip-feed--maybe-auto-load-older))
        (should (= older 1))
        (appkit-chat-history-window-set "11" "20")
        (zulip-feed-render)
        (cl-letf (((symbol-function 'zulip-feed-load-newer)
                   (lambda () (cl-incf newer))))
          (zulip-feed--maybe-auto-load-newer
           (appkit-chat-timeline-footer-start-position)))
        (should (= newer 1))))))

(ert-deftest zulip-feed-history-fence-cancels-surface-owned-transport ()
  (zulip-feed-test--with-account account
    (let* ((buffer (zulip-feed--open-buffer account (zulip-narrow-channel 7)))
           observed-owner handle canceled callback)
      (cl-letf (((symbol-function 'zulip-api-get-messages)
                 (lambda (_account _wire _anchor _before _after response &rest options)
                   (setq observed-owner (plist-get options :owner)
                         callback response
                         handle (appkit-register-handle
                                 observed-owner 'function 'request
                                 (lambda (_object) (setq canceled t)))))))
        (with-current-buffer buffer
          (zulip-feed-load-latest)
          (should (eq observed-owner (appkit-current-surface)))
          (let ((operation (appkit-chat-history-request-owner)))
            (should (appkit-chat-history-request-current-p operation))
            (appkit-chat-history-request-cancel)
            (should canceled)
            (should-not (appkit-handle-alive-p handle))
            (should-not (appkit-chat-history-request-current-p operation)))
          (funcall callback
                   (zulip-api-result--create
                    :ok-p t :data (list (cons 'messages
                                              (vector (zulip-feed-test--message "99" "stale"))))))
          (zulip-runtime-test--drain account)
          (should-not (zulip-state-message (zulip-account-state account) "99")))))))

(ert-deftest zulip-feed-auto-read-submits-exact-loaded-unread-prefix-once ()
  (zulip-feed-test--with-account account
    (setf (zulip-account-connected-p account) t)
    (let* ((narrow (zulip-narrow-topic 7 "client"))
           (key (zulip-narrow-key narrow))
           (messages (list (zulip-feed-test--message "11" "one")
                           (zulip-feed-test--message "20" "two")
                           (zulip-feed-test--message "30" "three")))
           (state (zulip-state-merge-messages
                   (zulip-account-state account) messages key))
           (call-count 0) captured)
      (dolist (id '("11" "20" "30"))
        (setq state (zulip-state-set-message-unread state id t)))
      (zulip-runtime-publish-state account state)
      (let ((buffer (zulip-feed--open-buffer account narrow)))
        (with-current-buffer buffer
          (appkit-chat-history-window-set "11" nil)
          (zulip-feed-render)
          (goto-char (appkit-chat-timeline-key-position "20"))
          (cl-letf (((symbol-function 'zulip-api-update-message-flags)
                     (lambda (_account ids operation flag _callback
                                       &rest options)
                       (cl-incf call-count)
                       (setq captured
                             (list ids operation flag
                                   (plist-get options :owner))))))
            (should (equal (zulip-feed--mark-read-through nil t t)
                           '("11" "20")))
            (should (= call-count 1))
            (should (equal (append (nth 0 captured) nil) '("11" "20")))
            (should (eq (nth 1 captured) 'add))
            (should (equal (nth 2 captured) "read"))
            (should (eq (nth 3 captured) (appkit-current-surface)))
            ;; The exact frontier and pending-ID gates suppress post-command
            ;; duplication before the queue event confirms the first request.
            (should-not (zulip-feed--mark-read-through nil t t))
            (should (= call-count 1))))))))

(ert-deftest zulip-feed-flag-failure-restores-read-state-in-captured-view ()
  (zulip-feed-test--with-account account
    (setf (zulip-account-connected-p account) t)
    (let* ((narrow (zulip-narrow-topic 7 "client"))
           (key (zulip-narrow-key narrow))
           (messages (list (zulip-feed-test--message "11" "one")
                           (zulip-feed-test--message "20" "two")))
           (state (zulip-state-merge-messages
                   (zulip-account-state account) messages key))
           callback observed-owner view)
      (dolist (id '("11" "20"))
        (setq state (zulip-state-set-message-unread state id t)))
      (zulip-runtime-publish-state account state)
      (let ((buffer (zulip-feed--open-buffer account narrow)))
        (with-current-buffer buffer
          (appkit-chat-history-window-set "11" nil)
          (zulip-feed-render)
          (goto-char (appkit-chat-timeline-key-position "20"))
          (setq view (appkit-current-surface))
          (cl-letf (((symbol-function 'zulip-api-update-message-flags)
                     (lambda (_account _ids _operation _flag response
                                       &rest options)
                       (setq callback response
                             observed-owner (plist-get options :owner)))))
            (should (equal (zulip-feed--mark-read-through nil t t)
                           '("11" "20"))))
          (should (eq observed-owner view))
          (should (equal zulip-feed--last-read-target-id "20"))
          (should (= (hash-table-count zulip-feed--pending-read-ids) 2)))
        ;; Model plz invoking the callback from an unrelated process buffer.
        ;; Its similarly named locals must remain untouched.
        (with-temp-buffer
          (setq-local zulip-feed--pending-read-ids
                      (make-hash-table :test #'equal))
          (puthash "11" 'foreign zulip-feed--pending-read-ids)
          (puthash "20" 'foreign zulip-feed--pending-read-ids)
          (setq-local zulip-feed--last-read-target-id "20")
          (funcall callback
                   (zulip-api-result--create
                    :ok-p nil :message "network down"))
          (zulip-runtime-test--drain account)
          (should (= (hash-table-count zulip-feed--pending-read-ids) 2))
          (should (eq (gethash "11" zulip-feed--pending-read-ids) 'foreign))
          (should (eq (gethash "20" zulip-feed--pending-read-ids) 'foreign))
          (should (equal zulip-feed--last-read-target-id "20")))
        (with-current-buffer buffer
          (should (= (hash-table-count zulip-feed--pending-read-ids) 0))
          (should-not zulip-feed--last-read-target-id)
          (should (equal zulip-feed--last-error "network down"))
          (should (zulip-state-unread-message-p
                   (zulip-account-state account) "11")))))))

(ert-deftest zulip-feed-explicit-unread-is-not-undone-by-auto-read ()
  (zulip-feed-test--with-account account
    (setf (zulip-account-connected-p account) t)
    (let* ((narrow (zulip-narrow-topic 7 "client"))
           (key (zulip-narrow-key narrow))
           (message (append (zulip-feed-test--message "20" "two")
                            '((flags . ("read")))))
           (state (zulip-state-merge-messages
                   (zulip-account-state account) (list message) key))
           calls)
      (zulip-runtime-publish-state account state)
      (let ((buffer (zulip-feed--open-buffer account narrow)))
        (with-current-buffer buffer
          (appkit-chat-history-window-set "20" nil)
          (zulip-feed-render)
          (goto-char (appkit-chat-timeline-key-position "20"))
          (cl-letf (((symbol-function 'zulip-api-update-message-flags)
                     (lambda (_account ids operation flag callback
                                       &rest options)
                       (push (list (append ids nil) operation flag
                                   (plist-get options :owner))
                             calls)
                       (funcall callback
                                (zulip-api-result--create :ok-p t)))))
            (zulip-feed-mark-unread)
            (should (equal (caar calls) '("20")))
            (should (eq (cadar calls) 'remove))
            ;; Simulate the queue's mark-unread state before automatic point
            ;; observation runs again.
            (zulip-runtime-publish-state
             account
             (zulip-state-set-message-unread
              (zulip-account-state account) "20" t))
            (should-not (zulip-feed--mark-read-through nil t t))
            (should (= (length calls) 1))))))))

(ert-deftest zulip-feed-toggle-star-uses-personal-flag-state ()
  (zulip-feed-test--with-account account
    (let* ((narrow (zulip-narrow-topic 7 "client"))
           (key (zulip-narrow-key narrow))
           (message (append (zulip-feed-test--message "20" "two")
                            '((flags . ("read" "starred")))))
           (state (zulip-state-merge-messages
                   (zulip-account-state account) (list message) key))
           captured)
      (zulip-runtime-publish-state account state)
      (let ((buffer (zulip-feed--open-buffer account narrow)))
        (with-current-buffer buffer
          (appkit-chat-history-window-set "20" nil)
          (zulip-feed-render)
          (goto-char (appkit-chat-timeline-key-position "20"))
          (should (get-text-property (point) 'zulip-message-starred))
          (should (save-excursion
                    (search-forward
                     "★" (appkit-chat-timeline-footer-start-position) t)))
          (cl-letf (((symbol-function 'zulip-api-update-message-flags)
                     (lambda (_account ids operation flag callback
                                       &rest options)
                       (setq captured
                             (list (append ids nil) operation flag
                                   (plist-get options :owner)))
                       (funcall callback
                                (zulip-api-result--create :ok-p t)))))
            (zulip-feed-toggle-star)
            (should (equal (nth 0 captured) '("20")))
            (should (eq (nth 1 captured) 'remove))
            (should (equal (nth 2 captured) "starred"))
            (should (eq (nth 3 captured) (appkit-current-surface)))))))))

(ert-deftest zulip-feed-edit-stages-raw-markdown-and-submits-through-view ()
  (zulip-feed-test--with-account account
    (let* ((narrow (zulip-narrow-topic 7 "client"))
           (key (zulip-narrow-key narrow))
           (message (zulip-feed-test--message "20" "<p>old</p>"))
           (state (zulip-state-merge-messages
                   (zulip-account-state account) (list message) key))
           get-owner update-call)
      (zulip-runtime-publish-state account state)
      (let ((buffer (zulip-feed--open-buffer account narrow)))
        (with-current-buffer buffer
          (appkit-chat-history-window-set "20" nil)
          (zulip-feed-render)
          (goto-char (appkit-chat-timeline-key-position "20"))
          (cl-letf (((symbol-function 'zulip-api-get-message)
                     (lambda (_account id callback &rest options)
                       (setq get-owner (plist-get options :owner))
                       (should (equal id "20"))
                       (should-not (plist-get options :apply-markdown))
                       (funcall callback (zulip-api-result--create
                                          :ok-p t :data '((raw_content . "**old**"))))))
                    ((symbol-function 'zulip-api-update-message)
                     (lambda (_account id callback &rest options)
                       (setq update-call (list id options))
                       (funcall callback (zulip-api-result--create :ok-p t)))))
            (appkit-chatbuf-input-set-text "unsent draft")
            (goto-char (appkit-chat-timeline-key-position "20"))
            (zulip-feed-edit-message)
            (zulip-runtime-test--drain account)
            (should (eq get-owner (appkit-current-surface)))
            (should (eq (appkit-chatbuf-aux-type) 'edit))
            (should (equal (appkit-chatbuf-aux-message-id) "20"))
            (should (equal (appkit-chatbuf-input-string) "**old**"))
            (appkit-chatbuf-input-set-text "**new**")
            (zulip-feed-send-message)
            (zulip-runtime-test--drain account)
            (should (equal (car update-call) "20"))
            (should (equal (plist-get (cadr update-call) :content) "**new**"))
            (should (eq (plist-get (cadr update-call) :owner) (appkit-current-surface)))
            (should-not (appkit-chatbuf-aux-active-p))
            (should (equal (appkit-chatbuf-input-string) "unsent draft"))))))))

(ert-deftest zulip-feed-inflight-edit-get-freezes-composer ()
  (zulip-feed-test--with-account account
    (let* ((narrow (zulip-narrow-topic 7 "client"))
           (key (zulip-narrow-key narrow))
           (message (zulip-feed-test--message "20" "old"))
           (state (zulip-state-merge-messages
                   (zulip-account-state account) (list message) key))
           (draft
            (concat
             (appkit-chatbuf-input-object-string
              "@Ada"
              '(:kind zulip-mention :user-id "42" :full-name "Ada"
                :wire "@**Ada|42**"))
             "protected draft"))
           get-callback)
      (zulip-runtime-publish-state account state)
      (let ((buffer (zulip-feed--open-buffer account narrow)))
        (with-current-buffer buffer
          (appkit-chat-history-window-set "20" nil)
          (zulip-feed-render)
          (cl-letf (((symbol-function 'zulip-api-get-message)
                     (lambda (_account _id callback &rest _options)
                       (setq get-callback callback))))
            (appkit-chatbuf-input-history-push "older draft")
            (appkit-chatbuf-input-set-text draft)
            (zulip-feed-edit-message message)
            ;; The initial generation-owned materialization has completed, but
            ;; the GET owner is still live.  User/programmatic edits remain
            ;; frozen until that exact operation settles.
            (zulip-runtime-test--drain account)
            (should (zulip-feed--edit-request-p))
            (should buffer-read-only)
            (should-not (appkit-chatbuf-rendering-p))
            (let ((undo-before buffer-undo-list))
              (goto-char (point-max))
              (should-error (insert " GET race")
                            :type 'buffer-read-only)
              (should-error (zulip-feed-draft-previous)
                            :type 'user-error)
              (should (equal buffer-undo-list undo-before)))
            (dolist (input (list (appkit-chatbuf-input-string)
                                 (appkit-chatbuf-input-state)))
              (should (equal input "@Ada protected draft"))
              (should (eq (plist-get
                           (get-text-property
                            0 appkit-chatbuf-input-object-property input)
                           :kind)
                          'zulip-mention))
              (should
               (equal
                (plist-get
                 (get-text-property
                  0 appkit-chatbuf-input-object-property input)
                 :wire)
                "@**Ada|42**")))

            (funcall get-callback
                     (zulip-api-result--create
                      :ok-p t :data '((raw_content . "raw source"))))
            (zulip-runtime-test--drain account)
            (should-not buffer-read-only)
            (should (equal (appkit-chatbuf-input-string) "raw source"))
            (zulip-feed-cancel-edit)
            (zulip-runtime-test--drain account)
            (should-not buffer-read-only)
            (let ((restored (appkit-chatbuf-input-string)))
              (should (equal restored "@Ada protected draft"))
              (should (eq (plist-get
                           (get-text-property
                            0 appkit-chatbuf-input-object-property restored)
                           :kind)
                          'zulip-mention))
              (should
               (equal
                (plist-get
                 (get-text-property
                  0 appkit-chatbuf-input-object-property restored)
                 :wire)
                "@**Ada|42**")))
            ;; Outside an edit owner/barrier, ordinary drafts remain fully
            ;; editable through the standard Appkit composer path.
            (appkit-chatbuf-input-set-text "ordinary draft")
            (should (equal (appkit-chatbuf-input-string)
                           "ordinary draft"))))))))

(ert-deftest zulip-feed-inflight-edit-patch-freezes-composer ()
  (zulip-feed-test--with-account account
    (let* ((narrow (zulip-narrow-topic 7 "client"))
           (key (zulip-narrow-key narrow))
           (message (zulip-feed-test--message "20" "old"))
           (state (zulip-state-merge-messages
                   (zulip-account-state account) (list message) key))
           (draft
            (concat
             (appkit-chatbuf-input-object-string
              "@Ada"
              '(:kind zulip-mention :user-id "42" :full-name "Ada"
                :wire "@**Ada|42**"))
             "protected draft"))
           patch-callback patch-content)
      (zulip-runtime-publish-state account state)
      (let ((buffer (zulip-feed--open-buffer account narrow)))
        (with-current-buffer buffer
          (appkit-chat-history-window-set "20" nil)
          (zulip-feed-render)
          (cl-letf (((symbol-function 'zulip-api-get-message)
                     (lambda (_account _id callback &rest _options)
                       (funcall
                        callback
                        (zulip-api-result--create
                         :ok-p t :data '((raw_content . "raw source"))))))
                    ((symbol-function 'zulip-api-update-message)
                     (lambda (_account _id callback &rest options)
                       (setq patch-callback callback
                             patch-content (plist-get options :content)))))
            (appkit-chatbuf-input-history-push "older draft")
            (appkit-chatbuf-input-set-text draft)
            (zulip-feed-edit-message message)
            (zulip-runtime-test--drain account)
            (should-not buffer-read-only)
            (should (equal (appkit-chatbuf-input-string) "raw source"))
            (appkit-chatbuf-input-set-text "edited raw source")
            (zulip-feed-submit-edit)
            (should (equal patch-content "edited raw source"))
            ;; A frame-only sync must not accidentally unlock the composer
            ;; while PATCH still owns this generation.
            (zulip-runtime-test--drain account)
            (should (zulip-feed--edit-request-p))
            (should buffer-read-only)
            (should-not (appkit-chatbuf-rendering-p))
            (let ((undo-before buffer-undo-list))
              (goto-char (point-max))
              (should-error (insert " PATCH race")
                            :type 'buffer-read-only)
              (should-error (zulip-feed-draft-previous)
                            :type 'user-error)
              (should (equal buffer-undo-list undo-before)))
            (should (equal (appkit-chatbuf-input-string)
                           "edited raw source"))
            (should (equal (appkit-chatbuf-input-state)
                           "edited raw source"))

            (funcall patch-callback (zulip-api-result--create :ok-p t))
            (zulip-runtime-test--drain account)
            (should-not buffer-read-only)
            (let ((restored (appkit-chatbuf-input-string)))
              (should (equal restored "@Ada protected draft"))
              (should (eq (plist-get
                           (get-text-property
                            0 appkit-chatbuf-input-object-property restored)
                           :kind)
                          'zulip-mention))
              (should
               (equal
                (plist-get
                 (get-text-property
                  0 appkit-chatbuf-input-object-property restored)
                 :wire)
                "@**Ada|42**")))
            ;; Outside an edit owner/barrier, ordinary drafts remain fully
            ;; editable through the standard Appkit composer path.
            (appkit-chatbuf-input-set-text "ordinary draft")
            (should (equal (appkit-chatbuf-input-string)
                           "ordinary draft"))))))))

(ert-deftest zulip-feed-cancel-edit-restores-rich-structured-draft ()
  (zulip-feed-test--with-account account
    (let* ((narrow (zulip-narrow-topic 7 "client"))
           (key (zulip-narrow-key narrow))
           (message (zulip-feed-test--message "20" "<p>old</p>"))
           (state (zulip-state-merge-messages
                   (zulip-account-state account) (list message) key))
           (buffer nil))
      (zulip-runtime-publish-state account state)
      (setq buffer (zulip-feed--open-buffer account narrow))
      (with-current-buffer buffer
        (appkit-chat-history-window-set "20" nil)
        (zulip-feed-render)
        (let ((draft
               (concat
                (appkit-chatbuf-input-object-string
                 "@Ada"
                 '(:kind zulip-mention :user-id "42" :full-name "Ada"
                   :wire "@**Ada|42**"))
                "later")))
          (appkit-markup-compose-set-active-codec 'org)
          (appkit-chatbuf-input-set-text draft)
          (goto-char (appkit-chat-timeline-key-position "20"))
          (cl-letf (((symbol-function 'zulip-api-get-message)
                     (lambda (_account _id callback &rest _options)
                       (funcall callback
                                (zulip-api-result--create
                                 :ok-p t :data '((raw_content . "raw")))))))
            (zulip-feed-edit-message)
            (let ((undo-before buffer-undo-list))
              (zulip-runtime-test--drain account)
              (should (equal buffer-undo-list undo-before)))
            (should (equal (appkit-chatbuf-input-string) "raw"))
            (should (eq appkit-markup-compose-active-codec 'markdown))
            (zulip-feed-cancel-edit)
            (should (equal (appkit-chatbuf-input-string) "raw"))
            (let ((undo-before buffer-undo-list))
              (zulip-runtime-test--drain account)
              (should (equal buffer-undo-list undo-before)))
            (should (eq appkit-markup-compose-active-codec 'org))
            (let* ((restored (appkit-chatbuf-input-string))
                   (object (get-text-property
                            0 appkit-chatbuf-input-object-property restored)))
              (should (equal restored "@Ada later"))
              (should (eq (plist-get object :kind) 'zulip-mention))
              (should (equal (plist-get object :wire)
                             "@**Ada|42**")))))))))

(ert-deftest zulip-feed-stale-edit-get-and-patch-cannot-cross-generations ()
  (zulip-feed-test--with-account account
    (let* ((narrow (zulip-narrow-topic 7 "client"))
           (key (zulip-narrow-key narrow))
           (message-twenty (zulip-feed-test--message "20" "twenty"))
           (message-thirty (zulip-feed-test--message "30" "thirty"))
           (state (zulip-state-merge-messages
                   (zulip-account-state account)
                   (list message-twenty message-thirty) key))
           (get-callbacks (make-hash-table :test #'equal))
           patch-callback)
      (zulip-runtime-publish-state account state)
      (let ((buffer (zulip-feed--open-buffer account narrow)))
        (with-current-buffer buffer
          (appkit-chat-history-window-set "20" nil)
          (zulip-feed-render)
          (cl-letf (((symbol-function 'zulip-api-get-message)
                     (lambda (_account id callback &rest _options)
                       (puthash id callback get-callbacks)))
                    ((symbol-function 'zulip-api-update-message)
                     (lambda (_account _id callback &rest _options)
                       (setq patch-callback callback))))
            ;; Cancel generation one, change the ordinary draft, and start a
            ;; different edit before its delayed GET completes.
            (appkit-chatbuf-input-set-text "draft zero")
            (zulip-feed-edit-message message-twenty)
            (let ((stale-get (gethash "20" get-callbacks)))
              (zulip-runtime-test--drain account)
              (zulip-feed-cancel-edit)
              (zulip-runtime-test--drain account)
              (appkit-chatbuf-input-set-text "draft after cancel")
              (zulip-feed-edit-message message-thirty)
              (zulip-runtime-test--drain account)
              (let ((new-owner zulip-feed--edit-operation-owner)
                    (new-generation zulip-feed--edit-generation))
                (goto-char (appkit-chat-timeline-key-position "30"))
                (let ((point-before (point)))
                  (funcall stale-get
                           (zulip-api-result--create
                            :ok-p t :data '((raw_content . "stale raw"))))
                  (zulip-runtime-test--drain account)
                  (should (= (point) point-before)))
                (should (eq zulip-feed--edit-operation-owner new-owner))
                (should (eq zulip-feed--edit-generation new-generation))
                (should (equal (zulip-feed--edit-message-id) "30"))
                (should (zulip-feed--edit-request-p))
                (should (equal (appkit-chatbuf-input-string)
                               "draft after cancel"))
                (should (equal (appkit-chatbuf-input-state)
                               "draft after cancel"))))

            ;; Deliver the accepted GET through its captured Surface.
            (let ((current-get (gethash "30" get-callbacks)))
              (funcall current-get
                       (zulip-api-result--create
                        :ok-p t :data '((raw_content . "raw thirty"))))
              (should (equal (appkit-chatbuf-input-string)
                             "draft after cancel"))
              (zulip-runtime-test--drain account)
              (should (equal (appkit-chatbuf-input-string) "raw thirty"))
              (should (= (point)
                         (appkit-chatbuf-input-logical-end-position))))

            ;; Cancel an in-flight PATCH, make another draft, and begin a new
            ;; edit.  Its delayed success cannot restore the old saved draft,
            ;; clear the new aux owner, push history, or move point.
            (appkit-chatbuf-input-set-text "edited thirty")
            (zulip-feed-submit-edit)
            (should patch-callback)
            (zulip-runtime-test--drain account)
            (zulip-feed-cancel-edit)
            (zulip-runtime-test--drain account)
            (appkit-chatbuf-input-set-text "new draft after patch cancel")
            (zulip-feed-edit-message message-twenty)
            (zulip-runtime-test--drain account)
            (let ((new-owner zulip-feed--edit-operation-owner)
                  (new-generation zulip-feed--edit-generation))
              (goto-char (appkit-chat-timeline-key-position "20"))
              (let ((point-before (point))
                    (history-before
                     (ring-length appkit-chatbuf--input-ring)))
                (funcall patch-callback
                         (zulip-api-result--create :ok-p t))
                (zulip-runtime-test--drain account)
                (should (= (point) point-before))
                (should (= (ring-length appkit-chatbuf--input-ring)
                           history-before)))
              (should (eq zulip-feed--edit-operation-owner new-owner))
              (should (eq zulip-feed--edit-generation new-generation))
              (should (equal (zulip-feed--edit-message-id) "20"))
              (should (zulip-feed--edit-request-p))
              (should (eq (plist-get (zulip-feed--edit-state)
                                     :operation-owner)
                          new-owner))
              (should (equal (appkit-chatbuf-input-string)
                             "new draft after patch cancel"))
              (should (equal (appkit-chatbuf-input-state)
                             "new draft after patch cancel")))))))))

(ert-deftest zulip-feed-send-serializes-structured-mention-but-echoes-label ()
  (zulip-feed-test--with-account account
    (let* ((narrow (zulip-narrow-topic 7 "client"))
           (buffer (zulip-feed--open-buffer account narrow))
           sent-content local-id)
      (with-current-buffer buffer
        (appkit-chat-history-window-establish-empty)
        (zulip-feed-render)
        (appkit-chatbuf-input-set-text
         (concat
          (appkit-chatbuf-input-object-string
           "@Ada"
           '(:kind zulip-mention :user-id "42" :full-name "Ada"
             :wire "@**Ada|42**"))
          "please review"))
        (cl-letf (((symbol-function 'zulip-api-send-message)
                   (lambda (_account _type _to _topic content _callback
                                     &rest _options)
                     (setq sent-content content))))
          (setq local-id (zulip-feed-send-message))
          (should (equal sent-content
                         "@**Ada|42** please review"))
          (let ((pending (zulip-state-message
                          (zulip-account-state account) local-id)))
            (should (equal (zulip-feed--field pending 'content)
                           sent-content))
            (should (equal (zulip-feed--field pending 'local-content)
                           "@Ada please review"))))))))

(ert-deftest zulip-feed-reaction-chip-toggles-exact-server-reaction ()
  (zulip-feed-test--with-account account
    (setf (zulip-state-self-user-id (zulip-account-state account)) "1")
    (let* ((narrow (zulip-narrow-topic 7 "client"))
           (key (zulip-narrow-key narrow))
           (message
            (append
             (zulip-feed-test--message "20" "two")
             '((reactions
                . (((emoji_name . "wave") (emoji_code . "1f44b")
                    (reaction_type . "unicode_emoji") (user_id . 1)))))))
           (state (zulip-state-merge-messages
                   (zulip-account-state account) (list message) key))
           captured)
      (zulip-runtime-publish-state account state)
      (let ((buffer (zulip-feed--open-buffer account narrow)))
        (with-current-buffer buffer
          (appkit-chat-history-window-set "20" nil)
          (zulip-feed-render)
          (let ((reaction (car (zulip-feed--reaction-groups message))))
            (cl-letf (((symbol-function 'zulip-api-remove-reaction)
                       (lambda (_account id callback &rest options)
                         (setq captured (list id options))
                         (funcall callback
                                  (zulip-api-result--create :ok-p t)))))
              (zulip-feed-toggle-reaction reaction "20")
              (should (equal (car captured) "20"))
              (should (equal (plist-get (cadr captured) :emoji-name)
                             "wave"))
              (should (equal (plist-get (cadr captured) :emoji-code)
                             "1f44b"))
              (should (equal (plist-get (cadr captured) :reaction-type)
                             "unicode_emoji"))
              (should (eq (plist-get (cadr captured) :owner)
                          (appkit-current-surface))))))))))

(provide 'zulip-feed-test)

;;; zulip-feed-test.el ends here
