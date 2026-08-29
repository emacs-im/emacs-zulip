;;; zulip-feed-test.el --- Tests for Zulip narrow/feed slice -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
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
  "Create account BINDING for BODY and clean up its views."
  (declare (indent 1) (debug (symbolp body)))
  `(let* ((state (zulip-state-create))
          (,binding
           (zulip-runtime-create-account
            :server "https://chat.example.test/"
            :email "ada@example.test"
            :api-key "secret"
            :state state))
          (buffers-before (buffer-list)))
     (setf (zulip-account-queue-id ,binding) "queue-1")
     (unwind-protect
         (progn ,@body)
       (zulip-runtime-stop-account ,binding)
       (dolist (buffer (buffer-list))
         (when (and (not (memq buffer buffers-before))
                    (buffer-live-p buffer))
           (kill-buffer buffer))))))

(ert-deftest zulip-feed-sender-face-colors-stable-user-identity ()
  (let* ((original
          '((sender_id . 2)
            (sender_full_name . "Original Name")))
         (renamed
          '((sender_id . 2)
            (sender_full_name . "Renamed User")))
         (expected
          (list (appkit-name-color-face "2")
                'zulip-message-sender-face)))
    (should (equal expected (zulip-feed--message-sender-face original)))
    (should
     (equal (zulip-feed--message-sender-face original)
            (zulip-feed--message-sender-face renamed)))))

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
      (zulip-feed--set-account-state account state)
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
      (zulip-feed--set-account-state account state)
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
      (zulip-feed--set-account-state account state)
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
        (should (equal (appkit-view-id (appkit-current-view))
                       (zulip-feed--view-id account alex-two)))
        (should (equal (zulip-narrow-key zulip-feed--narrow)
                       '((dm 2)))))
      (with-current-buffer three-buffer
        (should (equal (appkit-view-id (appkit-current-view))
                       (zulip-feed--view-id account alex-three)))
        (should (equal (zulip-narrow-key zulip-feed--narrow)
                       '((dm 3))))))))

(ert-deftest zulip-feed-history-load-uses-anchor-window-and-narrow-json ()
  (zulip-feed-test--with-account account
    (let* ((narrow (zulip-narrow-channel 7))
           (buffer (zulip-feed--open-buffer account narrow))
           (real-request-sync (symbol-function 'appkit-request-sync))
           (real-sync (symbol-function 'appkit-sync-invalidations))
           (direct-syncs 0)
           requested-timers
           call)
      (cl-letf (((symbol-function 'appkit-request-sync)
                 (lambda (view &rest options)
                   ;; Keep the owned timer dormant until the assertions below;
                   ;; this makes callback-vs-projection ordering deterministic.
                   (let ((timer
                          (apply real-request-sync view
                                 (append options '(:delay 60)))))
                     (push timer requested-timers)
                     timer)))
                ((symbol-function 'appkit-sync-invalidations)
                 (lambda (view)
                   (cl-incf direct-syncs)
                   (funcall real-sync view)))
                ((symbol-function 'zulip-api-get-messages)
                 (lambda (_account wire anchor before after callback
                          &rest _options)
                   (setq call (list wire anchor before after))
                   (funcall
                    callback
                    (zulip-api-result--create
                     :ok-p t
                     :data
                     (list
                      (cons 'messages
                            (vector
                             (zulip-feed-test--message "31" "history")))
                      (cons 'found_oldest t)
                      (cons 'found_newest t)))))))
        (with-current-buffer buffer
          (setq-local zulip-feed--pending-jump-id "31")
          (zulip-feed-load-latest)
          (should
           (equal call
                  (list "[{\"operator\":\"channel\",\"operand\":7}]"
                        "newest" zulip-history-page-size 0)))
          (should (appkit-chat-history-window-known-p))
          (should (appkit-chat-history-older-loaded-p))
          ;; The HTTP callback updates canonical history immediately, but may
          ;; only request projection.  Its event and frame invalidations share
          ;; one owned timer and do not re-enter the view synchronously.
          (should (= direct-syncs 0))
          (should-not (appkit-chat-timeline-keys))
          (should (equal zulip-feed--pending-jump-id "31"))
          ;; Request begin, history event fanout, and completion frame state
          ;; all invalidate through Appkit and share one owned timer.
          (should (= (length requested-timers) 3))
          (should (eq (nth 0 requested-timers)
                      (nth 1 requested-timers)))
          (should (eq (nth 1 requested-timers)
                      (nth 2 requested-timers)))
          (let ((pending (appkit-view-invalidations
                          (appkit-current-view))))
            (should (memq 'timeline (appkit-invalidations-parts pending)))
            (should (memq 'frame (appkit-invalidations-parts pending)))
            (should (equal (appkit-invalidations-entry-keys pending)
                           '("31"))))
          (funcall real-sync (appkit-current-view))
          (should (= direct-syncs 0))
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
        (zulip-feed--on-register account (zulip-state-create))
        (should (= calls 1))
        (with-current-buffer buffer
          (should (eq (appkit-chat-history-loading) 'latest)))))))

(ert-deftest zulip-feed-register-initial-load-defers-frame-to-appkit-sync ()
  (zulip-feed-test--with-account account
    (let* ((narrow (zulip-narrow-all))
           (buffer (zulip-feed--open-buffer account narrow))
           (real-request-sync (symbol-function 'appkit-request-sync))
           (real-sync (symbol-function 'appkit-sync-invalidations))
           (real-update-frame (symbol-function 'zulip-feed--update-frame))
           (register-active-p nil)
           (direct-frame-updates 0)
           (frame-updates 0)
           requested-timers
           (history-calls 0))
      (cl-letf (((symbol-function 'appkit-request-sync)
                 (lambda (view &rest options)
                   (let ((timer
                          (apply real-request-sync view
                                 (append options '(:delay 60)))))
                     (push timer requested-timers)
                     timer)))
                ((symbol-function 'zulip-feed--update-frame)
                 (lambda ()
                   (cl-incf frame-updates)
                   (when register-active-p
                     (cl-incf direct-frame-updates))
                   (funcall real-update-frame)))
                ((symbol-function 'zulip-api-get-messages)
                 (lambda (&rest _arguments)
                   (cl-incf history-calls)
                   'register-history-request)))
        (setf (zulip-account-connected-p account) t)
        (setq register-active-p t)
        (unwind-protect
            (zulip-feed--on-register account (zulip-state-create))
          (setq register-active-p nil))
        (should (= history-calls 1))
        (should (= direct-frame-updates 0))
        (should (= frame-updates 0))
        ;; Register fanout and history-request state coalesce into one timer.
        (should (= (length requested-timers) 2))
        (should (eq (car requested-timers) (cadr requested-timers)))
        (with-current-buffer buffer
          (should (eq (appkit-chat-history-loading) 'latest))
          (let ((pending (appkit-view-invalidations
                          (appkit-current-view))))
            (should (memq 'timeline (appkit-invalidations-parts pending)))
            (should (memq 'frame (appkit-invalidations-parts pending)))
            (should (memq 'composer (appkit-invalidations-parts pending))))
          ;; Only the Appkit transaction may now mutate generated frame text.
          (funcall real-sync (appkit-current-view)))
        (should (= direct-frame-updates 0))
        (should (= frame-updates 1))))))

(ert-deftest zulip-feed-replacement-view-resets-buffer-local-ownership ()
  (zulip-feed-test--with-account account
    (setf (zulip-account-connected-p account) t)
    (let* ((narrow (zulip-narrow-topic 7 "client"))
           (buffer (zulip-feed--open-buffer account narrow))
           old-view old-owner old-request-table replacement)
      (with-current-buffer buffer
        (setq old-view (appkit-current-view)
              old-request-table (appkit-view-request-table old-view))
        ;; A live view reuse preserves its controller and draft state; only a
        ;; newly attached replacement is allowed to reset ownership.
        (should (eq buffer (zulip-feed--open-buffer account narrow)))
        (should (eq old-view (appkit-current-view)))
        (setq old-owner (appkit-chat-history-request-begin 'older))
        (puthash zulip-feed--history-request-key
                 'stale-history-request old-request-table)
        (appkit-chatbuf-input-set-text "stale edit")
        (zulip-feed--set-edit-state "stale-message" t nil "stale draft")
        (setq-local zulip-feed--last-error "stale error"
                    zulip-feed--latest-live-keys '("stale-live")
                    zulip-feed--history-reload-needed-p t
                    zulip-feed--pending-jump-id "stale-jump"
                    zulip-feed--last-read-target-id "stale-read")
        (puthash "stale-read" t zulip-feed--pending-read-ids)
        (puthash "stale-unread" t zulip-feed--auto-read-suppressed-ids))
      (appkit-kill-view old-view)
      (should (buffer-live-p buffer))
      (with-current-buffer buffer
        (should-not (appkit-current-view))
        ;; This is the stale loading gate that used to suppress initial load.
        (should (eq (appkit-chat-history-loading) 'older)))
      (let ((history-calls 0))
        (cl-letf (((symbol-function 'zulip-api-get-messages)
                   (lambda (&rest _arguments)
                     (cl-incf history-calls)
                     'replacement-history-request)))
          (save-window-excursion
            (setq replacement (zulip-feed-open account narrow)))
          (should (= history-calls 1))))
      (should (eq replacement buffer))
      (with-current-buffer replacement
        (let ((new-view (appkit-current-view)))
          (should (appkit-view-live-p new-view))
          (should-not (eq new-view old-view))
          (should-not (eq (appkit-view-request-table new-view)
                          old-request-table))
          (should (eq (gethash zulip-feed--history-request-key
                               (appkit-view-request-table new-view))
                      'replacement-history-request))
          (should (eq (gethash zulip-feed--history-request-key
                               old-request-table)
                      'stale-history-request))
          (should-not (appkit-chat-history-request-current-p old-owner))
          (should-not (eq (appkit-chat-history-request-owner) old-owner))
          (should (eq (appkit-chat-history-loading) 'latest))
          (should-not (appkit-chatbuf-aux-active-p))
          (should (equal (appkit-chatbuf-input-string) ""))
          (should-not zulip-feed--pending-jump-id)
          (should (= (hash-table-count zulip-feed--pending-read-ids) 0))
          (should (= (hash-table-count
                      zulip-feed--auto-read-suppressed-ids)
                     0))
          (should-not zulip-feed--last-read-target-id)
          (should-not zulip-feed--last-error)
          (should-not zulip-feed--latest-live-keys)
          (should-not zulip-feed--history-reload-needed-p)
          (should (eq zulip-feed--pending
                      (zulip-feed--account-table account 'pending))))))))

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
            (zulip-feed--on-app-event
             account
             (list (cons 'type "message")
                   (cons 'message server-message)
                   (cons 'local_message_id local-id))
             old-state new-state)
            (appkit-sync-invalidations (appkit-current-view))
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
                     (regexp-quote "**bold** [link](https://example.com)")
                     (buffer-string)))
            ;; The HTTP response may win the race and promote the optimistic
            ;; object before the richer authoritative event arrives.
            (funcall send-callback
                     (zulip-api-result--create
                      :ok-p t :data (list (cons 'id server-id))))
            (appkit-sync-invalidations (appkit-current-view))
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
              (zulip-feed--on-app-event
               account
               (list (cons 'type "message")
                     (cons 'message server-message)
                     (cons 'local_message_id local-id))
               old-state new-state)
              (appkit-sync-invalidations (appkit-current-view)))
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
              (should (equal
                       (get-text-property (1- link-position) 'shr-url)
                       "https://example.com"))
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
          (appkit-sync-invalidations (appkit-current-view))
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
          (appkit-sync-invalidations (appkit-current-view))
          (should-not (zulip-state-message
                       (zulip-account-state account) local-id))
          (should (zulip-state-message
                   (zulip-account-state account) "90071992547409931234"))
          (should (equal (appkit-chat-timeline-keys)
                         '("90071992547409931234"))))))))

(ert-deftest zulip-feed-send-preserves-markdown-whitespace ()
  (zulip-feed-test--with-account account
    (let* ((narrow (zulip-narrow-topic 7 "client"))
           (buffer (zulip-feed--open-buffer account narrow))
           (content "    indented code\nline with break  \n")
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
            (should (equal sent-content content))
            (should (equal (zulip-state-object-get message 'content)
                           content))
            (should (equal (zulip-state-object-get message 'local-content)
                           content))
            (should (string-match-p (regexp-quote content)
                                    (buffer-string)))))))))

(ert-deftest zulip-feed-local-echo-treats-markdown-as-plain-text ()
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
          (should (string-match-p (regexp-quote content)
                                  (buffer-string))))))))

(ert-deftest zulip-feed-register-invalidates-old-history-owner ()
  (zulip-feed-test--with-account account
    (let* ((narrow (zulip-narrow-channel 7))
           (key (zulip-narrow-key narrow))
           (state (zulip-state-merge-messages
                   (zulip-account-state account)
                   (list (zulip-feed-test--message "11" "old edge"))
                   key))
           (buffer (zulip-feed--open-buffer account narrow))
           calls
           old-owner)
      (zulip-feed--set-account-state account state)
      (cl-letf (((symbol-function 'zulip-api-get-messages)
                 (lambda (_account _wire anchor before after callback
                          &rest _options)
                   (push (list anchor before after callback) calls))))
        (with-current-buffer buffer
          (appkit-chat-history-window-set "11" nil)
          (zulip-feed-load-older)
          (setq old-owner (appkit-chat-history-request-owner)))
        (zulip-feed--on-register account (zulip-state-create))
        (should (= (length calls) 2))
        (with-current-buffer buffer
          (should-not
           (appkit-chat-history-request-current-p old-owner))
          (should (eq (appkit-chat-history-loading) 'latest)))
        (let ((old-callback (nth 3 (cadr calls)))
              (latest-callback (nth 3 (car calls))))
          (funcall
           old-callback
           (zulip-api-result--create
            :ok-p t
            :data (list
                   (cons 'messages
                         (vector (zulip-feed-test--message "5" "stale")))
                   (cons 'found_oldest t)
                   (cons 'found_newest t))))
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
          (should (zulip-state-message
                   (zulip-account-state account) "20"))
          (with-current-buffer buffer
            ;; History completion requests projection; this explicit flush
            ;; represents the scheduler firing after the callback returns.
            (appkit-sync-invalidations (appkit-current-view))
            (should (equal (appkit-chat-timeline-keys) '("20")))))))))

(ert-deftest zulip-feed-sync-acks-events-only-after-success ()
  (zulip-feed-test--with-account account
    (let* ((narrow (zulip-narrow-all))
           (buffer (zulip-feed--open-buffer account narrow)))
      (with-current-buffer buffer
        (let ((view (appkit-current-view))
              (invalidations (appkit-invalidations-create)))
          (appkit-view-enqueue-event view '((type . noop)))
          (cl-letf (((symbol-function 'zulip-feed--sync-timeline)
                     (lambda (&rest _arguments)
                       (error "projection failed"))))
            (should-error
             (zulip-feed--sync-invalidations view invalidations)))
          (should (= 1 (length (appkit-view-pending-events view))))
          (cl-letf (((symbol-function 'zulip-feed--sync-timeline)
                     (lambda (&rest _arguments) nil))
                    ((symbol-function 'zulip-feed--update-frame)
                     (lambda () nil)))
            (zulip-feed--sync-invalidations view invalidations))
          (should-not (appkit-view-pending-events view)))))))

(ert-deftest zulip-feed-frame-sync-does-not-force-redisplay ()
  (zulip-feed-test--with-account account
    (let* ((narrow (zulip-narrow-all))
           (buffer (zulip-feed--open-buffer account narrow)))
      (with-current-buffer buffer
        (cl-letf (((symbol-function 'force-window-update)
                   (lambda (&rest _arguments)
                     (ert-fail "feed sync forced window redisplay")))
                  ((symbol-function 'force-mode-line-update)
                   (lambda (&rest _arguments)
                     (ert-fail "feed sync forced mode-line redisplay"))))
          (zulip-feed-render))))))

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
        (zulip-feed--on-register account (zulip-state-create))
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
        (with-current-buffer buffer
          (should (equal (appkit-chat-timeline-keys) (list local-id))))
        (funcall
         send-callback
         (zulip-api-result--create :ok-p t :data '((id . "101"))))
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
          (zulip-feed--on-app-event
           account (list (cons 'type "message") (cons 'message message))
           old-state new-state))
        (with-current-buffer buffer
          (appkit-sync-invalidations (appkit-current-view))
          (should (equal (appkit-chat-timeline-keys) '("60"))))
        (funcall
         history-callback
         (zulip-api-result--create
          :ok-p t
          :data '((messages . [])
                  (found_oldest . t)
                  (found_newest . t))))
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
          (zulip-feed--on-app-event
           account (list (cons 'type "message") (cons 'message message))
           old-state new-state))
        (with-current-buffer buffer
          (appkit-sync-invalidations (appkit-current-view)))
        (funcall history-callback
                 (zulip-api-result--create
                  :ok-p nil :message "offline"))
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
      (zulip-feed--set-account-state account state)
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
      (zulip-feed--set-account-state account state)
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
      (zulip-feed--set-account-state account state)
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
      (zulip-feed--set-account-state account state)
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
      (zulip-feed--set-account-state account state)
      (let ((buffer (zulip-feed--open-buffer account narrow)))
        (with-current-buffer buffer
          (appkit-chat-history-window-set "11" nil)
          (zulip-feed-render))
        (let* ((old-state (zulip-account-state account))
               (new-state (zulip-state-delete-message old-state "11")))
          (zulip-feed--on-app-event
           account '((type . delete_message) (message_id . "11"))
           old-state new-state))
        (with-current-buffer buffer
          (appkit-sync-invalidations (appkit-current-view))
          (should (equal (appkit-chat-history-window-first-key) "20"))
          (should (equal (appkit-chat-timeline-keys)
                         '("20" "30" "40")))
          (appkit-chat-history-window-set "20" "40")
          (zulip-feed-render))
        ;; Moving a boundary out of the narrow has the same repair semantics as
        ;; deletion; the event type is intentionally not delete_message.
        (let* ((old-state (zulip-account-state account))
               (new-state (zulip-state-delete-message old-state "40")))
          (zulip-feed--on-app-event
           account '((type . update_message) (message_id . "40"))
           old-state new-state))
        (with-current-buffer buffer
          (appkit-sync-invalidations (appkit-current-view))
          (should (equal (appkit-chat-history-window-last-key) "30"))
          (should (equal (appkit-chat-timeline-keys) '("20" "30"))))
        ;; If an unexhausted attached page disappears completely, reload
        ;; instead of falsely claiming that the entire narrow is empty.
        (let ((reload-calls 0))
          (cl-letf (((symbol-function 'zulip-api-get-messages)
                     (lambda (&rest _arguments)
                       (cl-incf reload-calls))))
            (with-current-buffer buffer
              (appkit-chat-history-window-set "20" nil)
              (appkit-chat-history-older-loaded-set nil))
            (let* ((old-state (zulip-account-state account))
                   (without-20
                    (zulip-state-delete-message old-state "20"))
                   (new-state
                    (zulip-state-delete-message without-20 "30")))
              (zulip-feed--on-app-event
               account
               '((type . delete_message) (message_ids . ["20" "30"]))
               old-state new-state))
            (with-current-buffer buffer
              (appkit-sync-invalidations (appkit-current-view))
              (should (= reload-calls 1))
              (should (eq (appkit-chat-history-loading) 'latest))
              (should-not (appkit-chat-history-window-empty-p)))))))))

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
      (zulip-feed--set-account-state account state)
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
      (zulip-feed--set-account-state account state)
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

(ert-deftest zulip-feed-history-transport-is-owned-by-view ()
  (zulip-feed-test--with-account account
    (let* ((narrow (zulip-narrow-channel 7))
           (buffer (zulip-feed--open-buffer account narrow))
           observed-owner)
      (cl-letf (((symbol-function 'zulip-api-get-messages)
                 (lambda (_account _wire _anchor _before _after _callback
                          &rest options)
                   (setq observed-owner (plist-get options :owner))
                   'fake-request)))
        (with-current-buffer buffer
          (zulip-feed-load-latest)
          (should (eq observed-owner (appkit-current-view)))
          (should (eq 'fake-request
                      (gethash zulip-feed--history-request-key
                               (appkit-view-request-table
                                (appkit-current-view))))))))))

(ert-deftest zulip-feed-uses-appkit-chatbuf-completion-and-history-adapters ()
  (zulip-feed-test--with-account account
    (let ((buffer (zulip-feed--open-buffer
                   account (zulip-narrow-topic 7 "client"))))
      (with-current-buffer buffer
        (should (derived-mode-p 'appkit-chatbuf-mode))
        (should (memq 'zulip-completion-mention-capf
                      completion-at-point-functions))
        (should (memq 'appkit-chat-emoji-capf
                      completion-at-point-functions))
        (should (eq (key-binding (kbd "M-p"))
                    #'zulip-feed-draft-previous))
        (should (eq (lookup-key zulip-feed-mode-map (kbd "C-c C-a"))
                    #'zulip-message-transient))
        (should (eq (lookup-key zulip-feed-mode-map (kbd "C-c C-t"))
                    #'zulip-feed-open-topic))
        (should (eq (lookup-key zulip-feed-mode-map (kbd "RET"))
                    #'zulip-feed-return-dwim))))))

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
      (zulip-feed--set-account-state account state)
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
            (should (eq (nth 3 captured) (appkit-current-view)))
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
      (zulip-feed--set-account-state account state)
      (let ((buffer (zulip-feed--open-buffer account narrow)))
        (with-current-buffer buffer
          (appkit-chat-history-window-set "11" nil)
          (zulip-feed-render)
          (goto-char (appkit-chat-timeline-key-position "20"))
          (setq view (appkit-current-view))
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
          (should (= (hash-table-count zulip-feed--pending-read-ids) 2))
          (should (eq (gethash "11" zulip-feed--pending-read-ids) 'foreign))
          (should (eq (gethash "20" zulip-feed--pending-read-ids) 'foreign))
          (should (equal zulip-feed--last-read-target-id "20")))
        (with-current-buffer buffer
          (should (= (hash-table-count zulip-feed--pending-read-ids) 0))
          (should-not zulip-feed--last-read-target-id)
          (should (equal zulip-feed--last-error "network down"))
          (let ((pending (appkit-view-invalidations view)))
            (should (memq 'frame (appkit-invalidations-parts pending)))
            (should (memq 'composer
                          (appkit-invalidations-parts pending)))))))))

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
      (zulip-feed--set-account-state account state)
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
            (zulip-feed--set-account-state
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
      (zulip-feed--set-account-state account state)
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
            (should (eq (nth 3 captured) (appkit-current-view)))))))))

(ert-deftest zulip-feed-action-errors-request-one-coalesced-sync ()
  (zulip-feed-test--with-account account
    (let* ((narrow (zulip-narrow-topic 7 "client"))
           (key (zulip-narrow-key narrow))
           (message (append (zulip-feed-test--message "20" "two")
                            '((flags . ("read")))))
           (state (zulip-state-merge-messages
                   (zulip-account-state account) (list message) key))
           (real-request-sync (symbol-function 'appkit-request-sync))
           (real-sync (symbol-function 'appkit-sync-invalidations))
           (direct-syncs 0)
           requested-timers
           callbacks)
      (zulip-feed--set-account-state account state)
      (let ((buffer (zulip-feed--open-buffer account narrow)))
        (with-current-buffer buffer
          (appkit-chat-history-window-set "20" nil)
          (zulip-feed-render)
          (goto-char (appkit-chat-timeline-key-position "20"))
          (cl-letf (((symbol-function 'appkit-request-sync)
                     (lambda (view &rest options)
                       (let ((timer
                              (apply real-request-sync view
                                     (append options '(:delay 60)))))
                         (push timer requested-timers)
                         timer)))
                    ((symbol-function 'appkit-sync-invalidations)
                     (lambda (view)
                       (cl-incf direct-syncs)
                       (funcall real-sync view)))
                    ((symbol-function 'zulip-api-update-message-flags)
                     (lambda (_account _ids _operation _flag callback
                              &rest _options)
                       (setq callbacks
                             (append callbacks (list callback))))))
            ;; Two independent HTTP completions land before the display timer.
            ;; Each records domain error state, but neither may synchronously
            ;; mutate generated content.
            (zulip-feed-toggle-star)
            (zulip-feed-toggle-star)
            (funcall (nth 0 callbacks)
                     (zulip-api-result--create
                      :ok-p nil :message "offline one"))
            (funcall (nth 1 callbacks)
                     (zulip-api-result--create
                      :ok-p nil :message "offline two"))
            (should (= direct-syncs 0))
            (should (equal zulip-feed--last-error "offline two"))
            (should (= (length requested-timers) 2))
            (should (eq (car requested-timers) (cadr requested-timers)))
            (let ((pending (appkit-view-invalidations
                            (appkit-current-view))))
              (should (memq 'frame (appkit-invalidations-parts pending)))
              (should (memq 'composer (appkit-invalidations-parts pending))))
            (should-not (string-match-p
                         "Request failed: offline two" (buffer-string)))
            (funcall real-sync (appkit-current-view))
            (should (= direct-syncs 0))
            (should (string-match-p
                     "Request failed: offline two" (buffer-string)))))))))

(ert-deftest zulip-feed-edit-stages-raw-markdown-and-submits-through-view ()
  (zulip-feed-test--with-account account
    (let* ((narrow (zulip-narrow-topic 7 "client"))
           (key (zulip-narrow-key narrow))
           (message (zulip-feed-test--message "20" "<p>old</p>"))
           (state (zulip-state-merge-messages
                   (zulip-account-state account) (list message) key))
           get-owner update-call)
      (zulip-feed--set-account-state account state)
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
                       (funcall
                        callback
                        (zulip-api-result--create
                         :ok-p t :data '((raw_content . "**old**"))))))
                    ((symbol-function 'zulip-api-update-message)
                     (lambda (_account id callback &rest options)
                       (setq update-call (list id options))
                       (funcall callback
                                (zulip-api-result--create :ok-p t)))))
            (appkit-chatbuf-input-set-text "unsent draft")
            (goto-char (appkit-chat-timeline-key-position "20"))
            (zulip-feed-edit-message)
            (should (eq get-owner (appkit-current-view)))
            (should (eq (appkit-chatbuf-aux-type) 'edit))
            (should (equal (appkit-chatbuf-aux-message-id) "20"))
            (should-not (appkit-chatbuf-composer-idle-p))
            ;; The callback settled canonical state but did not touch the
            ;; generated composer before the exact-view sync transaction.
            (should (equal (appkit-chatbuf-input-string) "unsent draft"))
            (should (equal (appkit-chatbuf-input-state) "**old**"))
            (goto-char (point-max))
            (should-error (insert " racing draft") :type 'buffer-read-only)
            (appkit-sync-invalidations (appkit-current-view))
            (should (equal (appkit-chatbuf-input-string) "**old**"))
            (appkit-chatbuf-input-set-text "**new**")
            (zulip-feed-send-message)
            (should (equal (car update-call) "20"))
            (should (equal (plist-get (cadr update-call) :content)
                           "**new**"))
            (should (eq (plist-get (cadr update-call) :owner)
                        (appkit-current-view)))
            (should-not (appkit-chatbuf-aux-active-p))
            (should (equal (appkit-chatbuf-input-string) "**new**"))
            (should (equal (appkit-chatbuf-input-state) "unsent draft"))
            ;; With aux already settled, an early RET must not send the stale
            ;; generated edit as an ordinary new message.
            (should-error (zulip-feed-send-message) :type 'user-error)
            (appkit-sync-invalidations (appkit-current-view))
            (should (equal (appkit-chatbuf-input-string)
                           "unsent draft"))))))))

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
      (zulip-feed--set-account-state account state)
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
            (appkit-sync-invalidations (appkit-current-view))
            (should (zulip-feed--edit-request-p))
            (should-not zulip-feed--edit-sync-request)
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
              (should (equal (zulip-completion-serialize-input input)
                             "@**Ada|42** protected draft")))

            (funcall get-callback
                     (zulip-api-result--create
                      :ok-p t :data '((raw_content . "raw source"))))
            (appkit-sync-invalidations (appkit-current-view))
            (should-not buffer-read-only)
            (should (equal (appkit-chatbuf-input-string) "raw source"))
            (zulip-feed-cancel-edit)
            (appkit-sync-invalidations (appkit-current-view))
            (should-not buffer-read-only)
            (let ((restored (appkit-chatbuf-input-string)))
              (should (equal restored "@Ada protected draft"))
              (should (eq (plist-get
                           (get-text-property
                            0 appkit-chatbuf-input-object-property restored)
                           :kind)
                          'zulip-mention))
              (should (equal (zulip-completion-serialize-input restored)
                             "@**Ada|42** protected draft")))
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
      (zulip-feed--set-account-state account state)
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
            (appkit-sync-invalidations (appkit-current-view))
            (should-not buffer-read-only)
            (should (equal (appkit-chatbuf-input-string) "raw source"))
            (appkit-chatbuf-input-set-text "edited raw source")
            (zulip-feed-submit-edit)
            (should (equal patch-content "edited raw source"))
            ;; A frame-only sync must not accidentally unlock the composer
            ;; while PATCH still owns this generation.
            (appkit-sync-invalidations (appkit-current-view))
            (should (zulip-feed--edit-request-p))
            (should-not zulip-feed--edit-sync-request)
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
            (appkit-sync-invalidations (appkit-current-view))
            (should-not buffer-read-only)
            (let ((restored (appkit-chatbuf-input-string)))
              (should (equal restored "@Ada protected draft"))
              (should (eq (plist-get
                           (get-text-property
                            0 appkit-chatbuf-input-object-property restored)
                           :kind)
                          'zulip-mention))
              (should (equal (zulip-completion-serialize-input restored)
                             "@**Ada|42** protected draft")))
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
      (zulip-feed--set-account-state account state)
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
          (appkit-chatbuf-input-set-text draft)
          (goto-char (appkit-chat-timeline-key-position "20"))
          (cl-letf (((symbol-function 'zulip-api-get-message)
                     (lambda (_account _id callback &rest _options)
                       (funcall callback
                                (zulip-api-result--create
                                 :ok-p t :data '((raw_content . "raw")))))))
            (zulip-feed-edit-message)
            (let ((undo-before buffer-undo-list))
              (appkit-sync-invalidations (appkit-current-view))
              (should (equal buffer-undo-list undo-before)))
            (should (equal (appkit-chatbuf-input-string) "raw"))
            (zulip-feed-cancel-edit)
            (should (equal (appkit-chatbuf-input-string) "raw"))
            (let ((undo-before buffer-undo-list))
              (appkit-sync-invalidations (appkit-current-view))
              (should (equal buffer-undo-list undo-before)))
            (let* ((restored (appkit-chatbuf-input-string))
                   (object (get-text-property
                            0 appkit-chatbuf-input-object-property restored)))
              (should (equal restored "@Ada later"))
              (should (eq (plist-get object :kind) 'zulip-mention))
              (should (equal (zulip-completion-serialize-input restored)
                             "@**Ada|42** later")))))))))

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
      (zulip-feed--set-account-state account state)
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
              (appkit-sync-invalidations (appkit-current-view))
              (zulip-feed-cancel-edit)
              (appkit-sync-invalidations (appkit-current-view))
              (appkit-chatbuf-input-set-text "draft after cancel")
              (zulip-feed-edit-message message-thirty)
              (appkit-sync-invalidations (appkit-current-view))
              (let ((new-owner zulip-feed--edit-operation-owner)
                    (new-generation zulip-feed--edit-generation))
                (goto-char (appkit-chat-timeline-key-position "30"))
                (let ((point-before (point)))
                  (funcall stale-get
                           (zulip-api-result--create
                            :ok-p t :data '((raw_content . "stale raw"))))
                  (should (= (point) point-before)))
                (should (eq zulip-feed--edit-operation-owner new-owner))
                (should (eq zulip-feed--edit-generation new-generation))
                (should (equal (zulip-feed--edit-message-id) "30"))
                (should (zulip-feed--edit-request-p))
                (should (equal (appkit-chatbuf-input-string)
                               "draft after cancel"))
                (should (equal (appkit-chatbuf-input-state)
                               "draft after cancel"))))

            ;; The accepted GET updates canonical state only; the exact-view
            ;; sync materializes it and performs the deferred point move.
            (let ((current-get (gethash "30" get-callbacks))
                  (point-before (point)))
              (funcall current-get
                       (zulip-api-result--create
                        :ok-p t :data '((raw_content . "raw thirty"))))
              (should (= (point) point-before))
              (should (equal (appkit-chatbuf-input-string)
                             "draft after cancel"))
              (should (equal (appkit-chatbuf-input-state) "raw thirty"))
              (appkit-sync-invalidations (appkit-current-view))
              (should (equal (appkit-chatbuf-input-string) "raw thirty"))
              (should (= (point)
                         (appkit-chatbuf-input-logical-end-position))))

            ;; Cancel an in-flight PATCH, make another draft, and begin a new
            ;; edit.  Its delayed success cannot restore the old saved draft,
            ;; clear the new aux owner, push history, or move point.
            (appkit-chatbuf-input-set-text "edited thirty")
            (zulip-feed-submit-edit)
            (should patch-callback)
            (appkit-sync-invalidations (appkit-current-view))
            (zulip-feed-cancel-edit)
            (appkit-sync-invalidations (appkit-current-view))
            (appkit-chatbuf-input-set-text "new draft after patch cancel")
            (zulip-feed-edit-message message-twenty)
            (appkit-sync-invalidations (appkit-current-view))
            (let ((new-owner zulip-feed--edit-operation-owner)
                  (new-generation zulip-feed--edit-generation))
              (goto-char (appkit-chat-timeline-key-position "20"))
              (let ((point-before (point))
                    (history-before
                     (ring-length appkit-chatbuf--input-ring)))
                (funcall patch-callback
                         (zulip-api-result--create :ok-p t))
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
      (zulip-feed--set-account-state account state)
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
                          (appkit-current-view))))))))))

(provide 'zulip-feed-test)

;;; zulip-feed-test.el ends here
