;;; zulip-api-test.el --- Tests for strict Zulip HTTP/API results -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'zulip-runtime-test)
(require 'cl-lib)
(require 'zulip-api)
(require 'zulip-runtime)

(defun zulip-api-test--account ()
  "Return a real account in the isolated test registry."
  (let* ((account
          (zulip-runtime-create-account
           :server "https://chat.example.test/"
           :email "person@example.test"
           :api-key "secret-key")))
    (setf (zulip-account-longpoll-timeout account) 73)
    account))

(defun zulip-api-test--surface (account)
  "Return a real Generated Surface owned by isolated ACCOUNT."
  (let ((buffer (zulip-feed--open-buffer account (zulip-narrow-all))))
    (cl-pushnew buffer zulip-runtime-test--buffers)
    (with-current-buffer buffer (appkit-current-surface))))

(defun zulip-api-test--response (status body &optional headers)
  "Return a plz response with STATUS, BODY, and HEADERS."
  (make-plz-response
   :version 2
   :status status
   :headers (or headers '((content-type . "application/json")))
   :body body))

(ert-deftest zulip-http-basic-auth-uses-account-credentials ()
  (zulip-runtime-test--isolated
    (let* ((account (zulip-api-test--account))
           (header (zulip-http-basic-auth account)))
      (should (string-prefix-p "Basic " header))
      (should (equal "person@example.test:secret-key"
                     (base64-decode-string (substring header 6)))))))

(ert-deftest zulip-http-get-encodes-query-and-delivers-success-result ()
  (let ((process (make-pipe-process :name "zulip-api-test-pipe" :noquery t)))
    (unwind-protect
        (zulip-runtime-test--isolated
          (let* ((account (zulip-api-test--account))
                 captured
                 result)
            (cl-letf (((symbol-function 'plz)
                       (lambda (method url &rest arguments)
                         (setq captured (list method url arguments))
                         (delete-process process)
                         (funcall
                          (plist-get arguments :then)
                          (zulip-api-test--response
                           200 "{\"result\":\"success\",\"value\":42}"))
                         process)))
              (zulip-http-request
               account 'get "/example"
               '(("queue_id" . "queue/string") ("last_event_id" . "9007199254740993"))
               (lambda (value) (setq result value)))
              (zulip-runtime-test--drain account))
            (should (eq 'get (car captured)))
            (should (string-match-p
                     "queue_id=queue%2Fstring"
                     (cadr captured)))
            (should (string-match-p
                     "last_event_id=9007199254740993"
                     (cadr captured)))
            (should-not (plist-get (nth 2 captured) :body))
            (let* ((headers (plist-get (nth 2 captured) :headers))
                   (authorization (cdr (assoc "Authorization" headers))))
              (should (equal (zulip-http-basic-auth account) authorization)))
            (should (zulip-api-result-p result))
            (should (zulip-api-result-ok-p result))
            (should (= 200 (zulip-api-result-status result)))
            (should (= 42 (gethash "value" (zulip-api-result-data result))))))
      (when (process-live-p process)
        (delete-process process)))))

(ert-deftest zulip-http-post-encodes-form-body ()
  (let ((process (make-pipe-process :name "zulip-api-test-pipe" :noquery t)))
    (unwind-protect
        (zulip-runtime-test--isolated
          (let* ((account (zulip-api-test--account))
                 captured)
            (cl-letf (((symbol-function 'plz)
                       (lambda (method url &rest arguments)
                         (setq captured (list method url arguments))
                         process)))
              (zulip-http-request
               account 'post "/messages"
               '(("type" . "direct") ("to" . [42 9007199254740993]))
               #'ignore)
              (zulip-runtime-test--drain account))
            (should (eq 'post (car captured)))
            (should-not (string-match-p "\\?" (cadr captured)))
            (let ((body (plist-get (nth 2 captured) :body))
                  (headers (plist-get (nth 2 captured) :headers)))
              (should (string-match-p "type=direct" body))
              (should (string-match-p "to=%5B42%2C9007199254740993%5D" body))
              (should (equal "application/x-www-form-urlencoded"
                             (cdr (assoc "Content-Type" headers)))))))
      (when (process-live-p process)
        (delete-process process)))))

(ert-deftest zulip-api-wire-message-id-array-is-exact-and-keeps-strings-quoted ()
  (zulip-runtime-test--isolated
    (let ((huge-id "900719925474099312345678901234567890"))
      (should (equal (concat "[" huge-id "]")
                     (zulip-api--wire-message-id-array (vector huge-id))))
      (should (equal (concat "[\"" huge-id "\"]")
                     (zulip-http--json-encode (vector huge-id)))))))

(ert-deftest zulip-api-wire-message-id-array-rejects-invalid-or-injected-text ()
  (zulip-runtime-test--isolated
    (dolist (invalid '("" "01" "-1" "+1" "1.0" "1e3"
                       "1,2" "1]" "1 null" "1\n2" 42))
      (let ((condition
             (should-error
              (zulip-api--wire-message-id-array (vector invalid)))))
        (should (eq 'error (car condition)))
        (should (string-prefix-p "Invalid Zulip message ID for JSON"
                                 (cadr condition)))))))

(ert-deftest zulip-api-message-flags-emit-exact-json-integer-ids ()
  (zulip-runtime-test--isolated
    (let* ((account (zulip-api-test--account))
           (huge-id "900719925474099312345678901234567890")
           captured processes)
      (unwind-protect
          (cl-letf (((symbol-function 'plz)
                     (lambda (method url &rest arguments)
                       (setq captured (list method url arguments))
                       (let ((process (make-pipe-process
                                       :name "zulip-api-flags-test-pipe"
                                       :noquery t)))
                         (push process processes)
                         process))))
            (zulip-api-update-message-flags
             account (vector huge-id "42") 'add "read" #'ignore)
            (zulip-runtime-test--drain account)
            (should (eq 'post (car captured)))
            (should (string-suffix-p "/api/v1/messages/flags" (cadr captured)))
            (should
             (equal
              (concat "messages=%5B" huge-id "%2C42%5D&op=add&flag=read")
              (plist-get (nth 2 captured) :body)))
            (zulip-api-update-message-flags-for-narrow
             account [((operator . "is") (operand . "unread"))]
             huge-id 50 0 'remove "starred" #'ignore :include-anchor nil)
            (zulip-runtime-test--drain account)
            (should (eq 'post (car captured)))
            (should (string-suffix-p "/api/v1/messages/flags/narrow"
                                     (cadr captured)))
            (should
             (equal
              (concat "anchor=" huge-id
                      "&include_anchor=false&num_before=50&num_after=0"
                      "&narrow=%5B%7B%22operator%22%3A%22is%22%2C"
                      "%22operand%22%3A%22unread%22%7D%5D&op=remove&flag=starred")
              (plist-get (nth 2 captured) :body))))
        (dolist (process processes)
          (when (process-live-p process) (delete-process process)))))))

(ert-deftest zulip-http-patch-encodes-form-body ()
  (zulip-runtime-test--isolated
    (let* ((account (zulip-api-test--account))
           (message-id "90071992547409931234")
           captured processes)
      (unwind-protect
          (cl-letf (((symbol-function 'plz)
                     (lambda (method url &rest arguments)
                       (setq captured (list method url arguments))
                       (let ((process (make-pipe-process
                                       :name "zulip-api-edit-test-pipe"
                                       :noquery t)))
                         (push process processes)
                         process))))
            (zulip-api-update-message
             account message-id #'ignore
             :topic "" :propagate-mode "change_all"
             :content "edited markdown" :prev-content-sha256 "sha256"
             :stream-id 42 :send-notification-to-old-thread nil
             :send-notification-to-new-thread t)
            (zulip-runtime-test--drain account)
            (should (eq 'patch (car captured)))
            (should (string-suffix-p
                     "/api/v1/messages/90071992547409931234" (cadr captured)))
            (should-not (string-search "?" (cadr captured)))
            (let ((body (plist-get (nth 2 captured) :body))
                  (headers (plist-get (nth 2 captured) :headers)))
              (should
               (equal (concat "topic=&propagate_mode=change_all"
                              "&send_notification_to_old_thread=false"
                              "&send_notification_to_new_thread=true"
                              "&content=edited%20markdown"
                              "&prev_content_sha256=sha256&stream_id=42")
                      body))
              (should (equal "application/x-www-form-urlencoded"
                             (cdr (assoc "Content-Type" headers)))))
            (zulip-api-delete-message account message-id #'ignore)
            (zulip-runtime-test--drain account)
            (should (eq 'delete (car captured)))
            (should (string-suffix-p
                     "/api/v1/messages/90071992547409931234" (cadr captured)))
            (should-not (plist-get (nth 2 captured) :body))
            (should-error (zulip-api-delete-message account 42 #'ignore)))
        (dolist (process processes)
          (when (process-live-p process) (delete-process process)))))))

(ert-deftest zulip-http-delete-encodes-queue-id-in-query ()
  (let ((process (make-pipe-process :name "zulip-api-test-pipe" :noquery t)))
    (unwind-protect
        (zulip-runtime-test--isolated
          (let* ((account (zulip-api-test--account))
                 captured)
            (cl-letf (((symbol-function 'plz)
                       (lambda (method url &rest arguments)
                         (setq captured (list method url arguments))
                         process)))
              (zulip-api-delete-queue account "queue/id:opaque" #'ignore)
              (zulip-runtime-test--drain account))
            (should (eq 'delete (car captured)))
            (should (string-match-p
                     "/api/v1/events\\?queue_id=queue%2Fid%3Aopaque\\'"
                     (cadr captured)))
            (should-not (plist-get (nth 2 captured) :body))))
      (when (process-live-p process)
        (delete-process process)))))

(ert-deftest zulip-http-http-error-preserves-response-and-api-error ()
  (let ((process (make-pipe-process :name "zulip-api-test-pipe" :noquery t)))
    (unwind-protect
        (zulip-runtime-test--isolated
          (let* ((account (zulip-api-test--account))
                 result
                 (body "{\"result\":\"error\",\"code\":\"BAD_EVENT_QUEUE_ID\",\"msg\":\"expired\"}"))
            (cl-letf (((symbol-function 'plz)
                       (lambda (_method _url &rest arguments)
                         (delete-process process)
                         (funcall
                          (plist-get arguments :else)
                          (make-plz-error
                           :response
                           (zulip-api-test--response
                            400 body '((content-type . "application/json")
                                       (request-id . "request-1")))))
                         process)))
              (zulip-http-request account 'get "/events" nil
                                  (lambda (value) (setq result value)))
              (zulip-runtime-test--drain account))
            (should (zulip-api-result-p result))
            (should-not (zulip-api-result-ok-p result))
            (should (= 400 (zulip-api-result-status result)))
            (should (equal '((content-type . "application/json")
                             (request-id . "request-1"))
                           (zulip-api-result-headers result)))
            (should (equal body (zulip-api-result-raw-body result)))
            (should (hash-table-p (zulip-api-result-data result)))
            (should (equal "BAD_EVENT_QUEUE_ID" (zulip-api-result-code result)))
            (should (equal "BAD_EVENT_QUEUE_ID"
                           (zulip-api-result-error-code result)))
            (should (equal "expired" (zulip-api-result-message result)))
            (should-not (zulip-api-result-transport-error result))))
      (when (process-live-p process)
        (delete-process process)))))

(ert-deftest zulip-http-transport-error-is-a-result-not-nil ()
  (let ((process (make-pipe-process :name "zulip-api-test-pipe" :noquery t)))
    (unwind-protect
        (zulip-runtime-test--isolated
          (let* ((account (zulip-api-test--account))
                 result
                 (transport (make-plz-error
                             :curl-error '(6 . "Could not resolve host."))))
            (cl-letf (((symbol-function 'plz)
                       (lambda (_method _url &rest arguments)
                         (delete-process process)
                         (funcall (plist-get arguments :else) transport)
                         process)))
              (zulip-http-request account 'get "/users/me" nil
                                  (lambda (value) (setq result value)))
              (zulip-runtime-test--drain account))
            (should (zulip-api-result-p result))
            (should-not (zulip-api-result-ok-p result))
            (should (eq transport (zulip-api-result-transport-error result)))
            (should-not (zulip-api-result-status result))
            (should-not (zulip-api-result-data result))))
      (when (process-live-p process)
        (delete-process process)))))

(ert-deftest zulip-http-json-error-preserves-status-and-raw-body ()
  (let ((process (make-pipe-process :name "zulip-api-test-pipe" :noquery t)))
    (unwind-protect
        (zulip-runtime-test--isolated
          (let* ((account (zulip-api-test--account))
                 result)
            (cl-letf (((symbol-function 'plz)
                       (lambda (_method _url &rest arguments)
                         (delete-process process)
                         (funcall (plist-get arguments :then)
                                  (zulip-api-test--response 200 "not json"))
                         process)))
              (zulip-http-request account 'get "/users/me" nil
                                  (lambda (value) (setq result value)))
              (zulip-runtime-test--drain account))
            (should (zulip-api-result-p result))
            (should-not (zulip-api-result-ok-p result))
            (should (= 200 (zulip-api-result-status result)))
            (should (equal "not json" (zulip-api-result-raw-body result)))
            (should (zulip-api-result-parse-error result))))
      (when (process-live-p process)
        (delete-process process)))))

(ert-deftest zulip-http-api-error-in-2xx-is-not-ok ()
  (zulip-runtime-test--isolated
    (let ((result
           (zulip-http--response-result
            (zulip-api-test--response
             200 "{\"result\":\"error\",\"code\":\"BAD_REQUEST\",\"msg\":\"bad\"}"))))
      (should-not (zulip-api-result-ok-p result))
      (should (equal "BAD_REQUEST" (zulip-api-result-code result)))
      (should (equal "bad" (zulip-api-result-message result))))))

(ert-deftest zulip-http-synchronous-setup-error-goes-to-callback ()
  (zulip-runtime-test--isolated
    (let* ((account (zulip-api-test--account)) result)
      (cl-letf (((symbol-function 'plz)
                 (lambda (&rest _arguments) (error "curl executable missing"))))
        (let ((handle (zulip-http-request account 'get "/users/me" nil
                                          (lambda (value) (setq result value)))))
          (should (appkit-handle-p handle))
          (zulip-runtime-test--drain account)
          (should-not (appkit-handle-alive-p handle))))
      (should (zulip-api-result-p result))
      (should-not (zulip-api-result-ok-p result))
      (should (zulip-api-result-transport-error result)))))

(ert-deftest zulip-http-preflight-error-goes-to-callback-once ()
  (zulip-runtime-test--isolated
    (let* ((account (zulip-api-test--account)) (calls 0) result)
      (cl-letf (((symbol-function 'plz)
                 (lambda (&rest _arguments)
                   (ert-fail "plz must not run after a preflight error"))))
        (let ((handle (zulip-http-request
                       account 42 "/users/me" nil
                       (lambda (value) (cl-incf calls) (setq result value)))))
          (should (appkit-handle-p handle))
          (zulip-runtime-test--drain account)
          (should-not (appkit-handle-alive-p handle))))
      (should (= calls 1))
      (should (zulip-api-result-p result))
      (should-not (zulip-api-result-ok-p result))
      (should (zulip-api-result-transport-error result)))))

(ert-deftest zulip-http-completion-forgets-appkit-handle-and-runs-once ()
  (zulip-runtime-test--isolated
    (let* ((account
            (zulip-runtime-create-account
             :server "https://http-completion-test.invalid"
             :email "http-completion@example.invalid"
             :api-key "secret"))
           (app (zulip-account-app account))
           process
           then
           handle
           (calls 0))
      (unwind-protect
          (cl-letf (((symbol-function 'plz)
                     (lambda (_method _url &rest arguments)
                       (setq then (plist-get arguments :then)
                             process
                             (make-pipe-process
                              :name "zulip-http-completion-test-pipe"
                              :noquery t))
                       process)))
            (let ((request
                    (zulip-http-request
                     account 'get "/users/me" nil
                     (lambda (_result) (cl-incf calls)))))
              (setq handle request)
              (should (appkit-handle-p request)))
            (zulip-runtime-test--drain account)
            (should (appkit-handle-p handle))
            (should (appkit-handle-alive-p handle))
            (funcall then
                     (zulip-api-test--response
                      200 "{\"result\":\"success\"}"))
            (zulip-runtime-test--drain account)
            ;; A defensive duplicate transport completion must be ignored.
            (funcall then
                     (zulip-api-test--response
                      200 "{\"result\":\"success\"}"))
            (zulip-runtime-test--drain account)
            (should (= calls 1))
            (should-not (appkit-handle-alive-p handle))
            (should-not (appkit-app-handles app)))
        (when (and (processp process) (process-live-p process))
          (delete-process process))
        (zulip-runtime-stop-account account)))))

(ert-deftest zulip-http-explicit-surface-owner-cancels-with-the-surface ()
  (zulip-runtime-test--isolated
    (let* ((account (zulip-api-test--account))
           (app (zulip-account-app account))
           (view (zulip-api-test--surface account))
           process then handle captured
           (calls 0))
      (unwind-protect
          (cl-letf (((symbol-function 'plz)
                     (lambda (method url &rest arguments)
                       (setq captured (list method url arguments)
                             then (plist-get arguments :then)
                             process
                             (make-pipe-process
                              :name "zulip-http-view-owner-test-pipe"
                              :noquery t))
                       process)))
            (setq handle
                  (zulip-api-get-message
                   account "90071992547409931234"
                   (lambda (_result) (cl-incf calls))
                   :apply-markdown nil :allow-empty-topic-name nil
                   :owner view))
            (zulip-runtime-test--drain account)
            (should (eq 'get (car captured)))
            (should
             (string-suffix-p
              (concat "/api/v1/messages/90071992547409931234"
                      "?apply_markdown=false&allow_empty_topic_name=false")
              (cadr captured)))
            (should-not (plist-get (nth 2 captured) :body))
            (should (appkit-handle-alive-p handle))
            (should-not (appkit-app-handles app))
            (appkit-surface-stop view)
            (should-not (process-live-p process))
            (should-not (appkit-handle-alive-p handle))
            (should-not (appkit-surface-handles view))
            ;; A completion queued before cancellation remains harmless.
            (funcall then
                     (zulip-api-test--response
                      200 "{\"result\":\"success\"}"))
            (zulip-runtime-test--drain account)
            (should (= calls 0))
            (should-error
             (zulip-api-get-message account 90071992547409931234 #'ignore)))
        (when (and (processp process) (process-live-p process))
          (delete-process process))))))

(ert-deftest zulip-api-register-custom-events-remain-in-fetch-set ()
  (zulip-runtime-test--isolated
    (let ((event-types '("message" "custom_future_event"))
          captured)
      (cl-letf (((symbol-function 'zulip-http-request)
                 (lambda (&rest arguments)
                   (setq captured arguments)
                   :request)))
        (zulip-api-register :account #'ignore event-types))
      (let ((params (nth 3 captured)))
        (should (eq event-types (cdr (assoc "event_types" params))))
        (should (equal ["message"
                        "custom_future_event"
                        "recent_private_conversations"
                        "realm"]
                       (cdr (assoc "fetch_event_types" params))))))))

(ert-deftest zulip-api-events-wrapper-preserves-opaque-identifiers ()
  (zulip-runtime-test--isolated
    (let* ((account (zulip-api-test--account))
           captured)
      (cl-letf (((symbol-function 'zulip-http-request)
                 (lambda (&rest arguments)
                   (setq captured arguments)
                   :request)))
        (zulip-api-get-events
         account "queue-9007199254740993" "90071992547409931234" #'ignore))
      (should (equal (list account 'get "/events")
                     (cl-subseq captured 0 3)))
      (let ((params (nth 3 captured)))
        (should (equal "queue-9007199254740993"
                       (cdr (assoc "queue_id" params))))
        (should (equal "90071992547409931234"
                       (cdr (assoc "last_event_id" params)))))
      (should (= 73 (plist-get (nthcdr 5 captured) :timeout))))))

(ert-deftest zulip-api-get-topics-encodes-explicit-false-and-validates-stream-id ()
  (zulip-runtime-test--isolated
    (let ((account (zulip-api-test--account))
          captured process)
      (unwind-protect
          (cl-letf (((symbol-function 'plz)
                     (lambda (method url &rest arguments)
                       (setq captured (list method url arguments)
                             process (make-pipe-process
                                      :name "zulip-api-topics-test-pipe"
                                      :noquery t))
                       process)))
            (zulip-api-get-topics
             account 42 #'ignore :allow-empty-topic-name nil)
            (zulip-runtime-test--drain account)
            (should (eq 'get (car captured)))
            (should (string-suffix-p
                     "/api/v1/users/me/42/topics?allow_empty_topic_name=false"
                     (cadr captured)))
            (should-not (plist-get (nth 2 captured) :body))
            (should-error (zulip-api-get-topics account "42/topics" #'ignore))
            (should-error (zulip-api-get-topics account "0" #'ignore))
            (should-error (zulip-api-get-topics account 0 #'ignore)))
        (when (and (processp process) (process-live-p process))
          (delete-process process))))))

(ert-deftest zulip-api-send-message-preserves-local-and-queue-ids ()
  (zulip-runtime-test--isolated
    (let* ((account :account)
           (local-id "local-90071992547409931234")
           (queue-id "queue/opaque:id")
           captured)
      (cl-letf (((symbol-function 'zulip-http-request)
                 (lambda (&rest arguments)
                   (setq captured arguments)
                   :request)))
        (zulip-api-send-message
         account "channel" 42 "topic" "hello" #'ignore
         :local-id local-id :queue-id queue-id))
      (should (equal (list account 'post "/messages")
                     (cl-subseq captured 0 3)))
      (let ((params (nth 3 captured)))
        (should (eq local-id (cdr (assoc "local_id" params))))
        (should (eq queue-id (cdr (assoc "queue_id" params))))
        (should (= 42 (cdr (assoc "to" params))))))))

(ert-deftest zulip-api-reaction-wrappers-use-message-path-and-wire-fields ()
  (zulip-runtime-test--isolated
    (let* ((account :account)
           (message-id "90071992547409931234")
           calls)
      (cl-letf (((symbol-function 'zulip-http-request)
                 (lambda (&rest arguments)
                   (push arguments calls)
                   :request)))
        (zulip-api-add-reaction
         account message-id "octopus" #'ignore
         :emoji-code "1f419" :reaction-type "unicode_emoji")
        (zulip-api-remove-reaction
         account message-id #'ignore
         :emoji-name "octopus" :emoji-code "1f419"
         :reaction-type "unicode_emoji"))
      (setq calls (nreverse calls))
      (let* ((add (car calls))
             (params (nth 3 add)))
        (should (equal
                 (list account 'post
                       "/messages/90071992547409931234/reactions")
                 (cl-subseq add 0 3)))
        (should (equal "octopus" (cdr (assoc "emoji_name" params))))
        (should (equal "1f419" (cdr (assoc "emoji_code" params))))
        (should (equal "unicode_emoji"
                       (cdr (assoc "reaction_type" params)))))
      (let* ((remove (cadr calls))
             (params (nth 3 remove)))
        (should (equal
                 (list account 'delete
                       "/messages/90071992547409931234/reactions")
                 (cl-subseq remove 0 3)))
        (should (equal "octopus" (cdr (assoc "emoji_name" params))))
        (should (equal "1f419" (cdr (assoc "emoji_code" params))))
        (should (equal "unicode_emoji"
                       (cdr (assoc "reaction_type" params))))))))

(provide 'zulip-api-test)

;;; zulip-api-test.el ends here
