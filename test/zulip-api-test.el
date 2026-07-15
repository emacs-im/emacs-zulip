;;; zulip-api-test.el --- Tests for strict Zulip HTTP/API results -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'zulip-api)
(require 'zulip-runtime)

(defun zulip-api-test--account ()
  "Return a representative test account."
  (zulip-account--create
   :id '("https://chat.example.test" "person@example.test")
   :server "https://chat.example.test/"
   :email "person@example.test"
   :api-key "secret-key"
   :longpoll-timeout 73))

(defun zulip-api-test--response (status body &optional headers)
  "Return a plz response with STATUS, BODY, and HEADERS."
  (make-plz-response
   :version 2
   :status status
   :headers (or headers '((content-type . "application/json")))
   :body body))

(ert-deftest zulip-http-basic-auth-uses-account-credentials ()
  (let* ((account (zulip-api-test--account))
         (header (zulip-http-basic-auth account)))
    (should (string-prefix-p "Basic " header))
    (should (equal "person@example.test:secret-key"
                   (base64-decode-string (substring header 6))))))

(ert-deftest zulip-http-get-encodes-query-and-delivers-success-result ()
  (let ((account (zulip-api-test--account))
        captured
        result)
    (cl-letf (((symbol-function 'plz)
               (lambda (method url &rest arguments)
                 (setq captured (list method url arguments))
                 (funcall
                  (plist-get arguments :then)
                  (zulip-api-test--response
                   200 "{\"result\":\"success\",\"value\":42}"))
                 :mock-process)))
      (zulip-http-request
       account 'get "/example"
       '(("queue_id" . "queue/string") ("last_event_id" . "9007199254740993"))
       (lambda (value) (setq result value))))
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

(ert-deftest zulip-http-post-encodes-form-body ()
  (let ((account (zulip-api-test--account))
        captured)
    (cl-letf (((symbol-function 'plz)
               (lambda (method url &rest arguments)
                 (setq captured (list method url arguments))
                 :mock-process)))
      (zulip-http-request
       account 'post "/messages"
       '(("type" . "direct") ("to" . [42 9007199254740993]))
       #'ignore))
    (should (eq 'post (car captured)))
    (should-not (string-match-p "\\?" (cadr captured)))
    (let ((body (plist-get (nth 2 captured) :body))
          (headers (plist-get (nth 2 captured) :headers)))
      (should (string-match-p "type=direct" body))
      (should (string-match-p "to=%5B42%2C9007199254740993%5D" body))
      (should (equal "application/x-www-form-urlencoded"
                     (cdr (assoc "Content-Type" headers)))))))

(ert-deftest zulip-api-wire-message-id-array-is-exact-and-keeps-strings-quoted ()
  (let ((huge-id "900719925474099312345678901234567890"))
    (should (equal (concat "[" huge-id "]")
                   (zulip-api--wire-message-id-array (vector huge-id))))
    (should (equal (concat "[\"" huge-id "\"]")
                   (zulip-http--json-encode (vector huge-id))))))

(ert-deftest zulip-api-wire-message-id-array-rejects-invalid-or-injected-text ()
  (dolist (invalid '("" "01" "-1" "+1" "1.0" "1e3"
                     "1,2" "1]" "1 null" "1\n2" 42))
    (let ((condition
           (should-error
            (zulip-api--wire-message-id-array (vector invalid)))))
      (should (eq 'error (car condition)))
      (should (string-prefix-p "Invalid Zulip message ID for JSON"
                               (cadr condition))))))

(ert-deftest zulip-api-message-flags-emit-exact-json-integer-ids ()
  (let ((account (zulip-api-test--account))
        (huge-id "900719925474099312345678901234567890")
        captured)
    (cl-letf (((symbol-function 'plz)
               (lambda (method url &rest arguments)
                 (setq captured (list method url arguments))
                 :mock-process)))
      (zulip-api-update-message-flags
       account (vector huge-id "42") 'add "read" #'ignore))
    (should (eq 'post (car captured)))
    (should (string-suffix-p "/api/v1/messages/flags" (cadr captured)))
    (should
     (equal
      (concat "messages=%5B" huge-id "%2C42%5D&op=add&flag=read")
      (plist-get (nth 2 captured) :body)))))

(ert-deftest zulip-http-patch-encodes-form-body ()
  (let ((account (zulip-api-test--account))
        captured)
    (cl-letf (((symbol-function 'plz)
               (lambda (method url &rest arguments)
                 (setq captured (list method url arguments))
                 :mock-process)))
      (zulip-http-request
       account 'patch "/messages/90071992547409931234"
       '(("content" . "edited markdown")
         ("send_notification_to_old_thread" . :json-false))
       #'ignore))
    (should (eq 'patch (car captured)))
    (should (string-suffix-p
             "/api/v1/messages/90071992547409931234" (cadr captured)))
    (should-not (string-match-p "\\?" (cadr captured)))
    (let ((body (plist-get (nth 2 captured) :body))
          (headers (plist-get (nth 2 captured) :headers)))
      (should (string-match-p "content=edited%20markdown" body))
      (should (string-match-p
               "send_notification_to_old_thread=false" body))
      (should (equal "application/x-www-form-urlencoded"
                     (cdr (assoc "Content-Type" headers)))))))

(ert-deftest zulip-http-delete-encodes-queue-id-in-query ()
  (let ((account (zulip-api-test--account))
        captured)
    (cl-letf (((symbol-function 'plz)
               (lambda (method url &rest arguments)
                 (setq captured (list method url arguments))
                 :mock-process)))
      (zulip-api-delete-queue account "queue/id:opaque" #'ignore))
    (should (eq 'delete (car captured)))
    (should (string-match-p
             "/api/v1/events\\?queue_id=queue%2Fid%3Aopaque\\'"
             (cadr captured)))
    (should-not (plist-get (nth 2 captured) :body))))

(ert-deftest zulip-http-http-error-preserves-response-and-api-error ()
  (let ((account (zulip-api-test--account))
        result
        (body "{\"result\":\"error\",\"code\":\"BAD_EVENT_QUEUE_ID\",\"msg\":\"expired\"}"))
    (cl-letf (((symbol-function 'plz)
               (lambda (_method _url &rest arguments)
                 (funcall
                  (plist-get arguments :else)
                  (make-plz-error
                   :response
                   (zulip-api-test--response
                    400 body '((content-type . "application/json")
                               (request-id . "request-1")))))
                 :mock-process)))
      (zulip-http-request account 'get "/events" nil
                          (lambda (value) (setq result value))))
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

(ert-deftest zulip-http-transport-error-is-a-result-not-nil ()
  (let ((account (zulip-api-test--account))
        result
        (transport (make-plz-error
                    :curl-error '(6 . "Could not resolve host."))))
    (cl-letf (((symbol-function 'plz)
               (lambda (_method _url &rest arguments)
                 (funcall (plist-get arguments :else) transport)
                 :mock-process)))
      (zulip-http-request account 'get "/users/me" nil
                          (lambda (value) (setq result value))))
    (should (zulip-api-result-p result))
    (should-not (zulip-api-result-ok-p result))
    (should (eq transport (zulip-api-result-transport-error result)))
    (should-not (zulip-api-result-status result))
    (should-not (zulip-api-result-data result))))

(ert-deftest zulip-http-json-error-preserves-status-and-raw-body ()
  (let ((account (zulip-api-test--account))
        result)
    (cl-letf (((symbol-function 'plz)
               (lambda (_method _url &rest arguments)
                 (funcall (plist-get arguments :then)
                          (zulip-api-test--response 200 "not json"))
                 :mock-process)))
      (zulip-http-request account 'get "/users/me" nil
                          (lambda (value) (setq result value))))
    (should (zulip-api-result-p result))
    (should-not (zulip-api-result-ok-p result))
    (should (= 200 (zulip-api-result-status result)))
    (should (equal "not json" (zulip-api-result-raw-body result)))
    (should (zulip-api-result-parse-error result))))

(ert-deftest zulip-http-api-error-in-2xx-is-not-ok ()
  (let ((result
         (zulip-http--response-result
          (zulip-api-test--response
           200 "{\"result\":\"error\",\"code\":\"BAD_REQUEST\",\"msg\":\"bad\"}"))))
    (should-not (zulip-api-result-ok-p result))
    (should (equal "BAD_REQUEST" (zulip-api-result-code result)))
    (should (equal "bad" (zulip-api-result-message result)))))

(ert-deftest zulip-http-synchronous-setup-error-goes-to-callback ()
  (let ((account (zulip-api-test--account))
        result)
    (cl-letf (((symbol-function 'plz)
               (lambda (&rest _arguments)
                 (error "curl executable missing"))))
      (should-not
       (zulip-http-request account 'get "/users/me" nil
                           (lambda (value) (setq result value)))))
    (should (zulip-api-result-p result))
    (should-not (zulip-api-result-ok-p result))
    (should (zulip-api-result-transport-error result))))

(ert-deftest zulip-http-preflight-error-goes-to-callback-once ()
  (let ((account (zulip-api-test--account))
        (calls 0)
        result)
    (cl-letf (((symbol-function 'plz)
               (lambda (&rest _arguments)
                 (ert-fail "plz must not run after a preflight error"))))
      (should-not
       (zulip-http-request
        account 42 "/users/me" nil
        (lambda (value)
          (cl-incf calls)
          (setq result value)))))
    (should (= calls 1))
    (should (zulip-api-result-p result))
    (should-not (zulip-api-result-ok-p result))
    (should (zulip-api-result-transport-error result))))

(ert-deftest zulip-http-completion-forgets-appkit-handle-and-runs-once ()
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
            (should (eq process request)))
          (setq handle (car (appkit-app-handles app)))
          (should (appkit-handle-alive-p handle))
          (should (eq handle
                      (process-get
                       process zulip-http--appkit-handle-property)))
          (funcall then
                   (zulip-api-test--response
                    200 "{\"result\":\"success\"}"))
          ;; A defensive duplicate transport completion must be ignored.
          (funcall then
                   (zulip-api-test--response
                    200 "{\"result\":\"success\"}"))
          (should (= calls 1))
          (should-not (process-live-p process))
          (should-not (appkit-handle-alive-p handle))
          (should-not (appkit-app-handles app)))
      (when (and (processp process) (process-live-p process))
        (delete-process process))
      (zulip-runtime-stop-account account))))

(ert-deftest zulip-http-explicit-view-owner-cancels-with-the-view ()
  (let* ((account
          (zulip-runtime-create-account
           :server "https://http-view-owner-test.invalid"
           :email "http-view-owner@example.invalid"
           :api-key "secret"))
         (app (zulip-account-app account))
         (buffer-name " *zulip-http-view-owner-test*")
         view process then handle
         (calls 0))
    (unwind-protect
        (cl-letf (((symbol-function 'plz)
                   (lambda (_method _url &rest arguments)
                     (setq then (plist-get arguments :then)
                           process
                           (make-pipe-process
                            :name "zulip-http-view-owner-test-pipe"
                            :noquery t))
                     process)))
          (setq view
                (appkit-open-view
                 :app app :id 'http-view-owner
                 :mode 'fundamental-mode :buffer-name buffer-name))
          (let ((request
                 (zulip-http-request
                  account 'get "/messages" nil
                  (lambda (_result) (cl-incf calls))
                  :owner view)))
            (should (eq process request)))
          (setq handle (car (appkit-view-handles view)))
          (should (appkit-handle-alive-p handle))
          (should (eq view (appkit-handle-owner handle)))
          (should-not (appkit-app-handles app))
          (appkit-kill-view view)
          (should-not (process-live-p process))
          (should-not (appkit-handle-alive-p handle))
          (should-not (appkit-view-handles view))
          ;; A completion queued before cancellation remains harmless.
          (funcall then
                   (zulip-api-test--response
                    200 "{\"result\":\"success\"}"))
          (should (= calls 0)))
      (when (and (processp process) (process-live-p process))
        (delete-process process))
      (when-let* ((buffer (get-buffer buffer-name)))
        (kill-buffer buffer))
      (zulip-runtime-stop-account account))))

(ert-deftest zulip-api-register-wrapper-uses-post-and-default-event-types ()
  (let ((account :account)
        captured)
    (cl-letf (((symbol-function 'zulip-http-request)
               (lambda (&rest arguments)
                 (setq captured arguments)
                 :request)))
      (should (eq :request (zulip-api-register account #'ignore))))
    (should (equal (list account 'post "/register")
                   (cl-subseq captured 0 3)))
    (let ((params (nth 3 captured)))
      (should (equal zulip-api-default-event-types
                     (cdr (assoc "event_types" params))))
      (should (equal zulip-api-default-fetch-event-types
                     (cdr (assoc "fetch_event_types" params))))
      (should (equal zulip-api-default-client-capabilities
                     (cdr (assoc "client_capabilities" params))))
      (dolist (event-type (append zulip-api-default-event-types nil))
        (should (member event-type
                        (append zulip-api-default-fetch-event-types nil))))
      (should (member "recent_private_conversations"
                      (append zulip-api-default-fetch-event-types nil)))
      (should (member "realm"
                      (append zulip-api-default-fetch-event-types nil)))
      (should-not (member "realm"
                          (append zulip-api-default-event-types nil)))
      (should (member "user_topic"
                      (append zulip-api-default-event-types nil)))
      (should (member "message"
                      (append zulip-api-default-event-types nil)))
      (should (eq t (cdr (assoc "apply_markdown" params)))))))

(ert-deftest zulip-api-register-custom-events-remain-in-fetch-set ()
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
                     (cdr (assoc "fetch_event_types" params)))))))

(ert-deftest zulip-api-events-wrapper-preserves-opaque-identifiers ()
  (let ((account (zulip-api-test--account))
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
    (should (= 73 (plist-get (nthcdr 5 captured) :timeout)))))

(ert-deftest zulip-api-get-messages-preserves-narrow-and-anchor ()
  (let* ((account :account)
         (narrow [((operator . "channel") (operand . "general"))])
         (anchor "90071992547409931234")
         (owner :history-view)
         captured)
    (cl-letf (((symbol-function 'zulip-http-request)
               (lambda (&rest arguments)
                 (setq captured arguments)
                 :request)))
      (zulip-api-get-messages
       account narrow anchor 50 0 #'ignore :owner owner))
    (should (equal (list account 'get "/messages")
                   (cl-subseq captured 0 3)))
    (let ((params (nth 3 captured)))
      (should (eq narrow (cdr (assoc "narrow" params))))
      (should (eq anchor (cdr (assoc "anchor" params))))
      (should (= 50 (cdr (assoc "num_before" params)))))
    (should (eq owner (plist-get (nthcdr 5 captured) :owner)))))

(ert-deftest zulip-api-get-message-defaults-to-display-ready-wire-query ()
  (let ((account (zulip-api-test--account))
        captured)
    (cl-letf (((symbol-function 'plz)
               (lambda (method url &rest arguments)
                 (setq captured (list method url arguments))
                 :mock-process)))
      (zulip-api-get-message
       account "90071992547409931234" #'ignore))
    (should (eq 'get (car captured)))
    (should
     (equal
      (concat "https://chat.example.test/api/v1/messages/"
              "90071992547409931234"
              "?apply_markdown=true&allow_empty_topic_name=true")
      (cadr captured)))
    (should-not (plist-get (nth 2 captured) :body))))

(ert-deftest zulip-api-get-message-preserves-opaque-id-false-values-and-owner ()
  (let ((account :account)
        (message-id "90071992547409931234")
        (owner :message-view)
        captured)
    (cl-letf (((symbol-function 'zulip-http-request)
               (lambda (&rest arguments)
                 (setq captured arguments)
                 :request)))
      (should
       (eq :request
           (zulip-api-get-message
            account message-id #'ignore
            :apply-markdown nil
            :allow-empty-topic-name nil
            :owner owner))))
    (should
     (equal
      (list account 'get
            "/messages/90071992547409931234")
      (cl-subseq captured 0 3)))
    (let ((params (nth 3 captured)))
      (should (eq :json-false
                  (cdr (assoc "apply_markdown" params))))
      (should (eq :json-false
                  (cdr (assoc "allow_empty_topic_name" params)))))
    (should (eq owner (plist-get (nthcdr 5 captured) :owner)))
    (should-error
     (zulip-api-get-message account 90071992547409931234 #'ignore))))

(ert-deftest zulip-api-get-topics-uses-stream-path-empty-topic-capability-and-owner ()
  (let ((account :account)
        (owner :root-view)
        captured)
    (cl-letf (((symbol-function 'zulip-http-request)
               (lambda (&rest arguments)
                 (setq captured arguments)
                 :request)))
      (should (eq :request
                  (zulip-api-get-topics
                   account "42" #'ignore :owner owner))))
    (should (equal (list account 'get "/users/me/42/topics")
                   (cl-subseq captured 0 3)))
    (should (eq t (cdr (assoc "allow_empty_topic_name"
                              (nth 3 captured)))))
    (should (eq owner (plist-get (nthcdr 5 captured) :owner)))))

(ert-deftest zulip-api-get-topics-encodes-explicit-false-and-validates-stream-id ()
  (let (captured)
    (cl-letf (((symbol-function 'zulip-http-request)
               (lambda (&rest arguments)
                 (setq captured arguments)
                 :request)))
      (zulip-api-get-topics
       :account 42 #'ignore :allow-empty-topic-name nil))
    (should (equal "/users/me/42/topics" (nth 2 captured)))
    (should (eq :json-false
                (cdr (assoc "allow_empty_topic_name" (nth 3 captured)))))
    (should-error (zulip-api-get-topics :account "42/topics" #'ignore))
    (should-error (zulip-api-get-topics :account "0" #'ignore))
    (should-error (zulip-api-get-topics :account 0 #'ignore))))

(ert-deftest zulip-api-send-message-preserves-local-and-queue-ids ()
  (let ((account :account)
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
      (should (= 42 (cdr (assoc "to" params)))))))

(ert-deftest zulip-api-message-flag-wrappers-preserve-ids-and-openapi-params ()
  (let* ((account :account)
         (message-id "90071992547409931234")
         (message-ids (vector message-id "90071992547409939999"))
         (anchor "90071992547409935555")
         (narrow [((operator . "is") (operand . "unread"))])
         (owner :view)
         calls)
    (cl-letf (((symbol-function 'zulip-http-request)
               (lambda (&rest arguments)
                 (push arguments calls)
                 :request)))
      (zulip-api-update-message-flags
       account message-ids 'add "read" #'ignore :owner owner)
      (zulip-api-update-message-flags-for-narrow
       account narrow anchor 50 0 'remove "starred" #'ignore
       :include-anchor nil :owner owner))
    (setq calls (nreverse calls))
    (let* ((specific (car calls))
           (params (nth 3 specific))
           (wire-ids (cdr (assoc "messages" params))))
      (should (equal (list account 'post "/messages/flags")
                     (cl-subseq specific 0 3)))
      (should (equal (concat "[" message-id ",90071992547409939999]")
                     wire-ids))
      (should (eq 'add (cdr (assoc "op" params))))
      (should (equal "read" (cdr (assoc "flag" params))))
      (should (eq owner (plist-get (nthcdr 5 specific) :owner))))
    (let* ((for-narrow (cadr calls))
           (params (nth 3 for-narrow)))
      (should (equal (list account 'post "/messages/flags/narrow")
                     (cl-subseq for-narrow 0 3)))
      (should (eq anchor (cdr (assoc "anchor" params))))
      (should (eq narrow (cdr (assoc "narrow" params))))
      (should (= 50 (cdr (assoc "num_before" params))))
      (should (= 0 (cdr (assoc "num_after" params))))
      (should (eq :json-false
                  (cdr (assoc "include_anchor" params))))
      (should (eq 'remove (cdr (assoc "op" params))))
      (should (equal "starred" (cdr (assoc "flag" params))))
      (should (eq owner (plist-get (nthcdr 5 for-narrow) :owner))))))

(ert-deftest zulip-api-update-and-delete-message-preserve-opaque-path-id ()
  (let ((account :account)
        (message-id "90071992547409931234")
        (owner :view)
        calls)
    (cl-letf (((symbol-function 'zulip-http-request)
               (lambda (&rest arguments)
                 (push arguments calls)
                 :request)))
      (zulip-api-update-message
       account message-id #'ignore
       :topic "" :propagate-mode "change_all"
       :send-notification-to-old-thread nil
       :send-notification-to-new-thread t
       :content "edited" :prev-content-sha256 "sha256"
       :stream-id 42 :owner owner)
      (zulip-api-delete-message account message-id #'ignore :owner owner))
    (setq calls (nreverse calls))
    (let* ((update (car calls))
           (params (nth 3 update)))
      (should (equal
               (list account 'patch
                     "/messages/90071992547409931234")
               (cl-subseq update 0 3)))
      (should (equal "" (cdr (assoc "topic" params))))
      (should (equal "change_all"
                     (cdr (assoc "propagate_mode" params))))
      (should (eq :json-false
                  (cdr (assoc "send_notification_to_old_thread" params))))
      (should (eq t
                  (cdr (assoc "send_notification_to_new_thread" params))))
      (should (equal "edited" (cdr (assoc "content" params))))
      (should (equal "sha256"
                     (cdr (assoc "prev_content_sha256" params))))
      (should (= 42 (cdr (assoc "stream_id" params))))
      (should (eq owner (plist-get (nthcdr 5 update) :owner))))
    (let ((delete (cadr calls)))
      (should (equal
               (list account 'delete
                     "/messages/90071992547409931234" nil)
               (cl-subseq delete 0 4)))
      (should (eq owner (plist-get (nthcdr 5 delete) :owner))))
    (should-error (zulip-api-delete-message account 42 #'ignore))))

(ert-deftest zulip-api-reaction-wrappers-use-message-path-and-wire-fields ()
  (let ((account :account)
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
                     (cdr (assoc "reaction_type" params)))))))

(provide 'zulip-api-test)

;;; zulip-api-test.el ends here
