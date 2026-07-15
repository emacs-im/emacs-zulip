;;; zulip-http.el --- Strict asynchronous HTTP results for Zulip -*- lexical-binding: t; -*-

;;; Commentary:

;; This module is the only place where emacs-zulip talks to `plz'.  Every
;; completion, including HTTP and transport failures, is delivered as a
;; `zulip-api-result'.  Callers therefore never need to interpret nil as an
;; unspecified mixture of network, HTTP, JSON, and Zulip API failures.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'url-util)
(require 'plz)
(require 'appkit-core)
(require 'zulip-runtime)

(defconst zulip-http-user-agent "emacs-zulip/0.1.0"
  "User-Agent header sent by emacs-zulip.")

(defconst zulip-http--appkit-handle-property
  'zulip-http--appkit-handle
  "Process property used to find a Zulip request's Appkit handle.")

(cl-defstruct (zulip-api-result
               (:constructor zulip-api-result--create)
               (:copier nil))
  "The complete outcome of one Zulip API request.

STATUS and HEADERS describe an HTTP response when one was received.
RAW-BODY is the unparsed response body and DATA is its parsed JSON value.
CODE and MESSAGE are Zulip's `code' and `msg' fields.  TRANSPORT-ERROR is
the original `plz-error' (or setup condition) when no HTTP response exists.
PARSE-ERROR preserves a JSON parsing condition.  OK-P is non-nil only for a
2xx response with valid JSON that is not a Zulip application error."
  ok-p
  status
  headers
  raw-body
  data
  code
  message
  transport-error
  parse-error)

;; Compatibility names for event code that uses the more explicit spelling.
(defalias 'zulip-api-result-error-code #'zulip-api-result-code)
(defalias 'zulip-api-result-error-message #'zulip-api-result-message)

(defun zulip-http--json-encode (value)
  "Encode Lisp VALUE as JSON for a Zulip request parameter."
  (let ((json-false :json-false)
        (json-null nil))
    (json-encode value)))

(defun zulip-http--parameter-value (value)
  "Return wire string for Zulip parameter VALUE."
  (cond
   ((stringp value) value)
   ((numberp value) (number-to-string value))
   ((eq value t) "true")
   ((eq value :json-false) "false")
   ((null value) "null")
   ((symbolp value) (symbol-name value))
   (t (zulip-http--json-encode value))))

(defun zulip-http-encode-params (params)
  "Return PARAMS alist encoded as an URL query/form string."
  (mapconcat
   (lambda (parameter)
     (concat
      (url-hexify-string (format "%s" (car parameter)))
      "="
      (url-hexify-string
       (zulip-http--parameter-value (cdr parameter)))))
   params
   "&"))

(defun zulip-http-api-url (account endpoint)
  "Return ACCOUNT's API URL for ENDPOINT."
  (let ((server (replace-regexp-in-string
                 "/+\\'" "" (zulip-account-server account)))
        (endpoint (if (string-prefix-p "/" endpoint)
                      endpoint
                    (concat "/" endpoint))))
    (concat server "/api/v1" endpoint)))

(defun zulip-http-basic-auth (account)
  "Return Basic Authorization header value for ACCOUNT."
  (concat "Basic "
          (base64-encode-string
           (concat (zulip-account-email account)
                   ":"
                   (zulip-account-api-key account))
           t)))

(defun zulip-http--method (method)
  "Return METHOD normalized to a lowercase symbol."
  (intern (downcase (if (symbolp method) (symbol-name method) method))))

(defun zulip-http--parse-json (body)
  "Parse JSON BODY and return (DATA PARSE-ERROR)."
  (condition-case err
      (list
       (if (fboundp 'json-parse-string)
           (json-parse-string body
                              :object-type 'hash-table
                              :array-type 'array
                              :null-object nil
                              :false-object :json-false)
         (let ((json-object-type 'hash-table)
               (json-array-type 'vector)
               (json-key-type 'string)
               (json-false :json-false)
               (json-null nil))
           (json-read-from-string body)))
       nil)
    (error (list nil err))))

(defun zulip-http--response-result (response)
  "Convert plz RESPONSE to a strict `zulip-api-result'."
  (let* ((status (plz-response-status response))
         (headers (plz-response-headers response))
         (body (or (plz-response-body response) ""))
         (parsed (zulip-http--parse-json body))
         (data (car parsed))
         (parse-error (cadr parsed))
         (code (and (hash-table-p data) (gethash "code" data)))
         (message (and (hash-table-p data) (gethash "msg" data)))
         (api-result (and (hash-table-p data) (gethash "result" data)))
         (http-ok (and (integerp status) (<= 200 status 299))))
    (zulip-api-result--create
     :ok-p (and http-ok
                (null parse-error)
                (not (equal api-result "error")))
     :status status
     :headers headers
     :raw-body body
     :data data
     :code code
     :message message
     :parse-error parse-error)))

(defun zulip-http--error-result (error-data)
  "Convert plz ERROR-DATA to a strict `zulip-api-result'."
  (let ((response (and (plz-error-p error-data)
                       (plz-error-response error-data))))
    (if response
        (zulip-http--response-result response)
      (zulip-api-result--create :ok-p nil :transport-error error-data))))

(defun zulip-http--setup-error-result (condition)
  "Return a transport result for synchronous setup CONDITION."
  (zulip-api-result--create :ok-p nil :transport-error condition))

(defun zulip-http--cancel-process (process)
  "Cancel PROCESS directly and forget its Appkit handle association."
  (when (processp process)
    (process-put process zulip-http--appkit-handle-property nil)
    (set-process-filter process nil)
    (set-process-sentinel process nil)
    (when (process-live-p process)
      (delete-process process))
    t))

(defun zulip-http-cancel-request (request)
  "Cancel asynchronous REQUEST returned by `zulip-http-request'.

When REQUEST belongs to an Appkit application or view, cancel its lifecycle
handle so the handle is removed from its owner as well as stopping the process.
A raw process without an Appkit handle is cancelled directly; this fallback
also supports transport test doubles that return an unregistered pipe process."
  (when (processp request)
    (let ((handle
           (process-get request zulip-http--appkit-handle-property)))
      (if (and (appkit-handle-p handle)
               (appkit-handle-alive-p handle))
          (appkit-cancel-handle handle)
        (zulip-http--cancel-process request)))))

(cl-defun zulip-http-request
    (account method endpoint params callback &key timeout headers owner)
  "Asynchronously request ACCOUNT ENDPOINT using METHOD and PARAMS.

CALLBACK is called exactly once with a `zulip-api-result', including on
transport, HTTP, API, JSON, and synchronous request-setup failures.  GET and
DELETE parameters are URL-encoded in the query string.  DELETE uses the query
because plz 0.9 does not transmit a DELETE body and Zulip accepts typed
parameters from either request.POST or request.GET.  POST and PATCH parameters
use an application/x-www-form-urlencoded body.

Return the plz process, or nil when request setup itself failed.  OWNER may be
a live Appkit application or view and defaults to ACCOUNT's application.  The
owner retains the process until it completes.  Pass the returned process to
`zulip-http-cancel-request' to cancel it early."
  (unless (functionp callback)
    (error "Zulip HTTP callback must be a function"))
  (let ((owner (or owner (zulip-account-app account)))
        process handle done)
    (cl-labels
        ((finish
          (result)
          (unless done
            (setq done t)
            (when handle
              (ignore-errors (appkit-cancel-handle handle))
              (setq handle nil))
            (funcall callback result))))
      (condition-case err
          (let* ((method (zulip-http--method method))
                 (encoded (and params (zulip-http-encode-params params)))
                 (query-method-p (memq method '(get delete)))
                 (body-method-p (memq method '(post patch)))
                 (url (zulip-http-api-url account endpoint))
                 (url
                  (if (and query-method-p encoded
                           (not (string-empty-p encoded)))
                      (concat url
                              (if (string-match-p "\\?" url) "&" "?")
                              encoded)
                    url))
                 (request-headers
                  (append
                   `(("Authorization" . ,(zulip-http-basic-auth account))
                     ("Accept" . "application/json")
                     ("User-Agent" . ,zulip-http-user-agent))
                   (when body-method-p
                     '(("Content-Type"
                        . "application/x-www-form-urlencoded")))
                   headers)))
            (setq process
                  (plz method url
                    :headers request-headers
                    :body (and body-method-p encoded)
                    :body-type 'text
                    :as 'response
                    :timeout timeout
                    :noquery t
                    :then (lambda (response)
                            (finish (zulip-http--response-result response)))
                    :else (lambda (error-data)
                            (finish (zulip-http--error-result error-data)))))
            ;; A test double may complete synchronously before returning.
            (when (and (not done)
                       (processp process)
                       owner)
              (setq handle
                    (appkit-register-handle
                     owner 'process process
                     (lambda (request)
                       ;; A response may already be queued by plz when the
                       ;; process is cancelled.  Mark it done so that queued
                       ;; transport callbacks remain harmless.
                       (setq done t
                             handle nil)
                       (zulip-http--cancel-process request))))
              (process-put process
                           zulip-http--appkit-handle-property handle)))
        (error
         ;; Do not reinterpret an error raised by the user's callback.
         (if done
             (signal (car err) (cdr err))
           (when (processp process)
             (zulip-http-cancel-request process))
           (finish (zulip-http--setup-error-result err)))))
      process)))

(provide 'zulip-http)

;;; zulip-http.el ends here
