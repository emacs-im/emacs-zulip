;;; zulip-runtime.el --- Multi-account Appkit runtime for Zulip -*- lexical-binding: t; -*-

;;; Commentary:

;; A Zulip realm/account pair owns one Appkit application session.  Protocol
;; state remains Zulip-owned; Appkit owns views, requests, timers, and cleanup.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'appkit-core)
(require 'zulip-customize)

(declare-function zulip-events-stop "zulip-events" account)

(cl-defstruct (zulip-account
               (:constructor zulip-account--create))
  "One authenticated Zulip account and its runtime-owned resources."
  id
  server
  email
  api-key
  app
  state
  feature-level
  server-version
  queue-id
  last-event-id
  longpoll-timeout
  poll-process
  retry-timer
  retry-handle
  generation
  connected-p)

(defvar zulip-runtime--accounts (make-hash-table :test #'equal)
  "Live Zulip accounts keyed by normalized account ID.")

(defvar zulip-runtime-change-hook nil
  "Hook run after canonical Zulip runtime state or account membership changes.

Each function receives ACCOUNT and a REASON symbol: `state', `added', or
`removed'.  Presentation features such as the global mode-line indicator use
this hook without becoming part of protocol reducers.")

(defun zulip-runtime-normalize-server (server)
  "Return normalized base URL for Zulip SERVER."
  (unless (and (stringp server) (not (string-empty-p (string-trim server))))
    (error "Zulip server URL must not be empty"))
  (let ((server (replace-regexp-in-string "/+\\'" "" (string-trim server)))
        (case-fold-search t))
    (unless (string-match-p "\\`https?://" server)
      (setq server (concat "https://" server)))
    (when (string-match
           "\\`\\(https?\\)://\\([^/]+\\)\\(.*\\)\\'" server)
      (setq server
            (concat (downcase (match-string 1 server))
                    "://"
                    (downcase (match-string 2 server))
                    (match-string 3 server))))
    server))

(defun zulip-runtime-account-id (server email)
  "Return stable account identity for SERVER and EMAIL."
  (list (zulip-runtime-normalize-server server)
        (downcase (string-trim email))))

(defun zulip-runtime-account (server email)
  "Return the live account for SERVER and EMAIL, or nil."
  (let ((account (gethash (zulip-runtime-account-id server email)
                          zulip-runtime--accounts)))
    (and (zulip-account-p account)
         (appkit-app-live-p (zulip-account-app account))
         account)))

(defun zulip-runtime-accounts ()
  "Return all live Zulip accounts."
  (let (accounts)
    (maphash
     (lambda (_id account)
       (when (and (zulip-account-p account)
                  (appkit-app-live-p (zulip-account-app account)))
         (push account accounts)))
     zulip-runtime--accounts)
    (nreverse accounts)))

(defun zulip-runtime-publish-state (account state)
  "Publish canonical STATE to ACCOUNT and its Appkit application.

All runtime updates to an account's protocol state go through this function so
the client-owned account slot and Appkit's projection source cannot diverge.
Return STATE."
  (unless (zulip-account-p account)
    (error "Not a Zulip account: %S" account))
  (setf (zulip-account-state account) state)
  (when-let* ((app (zulip-account-app account)))
    (when (appkit-app-p app)
      (setf (appkit-app-state app) state)))
  (run-hook-with-args 'zulip-runtime-change-hook account 'state)
  state)

(defun zulip-runtime--shutdown (app)
  "Release the Zulip account transported by APP."
  (let ((account (appkit-app-transport app)))
    (when (zulip-account-p account)
      (when (fboundp 'zulip-events-stop)
        (zulip-events-stop account))
      (setf (zulip-account-connected-p account) nil
            (zulip-account-app account) nil)
      (remhash (zulip-account-id account) zulip-runtime--accounts)
      (run-hook-with-args 'zulip-runtime-change-hook account 'removed))))

(appkit-define-app-kind zulip
  :shutdown #'zulip-runtime--shutdown)

(cl-defun zulip-runtime-create-account (&key server email api-key state)
  "Create or refresh an account for SERVER, EMAIL, API-KEY, and STATE.

There is at most one live account per normalized realm/email pair.  STATE
initializes a new account; a live account keeps its canonical state and views
when credentials are refreshed."
  (unless (and (stringp email) (not (string-empty-p (string-trim email))))
    (error "Zulip email must not be empty"))
  (unless (and (stringp api-key) (not (string-empty-p api-key)))
    (error "Zulip API key must not be empty"))
  (let* ((server (zulip-runtime-normalize-server server))
         (email (string-trim email))
         (id (zulip-runtime-account-id server email))
         (account (gethash id zulip-runtime--accounts)))
    (if (and (zulip-account-p account)
             (appkit-app-live-p (zulip-account-app account)))
        (progn
          (setf (zulip-account-api-key account) api-key)
          account)
      (setq account
            (zulip-account--create
             :id id
             :server server
             :email email
             :api-key api-key
             :state nil
             :longpoll-timeout zulip-event-long-poll-timeout
             :generation 0
             :connected-p nil))
      (let ((app (appkit-start-app
                  'zulip :id id :state nil :transport account)))
        (setf (zulip-account-app account) app)
        (zulip-runtime-publish-state account state))
      (puthash id account zulip-runtime--accounts)
      (run-hook-with-args 'zulip-runtime-change-hook account 'added)
      account)))

(defun zulip-runtime-stop-account (account)
  "Stop ACCOUNT and all Appkit-owned resources."
  (when (zulip-account-p account)
    ;; Invalidate protocol callbacks before Appkit cancels their process/timer
    ;; handles.  `zulip-events-stop' is intentionally idempotent because the
    ;; app-kind shutdown hook invokes it again during teardown.
    (when (fboundp 'zulip-events-stop)
      (zulip-events-stop account))
    (let ((app (zulip-account-app account)))
      (if (appkit-app-p app)
          (appkit-stop-app app)
        (remhash (zulip-account-id account) zulip-runtime--accounts)))
    t))

(defun zulip-runtime-stop-all ()
  "Stop every live Zulip account."
  (dolist (account (zulip-runtime-accounts))
    (zulip-runtime-stop-account account)))

(provide 'zulip-runtime)

;;; zulip-runtime.el ends here
