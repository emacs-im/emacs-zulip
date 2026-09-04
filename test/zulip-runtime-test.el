;;; zulip-runtime-test.el --- Runtime tests for emacs-zulip -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'seq)
(require 'zulip-runtime)
(require 'zulip)

(defvar zulip-runtime-test--buffers nil
  "Buffers explicitly acquired by the current isolated fixture.")

(defmacro zulip-runtime-test--isolated (&rest body)
  "Run BODY with an isolated account registry and runtime hooks."
  (declare (indent 0) (debug t))
  `(let ((zulip-runtime--accounts (make-hash-table :test #'equal))
         (zulip-runtime-change-hook nil)
         zulip-runtime-test--buffers)
     (unwind-protect
         (cl-letf (((symbol-function 'plz)
                    (lambda (&rest _arguments)
                      (ert-fail "Unexpected network request in isolated fixture")))
                   ((symbol-function 'auth-source-search)
                    (lambda (&rest _arguments)
                      (ert-fail "Unexpected user credential lookup in isolated fixture"))))
           ,@body)
       (zulip-runtime-stop-all)
       (dolist (buffer zulip-runtime-test--buffers)
         (when (buffer-live-p buffer) (kill-buffer buffer))))))

(defun zulip-runtime-test--drain (account)
  "Deliver queued work only for ACCOUNT and its attached Surfaces."
  (let ((app (zulip-account-app account))
        (remaining 1000)
        pending)
    (while
        (progn
          (setq pending nil)
          (when (appkit-app-live-p app)
            (let ((loops (list (appkit-app-loop app))))
              (maphash
               (lambda (id _entry)
                 (let ((surface (appkit-app-surface app id)))
                   (when (appkit-surface-live-p surface)
                     (push (appkit-surface-loop surface) loops))))
               (appkit-app-surfaces app))
              (dolist (loop loops)
                (when (> (appkit-loop-pending-count loop) 0)
                  (setq pending t)
                  (appkit-loop-run-pass loop)))))
          pending)
      (when (zerop (cl-decf remaining))
        (error "Zulip fixture delivery did not settle")))))

(ert-deftest zulip-runtime-normalizes-account-identity ()
  (should (equal (zulip-runtime-account-id "chat.example.com/" " ME@Example.COM ")
                 '("https://chat.example.com" "me@example.com")))
  (should (equal (zulip-runtime-normalize-server
                  "HTTPS://CHAT.Example.com/")
                 "https://chat.example.com")))

(ert-deftest zulip-runtime-reuses-one-live-app-per-account ()
  (zulip-runtime-test--isolated
    (let* ((first (zulip-runtime-create-account
                   :server "https://chat.example.com/"
                   :email "me@example.com" :api-key "one" :state 'old))
           (second (zulip-runtime-create-account
                    :server "chat.example.com"
                    :email "ME@example.com" :api-key "two" :state 'new)))
      (should (eq first second))
      (should (equal (zulip-account-api-key second) "two"))
      (should (eq (zulip-account-state second) 'old))
      (should (eq (appkit-app-model (zulip-account-app second)) second)))))

(ert-deftest zulip-runtime-owns-replaces-and-erases-api-key-copies ()
  (zulip-runtime-test--isolated
    (let* ((first-source (copy-sequence "first-secret"))
           (account
            (zulip-runtime-create-account
             :server "https://chat.example.com"
             :email "me@example.com"
             :api-key first-source))
           (first-owned (zulip-account-api-key account))
           (second-source (copy-sequence "second-secret")))
      (should (equal first-owned first-source))
      (should-not (eq first-owned first-source))
      (zulip-runtime-create-account
       :server "https://chat.example.com"
       :email "me@example.com"
       :api-key second-source)
      (let ((second-owned (zulip-account-api-key account)))
        (should (seq-every-p #'zerop (string-to-list first-owned)))
        (should (equal first-source "first-secret"))
        (should (equal second-source "second-secret"))
        (should (equal second-owned second-source))
        (should-not (eq second-owned second-source))
        (zulip-runtime-stop-account account)
        (should (seq-every-p #'zerop (string-to-list second-owned)))
        (should-not (zulip-account-api-key account))))))

(ert-deftest zulip-runtime-startup-failure-erases-owned-api-key ()
  (zulip-runtime-test--isolated
    (let (owned-key)
      (cl-letf (((symbol-function 'appkit-app-start)
                 (lambda (_kind &rest options)
                   (setq owned-key
                         (zulip-account-api-key
                          (plist-get options :input)))
                   (error "startup failed"))))
        (should-error
         (zulip-runtime-create-account
          :server "https://chat.example.com"
          :email "me@example.com"
          :api-key "startup-secret")))
      (should (seq-every-p #'zerop (string-to-list owned-key)))
      (should-not (zulip-runtime-accounts)))))

(ert-deftest zulip-runtime-publish-state-keeps-account-and-app-canonical ()
  (zulip-runtime-test--isolated
    (let* ((account (zulip-runtime-create-account
                     :server "publish.example" :email "me@example.com"
                     :api-key "secret" :state 'old))
           (app (zulip-account-app account))
           (next (list :state 'next)))
      (should (eq next (zulip-runtime-publish-state account next)))
      (should (eq next (zulip-account-state account)))
      (should (eq account (appkit-app-model app)))
      (should-error (zulip-runtime-publish-state :not-an-account next)))))

(ert-deftest zulip-runtime-isolates-realms-and-removes-stopped-account ()
  (zulip-runtime-test--isolated
    (let ((one (zulip-runtime-create-account
                :server "one.example" :email "me@example.com"
                :api-key "one"))
          (two (zulip-runtime-create-account
                :server "two.example" :email "me@example.com"
                :api-key "two")))
      (should-not (eq one two))
      (should (= (length (zulip-runtime-accounts)) 2))
      (zulip-runtime-stop-account one)
      (should-not (zulip-runtime-account "one.example" "me@example.com"))
      (should (= (length (zulip-runtime-accounts)) 1)))))

(ert-deftest zulip-connect-restart-preserves-live-account-state ()
  (zulip-runtime-test--isolated
    (let* ((state (zulip-state-upsert-message
                   (zulip-state-create)
                   '((id . "42") (type . "stream")
                     (stream_id . 7) (subject . "client")
                     (content . "cached"))))
           (account (zulip-runtime-create-account
                     :server "chat.example.com"
                     :email "me@example.com"
                     :api-key "old"
                     :state state))
           restarted)
      (cl-letf (((symbol-function 'zulip-events-start)
                 (lambda (value) value)))
        (setq restarted
              (zulip-connect "https://chat.example.com/"
                             "ME@example.com" "new")))
      (should (eq account restarted))
      (should (eq state (zulip-account-state restarted)))
      (should (eq restarted (appkit-app-model (zulip-account-app restarted))))
      (should (equal (zulip-account-api-key restarted) "new"))
      (should (zulip-state-message (zulip-account-state restarted) "42"))
      (should-error
       (zulip-connect "chat.example.com" "me@example.com" ""))
      (should (eq state (zulip-account-state restarted))))))

(provide 'zulip-runtime-test)

;;; zulip-runtime-test.el ends here
