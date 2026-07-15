;;; zulip-modes-test.el --- Tests for global Zulip presentation modes -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'zulip-modes)

(defun zulip-modes-test--state (unread mentions)
  "Return a Zulip state with UNREAD and MENTIONS aggregate counts."
  (let ((state (zulip-state-create)))
    (setf (zulip-state-unread-count state) unread)
    (dotimes (index mentions)
      (puthash (format "m%d" index) t (zulip-state-unread-mentions state)))
    state))

(ert-deftest zulip-mode-line-aggregates-accounts-and-tracks-runtime-hook ()
  (let ((zulip-runtime--accounts (make-hash-table :test #'equal))
        (zulip-runtime-change-hook nil)
        (mode-line-misc-info nil)
        (zulip-mode-line-mode nil)
        (zulip-mode-line-string "")
        (zulip-mode-line--cached-counts '(0 . 0)))
    (let ((first
           (zulip-runtime-create-account
            :server "https://one.example.test"
            :email "one@example.test"
            :api-key "secret-one"
            :state (zulip-modes-test--state 3 1)))
          (second
           (zulip-runtime-create-account
            :server "https://two.example.test"
            :email "two@example.test"
            :api-key "secret-two"
            :state (zulip-modes-test--state 4 2))))
      (unwind-protect
          (progn
            (zulip-mode-line-mode 1)
            (should (member 'zulip-mode-line-format mode-line-misc-info))
            (should (equal zulip-mode-line--cached-counts '(7 . 3)))
            (should (string-match-p "7" (zulip-mode-line-unread)))
            (should (string-match-p "@3" (zulip-mode-line-mentions)))
            (zulip-runtime-publish-state
             first (zulip-modes-test--state 1 0))
            (should (equal zulip-mode-line--cached-counts '(5 . 2)))
            (zulip-runtime-stop-account second)
            (should (equal zulip-mode-line--cached-counts '(1 . 0))))
        (zulip-mode-line-mode -1)
        (zulip-runtime-stop-all))
      (should-not (member 'zulip-mode-line-format mode-line-misc-info))
      (should-not (memq #'zulip-mode-line-update
                        zulip-runtime-change-hook)))))

(ert-deftest zulip-mode-line-runtime-hook-does-not-force-redisplay ()
  (let ((zulip-mode-line-mode t)
        (zulip-mode-line-string "")
        (zulip-mode-line--cached-counts '(0 . 0))
        (redisplays 0))
    (cl-letf (((symbol-function 'zulip-mode-line--counts)
               (lambda () '(3 . 1)))
              ((symbol-function 'force-mode-line-update)
               (lambda (&rest _) (cl-incf redisplays))))
      (zulip-mode-line-update)
      (should (equal zulip-mode-line--cached-counts '(3 . 1)))
      (should (zerop redisplays)))))

(provide 'zulip-modes-test)

;;; zulip-modes-test.el ends here
