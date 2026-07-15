;;; zulip-transient-test.el --- Tests for Zulip transient menus -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'seq)
(require 'zulip-transient)

(ert-deftest zulip-transient-message-prefix-is-a-command ()
  (should (commandp #'zulip-message-transient)))

(ert-deftest zulip-transient-message-prefix-exposes-feed-actions ()
  (let* ((expected
          '(("o" . zulip-feed-open-message-context)
            ("t" . zulip-feed-open-topic)
            ("c" . zulip-feed-copy-message)
            ("r" . zulip-feed-mark-read)
            ("u" . zulip-feed-mark-unread)
            ("s" . zulip-feed-toggle-star)
            ("R" . zulip-feed-retry-send)
            ("e" . zulip-feed-edit-message)
            ("d" . zulip-feed-delete-message)
            ("+" . zulip-feed-toggle-reaction)))
         (server-commands
          (delq 'zulip-feed-retry-send (mapcar #'cdr expected)))
         (objects (transient-suffixes 'zulip-message-transient))
         (suffixes
          (mapcar (lambda (suffix)
                    (cons (oref suffix key) (oref suffix command)))
                  objects)))
    (dolist (entry expected)
      (should (eq (cdr (assoc (car entry) suffixes)) (cdr entry))))
    ;; Server actions share one authoritative-message gate.  Retry is the
    ;; deliberate inverse: it is available only on failed local rows.
    (dolist (suffix
             (seq-filter
              (lambda (object)
                (memq (oref object command) server-commands))
              objects))
      (should (eq (oref suffix inapt-if)
                  'zulip-transient--message-inapt-reason)))
    (let ((retry (seq-find
                  (lambda (object)
                    (eq (oref object command) 'zulip-feed-retry-send))
                  objects)))
      (should retry)
      (should (eq (oref retry inapt-if)
                  'zulip-transient--retry-inapt-reason)))))

(ert-deftest zulip-transient-message-inapt-reason-explains-no-message ()
  (cl-letf (((symbol-function 'zulip-feed-message-at-point)
             (lambda (&optional _position) nil)))
    (should (equal (zulip-transient--message-inapt-reason)
                   "No Zulip message at point"))))

(ert-deftest zulip-transient-message-inapt-reason-rejects-local-row ()
  (dolist (message
           '(((id . "local-42") (pending . t))
             ((id . "local-failed") (pending . nil))))
    (cl-letf (((symbol-function 'zulip-feed-message-at-point)
               (lambda (&optional _position) message)))
      (should (equal (zulip-transient--message-inapt-reason)
                     "Message is still local; wait for server acknowledgement")))))

(ert-deftest zulip-transient-message-actions-accept-server-row ()
  (cl-letf (((symbol-function 'zulip-feed-message-at-point)
             (lambda (&optional _position)
               '((id . "90071992547409931234") (pending . nil)))))
    (should-not (zulip-transient--message-inapt-reason))))

(ert-deftest zulip-transient-retry-accepts-only-failed-local-row ()
  (dolist (case '((((id . "local-f") (failed . "denied")) . nil)
                  (((id . "local-p") (pending . t))
                   . "Local message has not failed")
                  (((id . "20")) . "Server messages do not need send retry")))
    (cl-letf (((symbol-function 'zulip-feed-message-at-point)
               (lambda (&optional _position) (car case))))
      (should (equal (zulip-transient--retry-inapt-reason) (cdr case))))))

(provide 'zulip-transient-test)

;;; zulip-transient-test.el ends here
