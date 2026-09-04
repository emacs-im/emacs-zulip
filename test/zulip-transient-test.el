;;; zulip-transient-test.el --- Tests for Zulip transient menus -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'seq)
(require 'zulip-transient)

(ert-deftest zulip-transient-message-actions-accept-server-row ()
  (cl-letf (((symbol-function 'zulip-feed-message-at-point)
             (lambda (&optional _position)
               '((id . "90071992547409931234") (pending . nil)))))
    (should-not (zulip-transient--message-inapt-reason))))

(provide 'zulip-transient-test)

;;; zulip-transient-test.el ends here
