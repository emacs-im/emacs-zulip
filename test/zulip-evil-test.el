;;; zulip-evil-test.el --- Tests for native Zulip Evil bindings -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'evil)
(require 'zulip)
(require 'zulip-evil)

(ert-deftest zulip-evil-root-preserves-native-prefixes ()
  (zulip-evil-setup)
  (with-temp-buffer
    (zulip-root-mode)
    (evil-normal-state)
    (appkit-evil-normalize-keymaps)
    (should (eq (key-binding (kbd "g g")) #'evil-goto-first-line))
    (should (eq (key-binding (kbd "g r")) #'zulip-root-refresh))
    (should (eq (key-binding (kbd "g o")) #'zulip-root-open-destination))
    (should (eq (key-binding (kbd "RET")) #'zulip-root-open-at-point))
    (should (eq (key-binding (kbd "q")) #'quit-window))))

(ert-deftest zulip-evil-feed-actions-follow-composer-context ()
  (zulip-evil-setup)
  (with-temp-buffer
    (zulip-feed-mode)
    (evil-normal-state)
    (zulip-feed-timeline-mode 1)
    (appkit-evil-normalize-keymaps)
    (should (eq (key-binding (kbd "g g")) #'evil-goto-first-line))
    (should (eq (key-binding (kbd "e")) #'evil-forward-word-end))
    (should (eq (key-binding (kbd "i"))
                #'appkit-evil-chatbuf-enter-input))
    (should (eq (key-binding (kbd "E")) #'zulip-feed-edit-message))
    (should (eq (key-binding (kbd "D")) #'zulip-feed-delete-message))
    (zulip-feed-timeline-mode -1)
    (appkit-evil-normalize-keymaps)
    (should (eq (key-binding (kbd "i")) #'evil-insert))))

(ert-deftest zulip-evil-motion-state-excludes-destructive-action ()
  (zulip-evil-setup)
  (with-temp-buffer
    (zulip-feed-mode)
    (evil-motion-state)
    (zulip-feed-timeline-mode 1)
    (appkit-evil-normalize-keymaps)
    (should (eq (key-binding (kbd "R")) #'zulip-feed-toggle-reaction))
    (should-not (eq (key-binding (kbd "D")) #'zulip-feed-delete-message))))

(provide 'zulip-evil-test)

;;; zulip-evil-test.el ends here
