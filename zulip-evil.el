;;; zulip-evil.el --- Native Evil bindings for emacs-zulip -*- lexical-binding: t; -*-

;;; Commentary:

;; Zulip's ordinary mode maps remain its Emacs-state interface.  This optional
;; adapter installs deliberate application actions in Evil state maps.
;; Timeline actions live on the point-sensitive minor-mode map, which Appkit
;; disables in the composer so ordinary Evil editing remains available there.

;;; Code:

(require 'appkit-evil)
(require 'zulip)

(defgroup zulip-evil nil
  "Optional native Evil integration for emacs-zulip."
  :group 'zulip
  :prefix "zulip-evil-")

(defcustom zulip-evil-enable-integration t
  "If non-nil, install emacs-zulip's Evil bindings automatically."
  :type 'boolean
  :group 'zulip-evil)

(defcustom zulip-evil-initial-state 'normal
  "Initial Evil state used for Zulip application buffers.
When nil, leave Evil's initial-state selection untouched."
  :type '(choice (const :tag "Don't override" nil)
          (const :tag "Normal" normal)
          (const :tag "Motion" motion)
          (const :tag "Emacs" emacs)
          (symbol :tag "Custom state"))
  :group 'zulip-evil)

(defconst zulip-evil--application-modes
  '(zulip-root-mode zulip-feed-mode)
  "Major modes participating in emacs-zulip's Evil integration.")

(defun zulip-evil--define-root-keys ()
  "Install navigator modal bindings."
  (appkit-evil-define-readonly-keys 'zulip-root-mode-map)
  (appkit-evil-map
    (:map zulip-root-mode-map
     :nm
     "RET" #'zulip-root-open-at-point
     "<return>" #'zulip-root-open-at-point
     "g r" #'zulip-root-refresh
     "g j" #'appkit-directory-next-item
     "g k" #'appkit-directory-previous-item
     "g o" #'zulip-root-open-destination
     "c" #'zulip-root-open-new-direct-message
     "g s" #'zulip-root-search-messages
     "g t" #'zulip-root-open-topic
     "U" #'zulip-root-next-unread
     "@" #'zulip-root-next-mentioned)))

(defun zulip-evil--define-feed-keys ()
  "Install feed-wide and timeline-only modal bindings."
  (appkit-evil-map
    (:map zulip-feed-mode-map
     :nm
     "RET" #'zulip-feed-return-dwim
     "<return>" #'zulip-feed-return-dwim
     "g j" #'zulip-feed-next-message
     "g k" #'zulip-feed-previous-message


     "g t" #'zulip-feed-open-topic
     "?" #'zulip-message-transient
     :i
     "RET" #'newline
     "<return>" #'newline)
    (:map zulip-feed-message-map
     :nm
     "q" #'quit-window
     "r" #'undefined
     "R" #'undefined
     "c" #'undefined
     "RET" #'zulip-feed-open-message-context
     "<return>" #'zulip-feed-open-message-context
     "g r" #'zulip-feed-open-message-context
     "Z y" #'zulip-feed-copy-message
     "M" #'zulip-feed-mark-read
     "U" #'zulip-feed-mark-unread
     "s" #'zulip-feed-toggle-star
     "!" #'zulip-feed-toggle-reaction
     "i" #'zulip-feed-edit-message
     "Z R" #'zulip-feed-retry-send
     "C-c C-k" #'zulip-feed-cancel-edit
     "?" #'zulip-message-transient
     "D" #'zulip-feed-delete-message
     "d d" #'zulip-feed-delete-message)))

;;;###autoload
(defun zulip-evil-setup ()
  "Install emacs-zulip's native Evil integration.
Safe to call multiple times and before Evil is loaded."
  (interactive)
  (when zulip-evil-enable-integration
    (zulip-evil--define-root-keys)
    (zulip-evil--define-feed-keys)
    (when (featurep 'evil)
      (appkit-evil-set-initial-states
       zulip-evil--application-modes zulip-evil-initial-state)
      (appkit-evil-normalize-buffers zulip-evil--application-modes))))

(zulip-evil-setup)

(with-eval-after-load 'evil
  (zulip-evil-setup))

(with-eval-after-load 'evil-snipe
  (dolist (mode zulip-evil--application-modes)
    (add-hook (intern (format "%s-hook" mode)) #'turn-off-evil-snipe-mode)
    (add-hook (intern (format "%s-hook" mode)) #'turn-off-evil-snipe-override-mode)))

(with-eval-after-load 'zulip-feed
  (when zulip-evil-enable-integration
    (appkit-evil-define-keys '(normal motion) 'zulip-feed-mode-map
      "g A" (lookup-key zulip-feed-mode-map (kbd "M-g")))))

(provide 'zulip-evil)

;;; zulip-evil.el ends here
