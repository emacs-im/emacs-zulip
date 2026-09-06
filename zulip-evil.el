;;; zulip-evil.el --- Native Evil bindings for emacs-zulip -*- lexical-binding: t; -*-

;;; Commentary:

;; Zulip's ordinary mode maps remain its Emacs-state interface.  This optional
;; adapter installs deliberate application actions in Evil state maps without
;; shadowing native motions, operators, or prefixes.  Timeline actions live on
;; the point-sensitive minor-mode map, which Appkit disables in the composer.

;;; Code:

(require 'appkit-evil)
(require 'zulip-customize)

(declare-function zulip-feed-cancel-edit "zulip-feed" ())
(declare-function zulip-feed-copy-message "zulip-feed" (&optional message))
(declare-function zulip-feed-delete-message "zulip-feed" (&optional message))
(declare-function zulip-feed-edit-message "zulip-feed" (&optional message))
(declare-function zulip-feed-load-latest "zulip-feed" ())
(declare-function zulip-feed-load-newer "zulip-feed" ())
(declare-function zulip-feed-load-older "zulip-feed" ())
(declare-function zulip-feed-mark-read "zulip-feed" (&optional position))
(declare-function zulip-feed-mark-unread "zulip-feed" (&optional message))
(declare-function zulip-feed-open-message-context "zulip-feed" (&optional message))
(declare-function zulip-feed-open-topic "zulip-feed" (&optional channel topic))
(declare-function zulip-feed-retry-send "zulip-feed" (&optional message))
(declare-function zulip-feed-toggle-reaction "zulip-feed" (&optional reaction message-id))
(declare-function zulip-feed-toggle-star "zulip-feed" (&optional message))
(declare-function zulip-message-transient "zulip-transient" (&rest arguments))
(declare-function zulip-root-next-mentioned "zulip-root" ())
(declare-function zulip-root-next-unread "zulip-root" ())
(declare-function zulip-root-open-at-point "zulip-root" ())
(declare-function zulip-root-open-destination "zulip-root" ())
(declare-function zulip-root-open-new-direct-message "zulip-root" ())
(declare-function zulip-root-open-topic "zulip-root" ())
(declare-function zulip-root-refresh "zulip-root" ())
(declare-function zulip-root-search-messages "zulip-root" (&optional query))

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
  "Install navigator bindings without replacing native Evil prefixes."
  (appkit-evil-define-readonly-keys 'zulip-root-mode-map)
  (appkit-evil-map
    (:map zulip-root-mode-map
     :nm
     "RET" #'zulip-root-open-at-point
     "<return>" #'zulip-root-open-at-point
     "g r" #'zulip-root-refresh
     "g o" #'zulip-root-open-destination
     "g m" #'zulip-root-open-new-direct-message
     "g s" #'zulip-root-search-messages
     "g t" #'zulip-root-open-topic
     "U" #'zulip-root-next-unread
     "@" #'zulip-root-next-mentioned)))

(defun zulip-evil--define-feed-keys ()
  "Install feed-wide and timeline-only modal bindings."
  (appkit-evil-map
    (:map zulip-feed-mode-map
     :nm
     "g r" #'zulip-feed-load-latest
     "g [" #'zulip-feed-load-older
     "g ]" #'zulip-feed-load-newer
     "g t" #'zulip-feed-open-topic
     "?" #'zulip-message-transient)
    (:map zulip-feed-message-map
     :nm
     "q" #'quit-window
     "RET" #'zulip-feed-open-message-context
     "<return>" #'zulip-feed-open-message-context
     "i" #'appkit-evil-chatbuf-enter-input
     "T" #'zulip-feed-open-topic
     "Y" #'zulip-feed-copy-message
     "M" #'zulip-feed-mark-read
     "U" #'zulip-feed-mark-unread
     "S" #'zulip-feed-toggle-star
     "R" #'zulip-feed-toggle-reaction
     "E" #'zulip-feed-edit-message
     "g R" #'zulip-feed-retry-send
     "C-c C-k" #'zulip-feed-cancel-edit
     "?" #'zulip-message-transient
     :n
     "D" #'zulip-feed-delete-message)))

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

(provide 'zulip-evil)

;;; zulip-evil.el ends here
