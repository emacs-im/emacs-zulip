;;; zulip-modes.el --- Global presentation modes for emacs-zulip -*- lexical-binding: t; -*-

;;; Commentary:

;; Telega-style global mode-line status aggregated across live Zulip accounts.

;;; Code:

(require 'cl-lib)
(require 'appkit-mode-line)
(require 'zulip-root)
(require 'zulip-runtime)
(require 'zulip-state)

(declare-function zulip "zulip" (&optional server email api-key))

(defface zulip-mode-line-unread-face
  '((t :inherit font-lock-warning-face :weight bold))
  "Face used for the global Zulip unread count."
  :group 'zulip)

(defface zulip-mode-line-mention-face
  '((t :inherit error :weight bold))
  "Face used for the global Zulip mention count."
  :group 'zulip)

(defvar zulip-mode-line-string ""
  "Cached emacs-zulip mode-line string.")

(defvar zulip-mode-line--cached-counts '(0 . 0)
  "Cached (UNREAD . MENTIONS) counts across live Zulip accounts.")

(defcustom zulip-mode-line-format
  '(zulip-mode-line-mode ("" zulip-mode-line-string))
  "Mode-line construct installed in `mode-line-misc-info'."
  :type 'sexp
  :group 'zulip
  :risky t)

(defcustom zulip-mode-line-string-format
  '((:eval (zulip-mode-line-icon))
    (:eval (zulip-mode-line-unread))
    (:eval (zulip-mode-line-mentions)))
  "Format cached by `zulip-mode-line-mode'."
  :type 'sexp
  :group 'zulip
  :risky t)

(defun zulip-mode-line--counts ()
  "Return aggregate (UNREAD . MENTIONS) for live Zulip accounts."
  (let ((unread 0)
        (mentions 0))
    (dolist (account (zulip-runtime-accounts))
      (let ((state (zulip-account-state account)))
        (when (zulip-state-p state)
          (cl-incf unread (max 0 (or (zulip-state-unread-count state) 0)))
          (cl-incf mentions
                   (hash-table-count (zulip-state-unread-mentions state))))))
    (cons unread mentions)))

(defun zulip-mode-line--open-root ()
  "Open a live account root, prompting only when several accounts exist."
  (let ((accounts (zulip-runtime-accounts)))
    (pcase accounts
      (`nil (call-interactively #'zulip))
      (`(,account) (zulip-root-open account))
      (_ (call-interactively #'zulip-root-open)))))

(defun zulip-mode-line-open-root ()
  "Open Zulip from the global mode-line indicator."
  (interactive)
  (zulip-mode-line--open-root))

(defun zulip-mode-line-open-unread ()
  "Open a Zulip root and move to its next unread destination."
  (interactive)
  (zulip-mode-line--open-root)
  (when (derived-mode-p 'zulip-root-mode)
    (zulip-root-next-unread)))

(defun zulip-mode-line-open-mentions ()
  "Open a Zulip root and move to its next destination with mentions."
  (interactive)
  (zulip-mode-line--open-root)
  (when (derived-mode-p 'zulip-root-mode)
    (zulip-root-next-mentioned)))

(defun zulip-mode-line-icon ()
  "Return the clickable Zulip mode-line label."
  (appkit-mode-line-indicator
   "Zulip" :prefix "  " :face 'mode-line-emphasis
   :command #'zulip-mode-line-open-root :help-echo "Open Zulip"))

(defun zulip-mode-line-unread ()
  "Return a clickable aggregate unread indicator."
  (let ((count (car zulip-mode-line--cached-counts)))
    (unless (zerop count)
      (appkit-mode-line-indicator
       (number-to-string count) :prefix " " :face 'zulip-mode-line-unread-face
       :command #'zulip-mode-line-open-unread
       :help-echo "Open an unread Zulip destination"))))

(defun zulip-mode-line-mentions ()
  "Return a clickable aggregate unread-mention indicator."
  (let ((count (cdr zulip-mode-line--cached-counts)))
    (unless (zerop count)
      (appkit-mode-line-indicator
       (format "@%d" count) :prefix " " :face 'zulip-mode-line-mention-face
       :command #'zulip-mode-line-open-mentions
       :help-echo "Open a Zulip destination with unread mentions"))))

(defun zulip-mode-line-update (&rest _ignored)
  "Refresh cached global Zulip mode-line state."
  (when zulip-mode-line-mode
    (setq zulip-mode-line--cached-counts (zulip-mode-line--counts))
    (appkit-mode-line-update-cache
     'zulip-mode-line-string zulip-mode-line-string-format)))

;;;###autoload
(define-minor-mode zulip-mode-line-mode
  "Toggle global Zulip unread and mention status in the mode line."
  :init-value nil
  :global t
  :group 'zulip
  (if zulip-mode-line-mode
      (progn
        (appkit-mode-line-install 'zulip-mode-line-format)
        (add-hook 'zulip-runtime-change-hook #'zulip-mode-line-update)
        (zulip-mode-line-update)
        (force-mode-line-update t))
    (appkit-mode-line-uninstall 'zulip-mode-line-format)
    (remove-hook 'zulip-runtime-change-hook #'zulip-mode-line-update)
    (setq zulip-mode-line-string ""
          zulip-mode-line--cached-counts '(0 . 0))
    (force-mode-line-update t)))

(provide 'zulip-modes)

;;; zulip-modes.el ends here
