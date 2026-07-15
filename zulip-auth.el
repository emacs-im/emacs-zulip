;;; zulip-auth.el --- Zuliprc account discovery -*- lexical-binding: t; -*-

;;; Commentary:

;; Parse standard and multi-account zuliprc files without exposing API keys in
;; account-selection UI.  Connection remains owned by the package entry point;
;; this module only supplies credentials selected by the user.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'zulip-customize)

(declare-function zulip-connect "zulip" (server email api-key))

(cl-defstruct (zulip-auth-profile
               (:constructor zulip-auth-profile-create))
  "One complete account discovered in a zuliprc file."
  name server email api-key file)

(defun zulip-auth--complete-profile (name fields file)
  "Build a complete profile named NAME from FIELDS read from FILE.

Return nil when any standard credential field is absent or empty."
  (let ((server (gethash "site" fields))
        (email (gethash "email" fields))
        (api-key (gethash "key" fields)))
    (when (and (not (string-empty-p (or server "")))
               (not (string-empty-p (or email "")))
               (not (string-empty-p (or api-key ""))))
      (zulip-auth-profile-create
       :name name :server server :email email :api-key api-key :file file))))

(defun zulip-auth-read-profiles (&optional file)
  "Return complete account profiles parsed from zuliprc FILE.

FILE defaults to `zulip-rc-file'.  Return nil when no file is configured, the
file is unreadable, or it contains no complete sections.  Both the conventional
`[api]' section and arbitrary named sections are supported.  Keys are
case-insensitive; whitespace around sections, keys, and values is ignored.
Blank lines and lines beginning with `#' or `;' are comments.  Incomplete
sections are deliberately skipped rather than borrowing values from another
account."
  (let ((path (or file zulip-rc-file)))
    (when (and path (file-readable-p (expand-file-name path)))
      (setq path (expand-file-name path))
      (with-temp-buffer
        (insert-file-contents path)
        (let ((section nil)
              (fields nil)
              profiles
              first-line-p)
          (setq first-line-p t)
          (cl-labels
              ((finish-section
                ()
                (when section
                  (when-let* ((profile
                               (zulip-auth--complete-profile
                                section fields path)))
                    (push profile profiles)))))
            (dolist (raw-line (split-string (buffer-string) "\n"))
              (let* ((raw-line
                      (if first-line-p
                          (string-remove-prefix "\ufeff" raw-line)
                        raw-line))
                     (line (string-trim raw-line)))
                (setq first-line-p nil)
                (cond
                 ((or (string-empty-p line)
                      (memq (aref line 0) '(?# ?\;))))
                 ((string-match "\\`\\[\\(.*\\)\\]\\'" line)
                  (finish-section)
                  (setq section (string-trim (match-string 1 line))
                        fields (make-hash-table :test #'equal))
                  (when (string-empty-p section)
                    (setq section nil)))
                 ((and section (string-match "[:=]" line))
                  (let* ((separator (match-beginning 0))
                         (key (downcase
                               (string-trim (substring line 0 separator))))
                         (value (string-trim (substring line (1+ separator)))))
                    (unless (string-empty-p key)
                      (puthash key value fields)))))))
            (finish-section))
          (nreverse profiles))))))

(defun zulip-auth-profile-label (profile)
  "Return a secret-free completion label for PROFILE."
  (unless (zulip-auth-profile-p profile)
    (signal 'wrong-type-argument (list 'zulip-auth-profile-p profile)))
  (format "%s — %s @ %s"
          (zulip-auth-profile-name profile)
          (zulip-auth-profile-email profile)
          (zulip-auth-profile-server profile)))

(defun zulip-auth--completion-alist (profiles)
  "Return a secret-free, uniquely labelled completion alist for PROFILES."
  (let ((seen (make-hash-table :test #'equal)))
    (mapcar
     (lambda (profile)
       (let* ((base (zulip-auth-profile-label profile))
              (number (1+ (gethash base seen 0)))
              (label (if (= number 1)
                         base
                       (format "%s <%d>" base number))))
         (puthash base number seen)
         (cons label profile)))
     profiles)))

(defun zulip-auth-select-profile (profiles &optional prompt)
  "Select one of PROFILES without displaying its API key.

Return nil for an empty list and return the sole profile without prompting.
PROMPT defaults to `Zulip account: '."
  (pcase profiles
    ('nil nil)
    (`(,profile) profile)
    (_
     (let* ((choices (zulip-auth--completion-alist profiles))
            (label (completing-read (or prompt "Zulip account: ")
                                    choices nil t)))
       (cdr (assoc label choices))))))

(defun zulip-auth-connect-profile (profile)
  "Connect using PROFILE and return the resulting account.

The API key is passed directly to `zulip-connect' and is never included in a
completion label or status message."
  (unless (zulip-auth-profile-p profile)
    (signal 'wrong-type-argument (list 'zulip-auth-profile-p profile)))
  ;; Loading this module directly through its autoloaded command must still
  ;; make the entry-point connector available.  `zulip' itself requires this
  ;; feature, so the dependency remains acyclic after this feature is loaded.
  (unless (fboundp 'zulip-connect)
    (require 'zulip))
  (zulip-connect (zulip-auth-profile-server profile)
                 (zulip-auth-profile-email profile)
                 (zulip-auth-profile-api-key profile)))

;;;###autoload
(defun zulip-connect-from-zuliprc (file)
  "Read FILE, select a complete zuliprc account, and connect it.

With one complete section no account prompt is necessary.  With several,
completion displays only section name, email, and server."
  (interactive
   (let* ((configured (and zulip-rc-file (expand-file-name zulip-rc-file)))
          (directory (or (and configured (file-name-directory configured))
                         default-directory)))
     (list (read-file-name "Zuliprc file: " directory configured t))))
  (let* ((profiles (zulip-auth-read-profiles file))
         (profile (zulip-auth-select-profile profiles)))
    (unless profile
      (user-error "No complete Zulip accounts in %s" (abbreviate-file-name file)))
    (zulip-auth-connect-profile profile)))

(provide 'zulip-auth)

;;; zulip-auth.el ends here
