;;; zulip-auth.el --- Auth-source credentials for Zulip -*- lexical-binding: t; -*-

;;; Commentary:

;; Configured account targets contain only local labels, HTTPS origins, and
;; login emails.  API keys are resolved through auth-source and never enter
;; Customize or account-selection data.

;;; Code:

(require 'auth-source)
(require 'cl-lib)
(require 'subr-x)
(require 'url-parse)
(require 'zulip-customize)
(require 'zulip-runtime)

(declare-function zulip-connect "zulip" (server email api-key))

(defconst zulip-auth--service "zulip"
  "Auth-source service label for default-port Zulip credentials.")

(cl-defstruct (zulip-auth-target
               (:constructor zulip-auth-target--create))
  "One validated non-secret configured Zulip account target."
  name server email)

(defun zulip-auth--nonblank-string-p (value)
  "Return non-nil when VALUE is a nonblank string."
  (and (stringp value) (not (string-empty-p (string-trim value)))))

(defun zulip-auth--normalized-origin (server)
  "Return validated canonical HTTPS origin for SERVER."
  (unless (zulip-auth--nonblank-string-p server)
    (user-error "Zulip account server must be a nonempty HTTPS origin"))
  (let* ((url (url-generic-parse-url (string-trim server)))
         (scheme (url-type url))
         (host (url-host url))
         (port (or (url-port url) 443))
         (path (or (url-filename url) "")))
    (unless (and (equal scheme "https")
                 (zulip-auth--nonblank-string-p host)
                 (string-match-p "\\`[A-Za-z0-9.:-]+\\'" host)
                 (integerp port)
                 (< 0 port 65536)
                 (null (url-user url))
                 (null (url-password url))
                 (member path '("" "/"))
                 (null (url-target url)))
      (user-error
       "Zulip account server must be an HTTPS origin without credentials, path, query, or fragment"))
    (concat "https://" (downcase host)
            (if (= port 443) "" (format ":%d" port)))))

(defun zulip-auth--normalized-email (email)
  "Return validated trimmed Zulip login EMAIL."
  (unless (and (zulip-auth--nonblank-string-p email)
               (not (string-match-p "[\r\n]" email)))
    (user-error "Zulip account email must be nonempty and single-line"))
  (string-trim email))

(defun zulip-auth--validate-target-spec (spec)
  "Validate configured target SPEC and return a normalized target."
  (unless (listp spec)
    (user-error "Zulip account must be a plist: %S" spec))
  (let ((cursor spec)
        seen)
    (while cursor
      (unless (and (consp cursor) (consp (cdr cursor)))
        (user-error "Zulip account plist is malformed: %S" spec))
      (let ((key (car cursor)))
        (unless (memq key '(:name :server :email))
          (user-error "Unknown Zulip account option %S" key))
        (when (memq key seen)
          (user-error "Duplicate Zulip account option %S" key))
        (push key seen))
      (setq cursor (cddr cursor))))
  (let ((name (plist-get spec :name)))
    (unless (zulip-auth--nonblank-string-p name)
      (user-error "Zulip account :name must be a nonempty string"))
    (zulip-auth-target--create
     :name (string-trim name)
     :server (zulip-auth--normalized-origin (plist-get spec :server))
     :email (zulip-auth--normalized-email (plist-get spec :email)))))

(defun zulip-auth-configured-targets ()
  "Return validated targets from `zulip-accounts'.

Names and normalized server/email identities must both be unique."
  (let ((names (make-hash-table :test #'equal))
        (identities (make-hash-table :test #'equal))
        targets)
    (dolist (spec zulip-accounts)
      (let* ((target (zulip-auth--validate-target-spec spec))
             (name-key (downcase (zulip-auth-target-name target)))
             (identity
              (zulip-runtime-account-id
               (zulip-auth-target-server target)
               (zulip-auth-target-email target))))
        (when (gethash name-key names)
          (user-error "Duplicate Zulip account name: %s"
                      (zulip-auth-target-name target)))
        (when (gethash identity identities)
          (user-error "Duplicate Zulip account target: %s"
                      (zulip-auth-target-name target)))
        (puthash name-key t names)
        (puthash identity t identities)
        (push target targets)))
    (nreverse targets)))

(defun zulip-auth-target-label (target)
  "Return a secret-free completion label for TARGET."
  (unless (zulip-auth-target-p target)
    (signal 'wrong-type-argument (list 'zulip-auth-target-p target)))
  (format "%s — %s @ %s"
          (zulip-auth-target-name target)
          (zulip-auth-target-email target)
          (zulip-auth-target-server target)))

(defun zulip-auth-select-target (targets &optional prompt)
  "Select one of TARGETS without consulting credential storage."
  (pcase targets
    ('nil nil)
    (`(,target) target)
    (_
     (let* ((choices
             (mapcar (lambda (target)
                       (cons (zulip-auth-target-label target) target))
                     targets))
            (label
             (completing-read
              (or prompt "Zulip account: ") choices nil t)))
       (cdr (assoc label choices))))))

(defun zulip-auth-source-spec (server email)
  "Return exact auth-source locator for Zulip SERVER and EMAIL."
  (let* ((origin (zulip-auth--normalized-origin server))
         (url (url-generic-parse-url origin))
         (port (or (url-port url) 443)))
    (list :host (url-host url)
          :user (zulip-auth--normalized-email email)
          :port (if (= port 443)
                    zulip-auth--service
                  (format "%s-%d" zulip-auth--service port)))))

(defun zulip-auth-api-key (server email)
  "Return a mutable copy of SERVER and EMAIL's auth-source API key."
  (let* ((spec (zulip-auth-source-spec server email))
         (source
          (car
           (apply #'auth-source-search
                  (append spec '(:require (:secret :port) :max 1)))))
         (secret (and source (auth-info-password source))))
    (unless (and (stringp secret)
                 (not (string-empty-p secret))
                 (not (string-match-p "[\r\n]" secret)))
      (user-error
       "No valid Zulip API key for host %s, user %s, and service %s in auth-source"
       (plist-get spec :host)
       (plist-get spec :user)
       (plist-get spec :port)))
    (copy-sequence secret)))

(provide 'zulip-auth)

;;; zulip-auth.el ends here
