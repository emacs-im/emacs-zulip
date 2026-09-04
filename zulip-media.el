;;; zulip-media.el --- Account-owned Zulip media adapters -*- lexical-binding: t; -*-

;;; Commentary:

;; Appkit owns resource acquisition, atomic disk caching, image construction,
;; and lifecycle handles.  This module supplies the Zulip-specific resource
;; identity and authentication policy.  Avatar transfers belong to the account
;; application, so closing one feed cannot cancel an image another feed needs.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'url-expand)
(require 'url-parse)
(require 'appkit-core)
(require 'appkit-resource)
(require 'appkit-media-image)
(require 'appkit-media-resource)
(require 'appkit-media-effect)
(require 'zulip-customize)
(require 'zulip-http)
(require 'zulip-runtime)
(require 'zulip-state)

(defun zulip-media--field (object key)
  "Return OBJECT field KEY through the canonical Zulip accessor."
  (and object (zulip-state-object-get object key)))

(defun zulip-media--sender-id (message)
  "Return MESSAGE sender ID as an opaque string, or nil."
  (when-let* ((id (or (zulip-media--field message 'sender-id)
                      (zulip-media--field message 'sender_id))))
    (format "%s" id)))

(defun zulip-media--raw-user-avatar-url (account user-id)
  "Return canonical USER-ID avatar URL recorded by ACCOUNT, or nil."
  (when-let* ((state (zulip-account-state account))
              (user (and (zulip-state-p state)
                         (zulip-state-user state user-id)))
              (url (or (zulip-media--field user 'avatar-url)
                       (zulip-media--field user 'avatar_url))))
    (and (stringp url) (not (string-empty-p url)) url)))

(defun zulip-media--raw-message-avatar-url (message)
  "Return MESSAGE's fallback avatar URL, or nil."
  (when-let* ((url (or (zulip-media--field message 'avatar-url)
                       (zulip-media--field message 'avatar_url))))
    (and (stringp url) (not (string-empty-p url)) url)))

(defun zulip-media--absolute-url (account raw-url)
  "Resolve ACCOUNT's RAW-URL and return an HTTP(S) URL, or nil."
  (when (and (zulip-account-p account)
             (stringp raw-url)
             (not (string-empty-p raw-url)))
    (let* ((server (zulip-account-server account))
           (url (condition-case nil
                    (url-expand-file-name raw-url (concat server "/"))
                  (error nil)))
           (parsed (and url (ignore-errors (url-generic-parse-url url))))
           (scheme (and parsed (downcase (or (url-type parsed) "")))))
      (and (member scheme '("http" "https")) url))))

(defun zulip-media--avatar-url (account message)
  "Return ACCOUNT's preferred absolute avatar URL for MESSAGE."
  (when-let* ((user-id (zulip-media--sender-id message))
              (raw (or (zulip-media--raw-user-avatar-url account user-id)
                       (zulip-media--raw-message-avatar-url message))))
    (zulip-media--absolute-url account raw)))

(defun zulip-media--default-port (scheme)
  "Return the default network port for URL SCHEME."
  (pcase scheme
    ("http" 80)
    ("https" 443)))

(defun zulip-media--origin (url)
  "Return normalized `(scheme host port)' origin for URL, or nil."
  (when-let* ((parsed (ignore-errors (url-generic-parse-url url)))
              (scheme (downcase (or (url-type parsed) "")))
              (host (url-host parsed)))
    (when (member scheme '("http" "https"))
      (list scheme (downcase host)
            (or (url-port parsed) (zulip-media--default-port scheme))))))

(defun zulip-media--same-origin-p (account url)
  "Return non-nil when URL has ACCOUNT's Zulip server origin."
  (equal (zulip-media--origin (zulip-account-server account))
         (zulip-media--origin url)))

(defun zulip-media--request-headers (account url)
  "Return safe image request headers for ACCOUNT and URL.

Authentication is included only for a byte source on the Zulip server's own
origin.  External Gravatar and CDN URLs must never receive account secrets."
  (append
   (copy-tree appkit-media-image-accept-headers)
   (when (zulip-media--same-origin-p account url)
     (list (cons "Authorization" (zulip-http-basic-auth account))))))

(defun zulip-media--cache-base (account user-id url)
  "Return disk cache base for ACCOUNT USER-ID and avatar URL."
  (expand-file-name
   (md5 (prin1-to-string (list (zulip-account-id account) user-id url)))
   (expand-file-name "avatars/" zulip-media-cache-directory)))

(defun zulip-media--image-from-file (file)
  "Return an Appkit-valid image descriptor for FILE, or nil."
  (when (appkit-media-file-present-p file)
    (condition-case nil
        (let ((image (create-image file nil nil :ascent 'center)))
          (and (appkit-media-image-object-valid-p image) image))
      (error nil))))

(defun zulip-media-avatar-demand (account message)
  "Return ACCOUNT MESSAGE's declarative avatar Resource demand, or nil.
The key is (avatar SENDER-ID ABSOLUTE-URL).  Projected rows retain the
demand and include its key in their dependencies."
  (when (and zulip-show-avatar-images
             (zulip-account-p account)
             (appkit-app-live-p (zulip-account-app account))
             (appkit-media-inline-image-rendering-available-p))
    (when-let* ((user-id (zulip-media--sender-id message))
                (url (zulip-media--avatar-url account message)))
      (let ((key (list 'avatar user-id url)))
        (appkit-resource-demand-create
         :key key
         :input (appkit-media-image-acquisition-create
                 (appkit-media-resource-create
                  :url url
                  :name (or (appkit-media-url-filename url) "avatar.img"))
                 (zulip-media--cache-base account user-id url)
                 :headers (zulip-media--request-headers account url))
         :loader #'appkit-media-image-resource-load
         :acquisition-identity (list 'zulip-avatar key url)
         :sharing-policy 'app-private
         :cache-policy 'while-interested)))))

(defun zulip-media--resource-image (account key)
  "Read ACCOUNT KEY's ready Resource, decoding only its local file.
No acquisition occurs here; projected row interests own its lifetime."
  (when-let* ((surface (appkit-current-surface))
              ((eq (appkit-surface-app surface) (zulip-account-app account)))
              (state (appkit-resource-state surface key))
              ((eq (appkit-resource-state-status state) 'ready))
              (file (appkit-resource-state-value state)))
    (let* ((cache (zulip-account-decoded-images account))
           (cached (gethash key cache)))
      (if (equal file (car-safe cached))
          (cdr cached)
        (let ((image (zulip-media--image-from-file file)))
          (puthash key (cons file image) cache)
          image)))))

(defun zulip-media-avatar-image (account message)
  "Return ACCOUNT MESSAGE's ready avatar without starting acquisition.
The canonical user's current URL takes precedence over MESSAGE's fallback,
so a superseded URL can never supply the current avatar."
  (when (and zulip-show-avatar-images
             (zulip-account-p account)
             (appkit-app-live-p (zulip-account-app account))
             (appkit-media-inline-image-rendering-available-p))
    (when-let* ((user-id (zulip-media--sender-id message))
                (url (zulip-media--avatar-url account message)))
      (zulip-media--resource-image account (list 'avatar user-id url)))))

(provide 'zulip-media)

;;; zulip-media.el ends here
