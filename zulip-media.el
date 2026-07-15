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
(require 'appkit-invalidation)
(require 'appkit-media)
(require 'zulip-customize)
(require 'zulip-http)
(require 'zulip-runtime)
(require 'zulip-state)

(cl-defstruct (zulip-media--avatar-cache
               (:constructor zulip-media--avatar-cache-create))
  "Account-scoped mutable state for progressively loaded avatars."
  images
  inflight
  failures
  urls)

(defconst zulip-media--avatar-store-key '(zulip-media avatar-cache)
  "Appkit resource-store key for one account's avatar cache.")

(defun zulip-media--new-table ()
  "Return an equal-tested media table."
  (make-hash-table :test #'equal))

(defun zulip-media--cache (account)
  "Return the account-owned avatar cache for ACCOUNT."
  (let* ((app (and (zulip-account-p account) (zulip-account-app account)))
         (store (and (appkit-app-p app) (appkit-app-resource-store app))))
    (unless store
      (error "Zulip account has no Appkit resource store"))
    (or (gethash zulip-media--avatar-store-key store)
        (let ((cache
               (zulip-media--avatar-cache-create
                :images (zulip-media--new-table)
                :inflight (zulip-media--new-table)
                :failures (zulip-media--new-table)
                :urls (zulip-media--new-table))))
          (puthash zulip-media--avatar-store-key cache store)
          cache))))

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

(defun zulip-media--notify-avatar (account user-id)
  "Redraw ACCOUNT feed rows depending on USER-ID's presentation."
  (let ((app (zulip-account-app account))
        (resource (list :user (format "%s" user-id))))
    (when (appkit-app-live-p app)
      (maphash
       (lambda (_view-id view)
         (when (and (appkit-view-live-p view)
                    (eq (appkit-view-mode view) 'zulip-feed-mode))
           (appkit-request-sync view :resource resource)))
       (appkit-app-view-registry app)))))

(defun zulip-media--current-url-p (account cache user-id url token)
  "Return non-nil when ACCOUNT URL and TOKEN identify USER-ID in CACHE."
  (let* ((record (gethash user-id
                          (zulip-media--avatar-cache-inflight cache)))
         (canonical
          (when-let* ((raw (zulip-media--raw-user-avatar-url account user-id)))
            (zulip-media--absolute-url account raw))))
    (and (appkit-app-live-p (zulip-account-app account))
         (eq token (plist-get record :token))
         (equal url (gethash user-id
                             (zulip-media--avatar-cache-urls cache)))
         ;; Unrelated canonical state publications are harmless.  Reject only
         ;; an actual user-avatar URL change while this transfer was running.
         (or (null canonical) (equal canonical url)))))

(defun zulip-media--finish-inflight (cache user-id token)
  "Finish USER-ID's TOKEN record in CACHE and forget its Appkit handle."
  (let* ((table (zulip-media--avatar-cache-inflight cache))
         (record (gethash user-id table)))
    (when (eq token (plist-get record :token))
      (remhash user-id table)
      (when-let* ((handle (plist-get record :lifecycle)))
        ;; A completed Appkit media caller handle is idempotently cancelable;
        ;; this call primarily removes the lifecycle wrapper from its owner.
        (appkit-cancel-handle handle))
      t)))

(defun zulip-media--avatar-succeeded
    (account cache user-id url token file)
  "Land ACCOUNT USER-ID avatar FILE in CACHE for current URL and TOKEN."
  (let ((current-p
         (zulip-media--current-url-p account cache user-id url token)))
    (zulip-media--finish-inflight cache user-id token)
    (when current-p
      (let ((image (zulip-media--image-from-file file)))
        (if image
            (progn
              (puthash user-id (cons url image)
                       (zulip-media--avatar-cache-images cache))
              (remhash user-id
                       (zulip-media--avatar-cache-failures cache)))
          (puthash user-id url
                   (zulip-media--avatar-cache-failures cache)))
        (zulip-media--notify-avatar account user-id)))))

(defun zulip-media--avatar-failed
    (account cache user-id url token _reason)
  "Record ACCOUNT USER-ID avatar failure in CACHE for current URL and TOKEN."
  (let ((current-p
         (zulip-media--current-url-p account cache user-id url token)))
    (zulip-media--finish-inflight cache user-id token)
    (when current-p
      (puthash user-id url (zulip-media--avatar-cache-failures cache))
      (zulip-media--notify-avatar account user-id))))

(defun zulip-media--cancel-old-inflight (cache user-id)
  "Cancel and forget CACHE's superseded avatar transfer for USER-ID."
  (let* ((table (zulip-media--avatar-cache-inflight cache))
         (record (gethash user-id table)))
    (when record
      ;; Forget first so the cancellation callback cannot mark the old URL as
      ;; the current failure.
      (remhash user-id table)
      (when-let* ((handle (plist-get record :lifecycle)))
        (appkit-cancel-handle handle)))))

(defun zulip-media--select-url (cache user-id url)
  "Make URL current for USER-ID in CACHE, clearing superseded values."
  (let ((urls (zulip-media--avatar-cache-urls cache)))
    (unless (equal url (gethash user-id urls))
      (zulip-media--cancel-old-inflight cache user-id)
      (remhash user-id (zulip-media--avatar-cache-images cache))
      (remhash user-id (zulip-media--avatar-cache-failures cache))
      (puthash user-id url urls))))

(defun zulip-media--start-avatar-fetch (account cache user-id url)
  "Start one avatar transfer owned by ACCOUNT in CACHE for USER-ID and URL."
  (let* ((app (zulip-account-app account))
         (inflight (zulip-media--avatar-cache-inflight cache))
         (token (list 'avatar url (float-time)))
         (record (list :token token :lifecycle nil))
         transfer)
    (puthash user-id record inflight)
    (setq transfer
          (appkit-media-cache-image-resource-async
           (appkit-media-resource-create
            :url url :name (or (appkit-media-url-filename url) "avatar.img"))
           (zulip-media--cache-base account user-id url)
           (lambda (file)
             (zulip-media--avatar-succeeded
              account cache user-id url token file))
           (lambda (reason)
             (zulip-media--avatar-failed
              account cache user-id url token reason))
           :headers (zulip-media--request-headers account url)))
    (cond
     ((not (appkit-media-transfer-p transfer))
      ;; Local/synchronous completion has already removed the token.  A setup
      ;; failure likewise arrives through the error callback.
      nil)
     ((not (eq token (plist-get (gethash user-id inflight) :token)))
      (appkit-media-cancel-transfer transfer)
      nil)
     ((not (appkit-app-live-p app))
      (remhash user-id inflight)
      (appkit-media-cancel-transfer transfer)
      nil)
     (t
      (let ((handle
             (appkit-register-handle
              app 'media-transfer transfer #'appkit-media-cancel-transfer)))
        (setq record (plist-put record :lifecycle handle))
        (puthash user-id record inflight)
        handle)))))

(defun zulip-media-avatar-image (account message)
  "Return cached sender avatar image for ACCOUNT and MESSAGE.

When no image is cached, start one deduplicated account-owned acquisition and
return nil.  The caller should render its stable text fallback; completion
invalidates only timeline rows that depend on this sender resource."
  (when (and zulip-show-avatar-images
             (zulip-account-p account)
             (appkit-app-live-p (zulip-account-app account))
             (appkit-media-inline-image-rendering-available-p))
    (when-let* ((user-id (zulip-media--sender-id message))
                (url (zulip-media--avatar-url account message)))
      (let* ((cache (zulip-media--cache account))
             (images (zulip-media--avatar-cache-images cache))
             (inflight (zulip-media--avatar-cache-inflight cache))
             (failures (zulip-media--avatar-cache-failures cache)))
        (zulip-media--select-url cache user-id url)
        (let ((cached (gethash user-id images)))
          (cond
           ((and (equal (car-safe cached) url)
                 (appkit-media-image-object-valid-p (cdr-safe cached)))
            (cdr cached))
           ((gethash user-id inflight) nil)
           ((equal (gethash user-id failures) url) nil)
           (t
            (let* ((base (zulip-media--cache-base account user-id url))
                   (file (appkit-media-image-cache-existing-file base))
                   (image (and file (zulip-media--image-from-file file))))
              (if image
                  (progn
                    (puthash user-id (cons url image) images)
                    image)
                (zulip-media--start-avatar-fetch
                 account cache user-id url)
                nil)))))))))

(provide 'zulip-media)

;;; zulip-media.el ends here
