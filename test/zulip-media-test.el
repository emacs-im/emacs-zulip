;;; zulip-media-test.el --- Tests for Zulip Appkit media adapters -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'zulip-feed)
(require 'zulip-media)

(defun zulip-media-test--state (avatar-url)
  "Return canonical test state whose self user has AVATAR-URL."
  (zulip-state-from-register
   (list
    (cons 'user_id 1)
    (cons 'realm_users
          (vector
           (list (cons 'user_id 1)
                 (cons 'full_name "Ada")
                 (cons 'email "ada@example.test")
                 (cons 'avatar_url avatar-url)))))))

(defmacro zulip-media-test--with-account (binding avatar-url &rest body)
  "Create isolated account BINDING with AVATAR-URL and evaluate BODY."
  (declare (indent 2) (debug (symbolp form body)))
  `(let ((zulip-runtime--accounts (make-hash-table :test #'equal)))
     (let* ((state (zulip-media-test--state ,avatar-url))
            (,binding
             (zulip-runtime-create-account
              :server "https://chat.example.test"
              :email "ada@example.test"
              :api-key "secret"
              :state state)))
       (unwind-protect
           (progn ,@body)
         (zulip-runtime-stop-all)))))

(defun zulip-media-test--message (&optional avatar-url)
  "Return a self message with optional fallback AVATAR-URL."
  (append
   '((id . "100") (type . "private") (sender_id . 1)
     (sender_full_name . "Ada") (content . "hello") (timestamp . 1))
   (when avatar-url `((avatar_url . ,avatar-url)))))

(ert-deftest zulip-media-prefers-canonical-avatar-and-authenticates-only-origin ()
  (zulip-media-test--with-account account "/user_uploads/avatar.png"
    (let* ((message
            (zulip-media-test--message
             "https://cdn.example.test/stale.png"))
           (url (zulip-media--avatar-url account message))
           (same-origin (zulip-media--request-headers account url))
           (external
            (zulip-media--request-headers
             account "https://secure.gravatar.com/avatar/example")))
      (should (equal url
                     "https://chat.example.test/user_uploads/avatar.png"))
      (should (assoc "Accept" same-origin))
      (should (assoc "Authorization" same-origin))
      (should (assoc "Accept" external))
      (should-not (assoc "Authorization" external)))))

(ert-deftest zulip-media-avatar-fetch-is-deduplicated-app-owned-and-state-safe ()
  (zulip-media-test--with-account account "/avatar/ada.png"
    (let ((fetch-count 0)
          success
          canceled
          (fake-image '(image :type png)))
      (cl-letf (((symbol-function
                  'appkit-media-inline-image-rendering-available-p)
                 (lambda () t))
                ((symbol-function 'appkit-media-image-cache-existing-file)
                 (lambda (_base) nil))
                ((symbol-function 'appkit-media-image-object-valid-p)
                 (lambda (image) (eq image fake-image)))
                ((symbol-function 'zulip-media--image-from-file)
                 (lambda (_file) fake-image))
                ((symbol-function 'appkit-media-transfer-p)
                 (lambda (object) (eq object :transfer)))
                ((symbol-function 'appkit-media-cancel-transfer)
                 (lambda (object) (push object canceled) t))
                ((symbol-function 'appkit-media-cache-image-resource-async)
                 (lambda (_resource _base on-success _on-error &rest _options)
                   (cl-incf fetch-count)
                   (setq success on-success)
                   :transfer)))
        (let ((message (zulip-media-test--message)))
          (should-not (zulip-media-avatar-image account message))
          (should-not (zulip-media-avatar-image account message))
          (should (= fetch-count 1))
          (should (= (length (appkit-app-handles
                              (zulip-account-app account)))
                     1))
          ;; Publishing unrelated state must not invalidate a byte source whose
          ;; canonical user avatar URL is unchanged.
          (zulip-runtime-publish-state
           account (zulip-state-copy (zulip-account-state account)))
          (funcall success "/tmp/ada.png")
          (should (eq (zulip-media-avatar-image account message) fake-image))
          (should-not (appkit-app-handles (zulip-account-app account)))
          (should (equal canceled '(:transfer))))))))

(ert-deftest zulip-media-stale-url-callback-cannot-overwrite-new-avatar ()
  (zulip-media-test--with-account account "/avatar/old.png"
    (let (successes canceled
          (old-image '(image :old))
          (new-image '(image :new)))
      (cl-letf (((symbol-function
                  'appkit-media-inline-image-rendering-available-p)
                 (lambda () t))
                ((symbol-function 'appkit-media-image-cache-existing-file)
                 (lambda (_base) nil))
                ((symbol-function 'appkit-media-image-object-valid-p)
                 (lambda (image) (memq image (list old-image new-image))))
                ((symbol-function 'zulip-media--image-from-file)
                 (lambda (file)
                   (if (string-match-p "old" file) old-image new-image)))
                ((symbol-function 'appkit-media-transfer-p)
                 (lambda (_object) t))
                ((symbol-function 'appkit-media-cancel-transfer)
                 (lambda (object) (push object canceled) t))
                ((symbol-function 'appkit-media-cache-image-resource-async)
                 (lambda (_resource _base on-success _on-error &rest _options)
                   (let ((transfer (intern (format ":transfer-%d"
                                                  (1+ (length successes))))))
                     (push (cons transfer on-success) successes)
                     transfer))))
        (let ((message (zulip-media-test--message)))
          (should-not (zulip-media-avatar-image account message))
          (let ((state (zulip-media-test--state "/avatar/new.png")))
            (zulip-runtime-publish-state account state))
          (should-not (zulip-media-avatar-image account message))
          (should (= (length successes) 2))
          (let ((new-callback (cdr (car successes)))
                (old-callback (cdr (cadr successes))))
            (funcall old-callback "/tmp/old.png")
            (should-not (zulip-media-avatar-image account message))
            (funcall new-callback "/tmp/new.png")
            (should (eq (zulip-media-avatar-image account message)
                        new-image)))
          (should (= (length canceled) 2)))))))

(ert-deftest zulip-media-avatar-cache-is-account-isolated ()
  (let ((zulip-runtime--accounts (make-hash-table :test #'equal))
        accounts)
    (unwind-protect
        (let* ((state (zulip-media-test--state "/avatar/shared.png"))
               (first
                (zulip-runtime-create-account
                 :server "https://one.example.test" :email "a@example.test"
                 :api-key "one" :state state))
               (second
                (zulip-runtime-create-account
                 :server "https://two.example.test" :email "a@example.test"
                 :api-key "two" :state (zulip-state-copy state))))
          (setq accounts (list first second))
          (should-not (eq (zulip-media--cache first)
                          (zulip-media--cache second)))
          (should-not
           (equal (zulip-media--cache-base first "1" "https://cdn/x.png")
                  (zulip-media--cache-base second "1" "https://cdn/x.png"))))
      (dolist (account accounts)
        (zulip-runtime-stop-account account)))))

(ert-deftest zulip-media-completion-invalidates-only-sender-resource ()
  (zulip-media-test--with-account account "/avatar/ada.png"
    (let (requested view)
      (with-temp-buffer
        (setq view
              (appkit-attach-view
               :app (zulip-account-app account) :id 'media-test
               :state nil :mode 'zulip-feed-mode
               :sync-function #'ignore :parts '(timeline)))
        (cl-letf (((symbol-function 'appkit-request-sync)
                   (lambda (value &rest options)
                     (setq requested (cons value options))))
                  ((symbol-function 'appkit-invalidate)
                   (lambda (&rest _arguments)
                     (ert-fail "avatar callback used bare invalidation")))
                  ((symbol-function 'appkit-schedule-sync)
                   (lambda (&rest _arguments)
                     (ert-fail "avatar callback used bare scheduling"))))
          (zulip-media--notify-avatar account "1"))
        (should (eq (car requested) view))
        (should (equal (plist-get (cdr requested) :resource)
                       '(:user "1")))))))

(ert-deftest zulip-feed-row-passes-avatar-image-to-shared-geometry ()
  (zulip-media-test--with-account account "/avatar/ada.png"
    (let (seen-image seen-options)
      (with-temp-buffer
        (setq-local zulip-feed--account account
                    zulip-feed--narrow (zulip-narrow-all)
                    zulip-feed--fill-column 80)
        (cl-letf (((symbol-function 'zulip-media-avatar-image)
                   (lambda (_account _message) :avatar-image))
                  ((symbol-function 'appkit-chat-avatar-prefixes)
                   (lambda (image _fallback &rest options)
                     (setq seen-image image seen-options options)
                     (list :header "A " :first-body "  " :rest-body "  "))))
          (zulip-feed--row-printer
           (appkit-chat-timeline-row-create
            :key "100" :payload (zulip-media-test--message)
            :context nil :dependencies '((:user "1")))))
        (should (eq seen-image :avatar-image))
        (should (eq (plist-get seen-options :resize) t))))))

(provide 'zulip-media-test)

;;; zulip-media-test.el ends here
