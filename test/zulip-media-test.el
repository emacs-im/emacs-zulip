;;; zulip-media-test.el --- Tests for Zulip Appkit media adapters -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'appkit-projection)
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

(defun zulip-media-test--message (&optional avatar-url)
  "Return a self message with optional fallback AVATAR-URL."
  (append
   '((id . "100") (type . "private") (sender_id . 1)
     (sender_full_name . "Ada") (content . "hello") (timestamp . 1))
   (when avatar-url `((avatar_url . ,avatar-url)))))

(defmacro zulip-media-test--with-account (binding avatar-url &rest body)
  "Create isolated account BINDING with AVATAR-URL and evaluate BODY."
  (declare (indent 2) (debug (symbolp form body)))
  `(let ((zulip-runtime--accounts (make-hash-table :test #'equal))
         (zulip-runtime-change-hook nil)
         (zulip-show-avatar-images t))
     (let ((,binding
            (zulip-runtime-create-account
             :server "https://chat.example.test"
             :email "ada@example.test"
             :api-key "synthetic-key"
             :state (zulip-media-test--state ,avatar-url))))
       (unwind-protect
           (progn ,@body)
         (zulip-runtime-stop-all)))))

(defun zulip-media-test--drain (account)
  "Drain ACCOUNT's real App and Surface loops without timers or network."
  (let ((app (zulip-account-app account))
        (remaining 100)
        pending)
    (when (appkit-app-live-p app)
      (while
          (progn
            (setq pending nil)
            (let ((loops (list (appkit-app-loop app))))
              (maphash (lambda (_identity entry)
                         (push (appkit-surface-loop (cdr entry)) loops))
                       (appkit-app-surfaces app))
              (dolist (loop loops)
                (when (> (appkit-loop-pending-count loop) 0)
                  (setq pending t)
                  (appkit-loop-run-pass loop))
                (when (eq (appkit-loop-status loop) 'faulted)
                  (error "Zulip media fixture loop fault: %S"
                         (appkit-loop-fault loop)))))
            (when (and pending (<= (cl-decf remaining) 0))
              (error "Zulip media fixture did not quiesce"))
            pending)))))

(defun zulip-media-test--row (key &optional demand)
  "Return projected KEY with a real optional Resource DEMAND."
  (appkit-projection-row-create
   :key key :payload key
   :resource-demands (and demand (list demand))
   :dependencies (and demand (list (appkit-resource-demand-key demand)))))

(defun zulip-media-test--surface (account identity project &optional printer)
  "Mount ACCOUNT IDENTITY using real projection PROJECT and PRINTER."
  (appkit-open-generated-surface
   (appkit-surface-type-create
    :name 'zulip-media-test :mode #'special-mode
    :init (lambda (_context input)
            (appkit-next :model input
                         :render (appkit-projection-change-create :full-p t)))
    :update (lambda (_context model message)
              (appkit-next :model model
                           :render (if (appkit-projection-change-p message)
                                       message
                                     appkit-render-none)))
    :renderer-factory
    (lambda (_surface)
      (appkit-projection-renderer-create
       :project-all (lambda (_surface _app _model) (funcall project))
       :printer (lambda (_surface _app row)
                  (when printer (funcall printer (appkit-projection-row-key row)))
                  (insert (appkit-projection-row-key row) "\n")))))
   :app (zulip-account-app account) :identity identity))

(ert-deftest zulip-media-avatar-acquisition-authenticates-only-the-account-origin ()
  (zulip-media-test--with-account account "/avatar/canonical.png"
    (let (requests)
      (cl-letf (((symbol-function 'appkit-media-inline-image-rendering-available-p)
                 (lambda () t))
                ((symbol-function 'appkit-media-image-cache-existing-file)
                 (lambda (_base) nil))
                ((symbol-function 'appkit-media-cache-image-resource-async)
                 (lambda (resource _base _resolve _reject &rest options)
                   (push (cons (alist-get 'url resource)
                               (plist-get options :headers)) requests)
                   (appkit-media--transfer-handle-create)))
                ((symbol-function 'appkit-media-cancel-transfer) #'ignore))
        (let* ((message (zulip-media-test--message "https://cdn.example.test/stale.png"))
               (surface
                (zulip-media-test--surface
                 account 'canonical
                 (lambda () (list (zulip-media-test--row
                                   "100" (zulip-media-avatar-demand account message)))))))
          (zulip-media-test--drain account)
          (should (equal (caar requests)
                         "https://chat.example.test/avatar/canonical.png"))
          (should (equal (cdr (assoc "Authorization" (cdar requests)))
                         (concat "Basic "
                                 (base64-encode-string
                                  "ada@example.test:synthetic-key" t))))
          (appkit-surface-stop surface))
        (zulip-runtime-publish-state account (zulip-media-test--state nil))
        (zulip-media-test--drain account)
        (dolist (case '(("/avatar/relative.png" "https://chat.example.test/avatar/relative.png" t)
                        ("https://chat.example.test:443/avatar/default.png" "https://chat.example.test:443/avatar/default.png" t)
                        ("http://chat.example.test/avatar/downgrade.png" "http://chat.example.test/avatar/downgrade.png" nil)
                        ("https://chat.example.test:8443/avatar/port.png" "https://chat.example.test:8443/avatar/port.png" nil)
                        ("//cdn.example.test/avatar.png" "https://cdn.example.test/avatar.png" nil)
                        ("https://secure.gravatar.com/avatar/example" "https://secure.gravatar.com/avatar/example" nil)))
          (let* ((message (zulip-media-test--message (car case)))
                 (surface
                  (zulip-media-test--surface
                   account (car case)
                   (lambda () (list (zulip-media-test--row
                                     "100" (zulip-media-avatar-demand account message)))))))
            (zulip-media-test--drain account)
            (should (equal (caar requests) (cadr case)))
            (should (assoc "Accept" (cdar requests)))
            (should (eq (not (null (assoc "Authorization" (cdar requests))))
                        (nth 2 case)))
            (appkit-surface-stop surface)))))))

(ert-deftest zulip-media-avatar-ready-read-does-not-acquire-without-row-interest ()
  (zulip-media-test--with-account account "/avatar/ada.png"
    (cl-letf (((symbol-function 'appkit-media-inline-image-rendering-available-p)
               (lambda () t))
              ((symbol-function 'appkit-media-cache-image-resource-async)
               (lambda (&rest _arguments)
                 (ert-fail "Ready read started an acquisition")))
              ((symbol-function 'appkit-media-image-cache-existing-file)
               (lambda (_base)
                 (ert-fail "Ready read probed an acquisition cache"))))
      (let ((surface
             (zulip-media-test--surface
              account 'no-interest
              (lambda () (list (zulip-media-test--row "100")))
              (lambda (_key)
                (should-not (zulip-media-avatar-image
                             account (zulip-media-test--message)))))))
        (zulip-media-test--drain account)
        (with-current-buffer (appkit-surface-buffer surface)
          (should-not (zulip-media-avatar-image account (zulip-media-test--message))))))))

(ert-deftest zulip-media-avatar-interest-shares-within-account-and-cancels-last-owner ()
  (zulip-media-test--with-account account "https://cdn.example.test/shared.png"
    (let ((other (zulip-runtime-create-account
                  :server "https://chat.example.test" :email "other@example.test"
                  :api-key "other-synthetic-key"
                  :state (zulip-media-test--state "https://cdn.example.test/shared.png")))
          (starts 0)
          handles canceled bases first second third)
      (cl-letf (((symbol-function 'appkit-media-inline-image-rendering-available-p)
                 (lambda () t))
                ((symbol-function 'appkit-media-image-cache-existing-file)
                 (lambda (_base) nil))
                ((symbol-function 'appkit-media-cache-image-resource-async)
                 (lambda (_resource base &rest _arguments)
                   (cl-incf starts)
                   (push base bases)
                   (let ((handle (appkit-media--transfer-handle-create)))
                     (push handle handles)
                     handle)))
                ((symbol-function 'appkit-media-cancel-transfer)
                 (lambda (handle) (push handle canceled))))
        (let ((project
               (lambda () (list (zulip-media-test--row
                                 "100" (zulip-media-avatar-demand
                                        account (zulip-media-test--message)))))))
          (setq first (zulip-media-test--surface account 'first project)
                second (zulip-media-test--surface account 'second project)))
        (zulip-media-test--drain account)
        (should (= starts 1))
        (setq third
              (zulip-media-test--surface
               other 'third
               (lambda () (list (zulip-media-test--row
                                 "100" (zulip-media-avatar-demand
                                        other (zulip-media-test--message)))))))
        (zulip-media-test--drain other)
        (should (= starts 2))
        (should-not (equal (car bases) (cadr bases)))
        (appkit-surface-stop first)
        (should-not canceled)
        (appkit-surface-stop second)
        (should (equal canceled (list (cadr handles))))
        (appkit-surface-stop third)
        (should (equal canceled handles))))))

(ert-deftest zulip-media-avatar-ready-notifies-dependent-rows-and-isolates-accounts ()
  (zulip-media-test--with-account account "https://cdn.example.test/shared.png"
    (let ((other (zulip-runtime-create-account
                  :server "https://chat.example.test" :email "other@example.test"
                  :api-key "other-synthetic-key"
                  :state (zulip-media-test--state "https://cdn.example.test/shared.png")))
          callbacks rendered surface other-surface)
      (cl-letf (((symbol-function 'appkit-media-inline-image-rendering-available-p)
                 (lambda () t))
                ((symbol-function 'appkit-media-image-cache-existing-file)
                 (lambda (_base) nil))
                ((symbol-function 'appkit-media-cache-image-resource-async)
                 (lambda (_resource _base resolve _reject &rest _arguments)
                   (push resolve callbacks)
                   (appkit-media--transfer-handle-create)))
                ((symbol-function 'appkit-media-cancel-transfer) #'ignore)
                ((symbol-function 'zulip-media--image-from-file) #'identity))
        (setq surface
              (zulip-media-test--surface
               account 'rows
               (lambda ()
                 (let ((demand (zulip-media-avatar-demand
                                account (zulip-media-test--message))))
                   (list (zulip-media-test--row "100" demand)
                         (zulip-media-test--row "101" demand)
                         (zulip-media-test--row "102"))))
               (lambda (key) (push key rendered))))
        (zulip-media-test--drain account)
        (let ((first (car callbacks)))
          (setq other-surface
                (zulip-media-test--surface
                 other 'rows
                 (lambda () (list (zulip-media-test--row
                                   "100" (zulip-media-avatar-demand
                                          other (zulip-media-test--message)))))))
          (zulip-media-test--drain other)
          (setq rendered nil)
          (funcall first "/synthetic/first.png")
          (zulip-media-test--drain account)
          (should (equal (sort rendered #'string<) '("100" "101")))
          (with-current-buffer (appkit-surface-buffer surface)
            (should (equal (zulip-media-avatar-image account (zulip-media-test--message))
                           "/synthetic/first.png"))
            (should-not (zulip-media-avatar-image other (zulip-media-test--message))))
          (with-current-buffer (appkit-surface-buffer other-surface)
            (should-not (zulip-media-avatar-image other (zulip-media-test--message)))))))))

(ert-deftest zulip-media-avatar-replacement-rejects-stale-url-and-handles-sync-settlement ()
  (zulip-media-test--with-account account "/avatar/old.png"
    (let (callbacks handles canceled immediate surface)
      (cl-letf (((symbol-function 'appkit-media-inline-image-rendering-available-p)
                 (lambda () t))
                ((symbol-function 'appkit-media-image-cache-existing-file)
                 (lambda (_base) nil))
                ((symbol-function 'appkit-media-cache-image-resource-async)
                 (lambda (_resource _base resolve _reject &rest _arguments)
                   (let ((handle (appkit-media--transfer-handle-create)))
                     (push resolve callbacks)
                     (push handle handles)
                     (when immediate (funcall resolve immediate))
                     handle)))
                ((symbol-function 'appkit-media-cancel-transfer)
                 (lambda (handle) (push handle canceled)))
                ((symbol-function 'zulip-media--image-from-file) #'identity))
        (setq surface
              (zulip-media-test--surface
               account 'avatar
               (lambda () (list (zulip-media-test--row
                                 "100" (zulip-media-avatar-demand
                                        account (zulip-media-test--message)))))))
        (zulip-media-test--drain account)
        (let ((old (car callbacks))
              (old-handle (car handles)))
          (zulip-runtime-publish-state
           account (zulip-media-test--state "/avatar/new.png"))
          (zulip-media-test--drain account)
          (with-current-buffer (appkit-surface-buffer surface)
            (should-not (zulip-media-avatar-image account (zulip-media-test--message))))
          (setq immediate "/synthetic/new.png")
          (appkit-surface-send surface (appkit-projection-change-create :full-p t))
          (zulip-media-test--drain account)
          (should (memq old-handle canceled))
          (funcall old "/synthetic/late-old.png")
          (zulip-media-test--drain account)
          (with-current-buffer (appkit-surface-buffer surface)
            (should (equal (zulip-media-avatar-image account (zulip-media-test--message))
                           "/synthetic/new.png"))))))))

(provide 'zulip-media-test)

;;; zulip-media-test.el ends here
