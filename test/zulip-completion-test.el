;;; zulip-completion-test.el --- Tests for Zulip completion -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'zulip-runtime-test)
(require 'cl-lib)
(require 'zulip-completion)

(defun zulip-completion-test--user (id name &optional email active)
  "Return a normalized user with ID, NAME, EMAIL, and ACTIVE marker.

ACTIVE equal to `missing' omits `is_active'."
  (append
   `((id . ,id) (full_name . ,name))
   (when email `((email . ,email)))
   (unless (eq active 'missing) `((is_active . ,active)))))

(defun zulip-completion-test--account (&rest users)
  "Return an isolated account containing USERS."
  (let ((state (zulip-state-create)))
    (dolist (user users)
      (puthash (zulip-state-object-get user 'id)
               user
               (zulip-state-users state)))
    (zulip-runtime-create-account
     :server "https://chat.example.test"
     :email "me@example.test"
     :api-key "api-key-must-not-leak"
     :state state)))

(defmacro zulip-completion-test--with-cache (&rest body)
  "Run BODY with an isolated account candidate cache."
  (declare (indent 0) (debug t))
  `(zulip-runtime-test--isolated
     (let ((zulip-completion--account-cache (make-hash-table :test #'eq)))
       ,@body)))

(ert-deftest zulip-completion-builds-active-mention-candidates-without-secret ()
  (zulip-completion-test--with-cache
    (let* ((account
            (zulip-completion-test--account
             (zulip-completion-test--user
              "1" " Ada Lovelace " "ada@example.test" t)
             (zulip-completion-test--user
              "2" "Inactive" "inactive@example.test" :json-false)
             (zulip-completion-test--user
              "3" "Implicit Active" nil 'missing)))
           (candidates (zulip-completion-mention-candidates account)))
      (should
       (equal '("@Ada Lovelace" "@Implicit Active")
              (mapcar #'appkit-chat-completion-candidate-label candidates)))
      (should
       (equal '("@**Ada Lovelace|1**" "@**Implicit Active|3**")
              (mapcar #'appkit-chat-completion-candidate-insert candidates)))
      (should-not
       (string-match-p "api-key-must-not-leak" (prin1-to-string candidates))))))

(ert-deftest zulip-completion-colliding-labels-are-stable-and-casefold-unique ()
  (zulip-completion-test--with-cache
    (let* ((account
            (zulip-completion-test--account
             (zulip-completion-test--user "2" "Same" nil t)
             (zulip-completion-test--user "1" "same" nil t)))
           (candidates (zulip-completion-mention-candidates account))
           (labels (mapcar #'appkit-chat-completion-candidate-label candidates)))
      (should (equal '("@same (1)" "@Same (2)") labels))
      (should (= (length labels)
                 (length (delete-dups (mapcar #'downcase labels))))))))

(ert-deftest zulip-completion-cache-reuses-and-explicitly-invalidates ()
  (zulip-completion-test--with-cache
    (let* ((account
            (zulip-completion-test--account
             (zulip-completion-test--user "1" "Before" nil t)))
           (builds 0)
           (original (symbol-function 'zulip-completion--build-mention-candidates)))
      (cl-letf (((symbol-function 'zulip-completion--build-mention-candidates)
                 (lambda (state)
                   (cl-incf builds)
                   (funcall original state))))
        (should (eq (zulip-completion-mention-candidates account)
                    (zulip-completion-mention-candidates account)))
        (should (= builds 1))
        ;; Same-size realm_user changes rely on the public invalidation hook.
        (puthash "1"
                 (zulip-completion-test--user "1" "After" nil t)
                 (zulip-state-users (zulip-account-state account)))
        (should
         (equal "@Before"
                (appkit-chat-completion-candidate-label
                 (car (zulip-completion-mention-candidates account)))))
        (zulip-completion-invalidate-account-cache account)
        (should
         (equal "@After"
                (appkit-chat-completion-candidate-label
                 (car (zulip-completion-mention-candidates account)))))
        (should (= builds 2))))))

(ert-deftest zulip-completion-cache-detects-user-count-and-register-change ()
  (zulip-completion-test--with-cache
    (let* ((account
            (zulip-completion-test--account
             (zulip-completion-test--user "1" "One" nil t)))
           (state (zulip-account-state account)))
      (should (= 1 (length (zulip-completion-mention-candidates account))))
      (puthash "2" (zulip-completion-test--user "2" "Two" nil t)
               (zulip-state-users state))
      (should (= 2 (length (zulip-completion-mention-candidates account))))
      (setf (zulip-state-register-data state) '((queue_id . "new")))
      (puthash "1" (zulip-completion-test--user "1" "Renamed" nil t)
               (zulip-state-users state))
      (should
       (member "@Renamed"
               (mapcar #'appkit-chat-completion-candidate-label
                       (zulip-completion-mention-candidates account)))))))

(ert-deftest zulip-completion-mention-capf-inserts-atomic-visible-object ()
  (zulip-completion-test--with-cache
    (let ((account
           (zulip-completion-test--account
            (zulip-completion-test--user
             "42" "Ada Lovelace" "ada@example.test" t))))
      (with-temp-buffer
        (appkit-chatbuf-install-prompt ">>> ")
        (zulip-completion-setup account)
        (insert "@ada")
        (let* ((capf (zulip-completion-mention-capf))
               (table (nth 2 capf))
               (exit (plist-get (nthcdr 3 capf) :exit-function)))
          (should (equal '("@Ada Lovelace")
                         (all-completions "@ada" table)))
          (delete-region (- (point) 4) (point))
          (insert "@Ada Lovelace")
          (funcall exit "@Ada Lovelace" 'finished)
          (let* ((input (appkit-chatbuf-input-string))
                 (object (get-text-property
                          0 appkit-chatbuf-input-object-property input)))
            (should (equal "@Ada Lovelace " input))
            (should (eq (plist-get object :kind) 'zulip-mention))
            (should (equal (plist-get object :user-id) "42"))
            (should (equal (plist-get object :wire)
                           "@**Ada Lovelace|42**"))))))))

(ert-deftest zulip-completion-token-at-point-identifies-mention-and-emoji ()
  (with-temp-buffer
    (appkit-chatbuf-install-prompt ">>> ")
    (insert "@ada")
    (should (eq 'mention
                (plist-get (zulip-completion-token-at-point) :kind)))
    (delete-region (appkit-chatbuf-input-start-position) (point-max))
    (insert ":rocket:")
    (should (eq 'emoji
                (plist-get (zulip-completion-token-at-point) :kind)))))

(provide 'zulip-completion-test)

;;; zulip-completion-test.el ends here
