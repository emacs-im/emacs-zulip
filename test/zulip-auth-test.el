;;; zulip-auth-test.el --- Zuliprc discovery tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'zulip-auth)
(require 'zulip)

(defmacro zulip-auth-test--with-file (contents binding &rest body)
  "Write CONTENTS to a temporary zuliprc bound as BINDING, then run BODY."
  (declare (indent 2) (debug (form symbolp body)))
  `(let ((,binding (make-temp-file "emacs-zuliprc-")))
     (unwind-protect
         (progn
           (with-temp-file ,binding
             (insert ,contents))
           ,@body)
       (delete-file ,binding))))

(ert-deftest zulip-auth-parses-standard-and-named-sections ()
  (zulip-auth-test--with-file
      (concat
       "\ufeff  # leading comment\n"
       "\n"
       " [ api ] \n"
       " email = me@example.com \n"
       " key = secret-one \n"
       " site = https://chat.example.com/ \n"
       "; another comment\n"
       "[work]\n"
       "SITE: https://work.example.com\n"
       "EMAIL: worker@example.com\n"
       "KEY: secret-two\n")
      file
    (let ((profiles (zulip-auth-read-profiles file)))
      (should (= (length profiles) 2))
      (pcase-let ((`(,personal ,work) profiles))
        (should (equal (zulip-auth-profile-name personal) "api"))
        (should (equal (zulip-auth-profile-server personal)
                       "https://chat.example.com/"))
        (should (equal (zulip-auth-profile-email personal) "me@example.com"))
        (should (equal (zulip-auth-profile-api-key personal) "secret-one"))
        (should (equal (zulip-auth-profile-name work) "work"))
        (should (equal (zulip-auth-profile-server work)
                       "https://work.example.com"))))))

(ert-deftest zulip-auth-skips-incomplete-sections-without-value-leakage ()
  (zulip-auth-test--with-file
      (concat
       "[missing-key]\nemail=a@example.com\nsite=https://a.example\n"
       "[complete]\nemail=b@example.com\nkey=b-key\nsite=https://b.example\n"
       "[empty]\nemail=c@example.com\nkey=   \nsite=https://c.example\n")
      file
    (let ((profiles (zulip-auth-read-profiles file)))
      (should (= (length profiles) 1))
      (should (equal (zulip-auth-profile-name (car profiles)) "complete"))
      (should (equal (zulip-auth-profile-api-key (car profiles)) "b-key")))))

(ert-deftest zulip-auth-missing-or-disabled-file-is-empty ()
  (let ((zulip-rc-file nil))
    (should-not (zulip-auth-read-profiles)))
  (should-not
   (zulip-auth-read-profiles
    (expand-file-name "definitely-missing-zuliprc" temporary-file-directory))))

(ert-deftest zulip-auth-selection-never-displays-api-key ()
  (let* ((first (zulip-auth-profile-create
                 :name "one" :server "https://one.example"
                 :email "one@example.com" :api-key "DO-NOT-DISPLAY-ONE"))
         (second (zulip-auth-profile-create
                  :name "two" :server "https://two.example"
                  :email "two@example.com" :api-key "DO-NOT-DISPLAY-TWO"))
         seen-collection)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _)
                 (setq seen-collection collection)
                 (caar (last collection)))))
      (should (eq (zulip-auth-select-profile (list first second)) second)))
    (let ((display (format "%S" (mapcar #'car seen-collection))))
      (should-not (string-match-p "DO-NOT-DISPLAY" display))
      (should (string-match-p "two@example.com" display)))))

(ert-deftest zulip-auth-single-profile-selection-does-not-prompt ()
  (let ((profile (zulip-auth-profile-create
                  :name "api" :server "https://chat.example"
                  :email "me@example.com" :api-key "secret")))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) (ert-fail "Unexpected account prompt"))))
      (should (eq (zulip-auth-select-profile (list profile)) profile)))))

(ert-deftest zulip-connect-from-zuliprc-passes-selected-credentials ()
  (zulip-auth-test--with-file
      "[api]\nsite=https://chat.example\nemail=me@example.com\nkey=private-key\n"
      file
    (let (arguments)
      (cl-letf (((symbol-function 'zulip-connect)
                 (lambda (&rest values)
                   (setq arguments values)
                   'connected)))
        (should (eq (zulip-connect-from-zuliprc file) 'connected))
        (should (equal arguments
                       '("https://chat.example" "me@example.com" "private-key")))))))

(ert-deftest zulip-without-explicit-credentials-prefers-zuliprc ()
  (zulip-auth-test--with-file
      "[api]\nsite=https://chat.example\nemail=me@example.com\nkey=private-key\n"
      file
    (let ((zulip-rc-file file)
          connected
          opened)
      (cl-letf (((symbol-function 'zulip--connect-or-reuse)
                 (lambda (&rest values)
                   (setq connected values)
                   'account))
                ((symbol-function 'zulip-root-open)
                 (lambda (account)
                   (setq opened account)
                   'root)))
        (should (eq (zulip) 'root))
        (should (eq opened 'account))
        (should (equal connected
                       '("https://chat.example" "me@example.com" "private-key")))))))

(ert-deftest zulip-explicit-credentials-bypass-zuliprc ()
  (let (connected)
    (cl-letf (((symbol-function 'zulip--connect-or-reuse)
               (lambda (&rest values)
                 (setq connected values)
                 'account))
              ((symbol-function 'zulip-auth-read-profiles)
               (lambda (&rest _)
                 (ert-fail "Explicit credentials must bypass zuliprc")))
              ((symbol-function 'zulip-root-open) #'identity))
      (should (eq (zulip "https://manual.example" "me@example.com" "key")
                  'account))
      (should (equal connected
                     '("https://manual.example" "me@example.com" "key"))))))

(ert-deftest zulip-combined-feed-without-credentials-prefers-zuliprc ()
  (let (opened-account
        opened-narrow)
    (cl-letf (((symbol-function 'zulip--connect-default)
               (lambda () 'default-account))
              ((symbol-function 'zulip--connect-or-reuse)
               (lambda (&rest _)
                 (ert-fail "No-argument combined feed must use zuliprc")))
              ((symbol-function 'zulip-feed-open)
               (lambda (account narrow)
                 (setq opened-account account
                       opened-narrow narrow)
                 'feed)))
      (should (eq (zulip-open-combined-feed) 'feed))
      (should (eq opened-account 'default-account))
      (should (eq (zulip-narrow-kind opened-narrow) 'all)))))

(provide 'zulip-auth-test)

;;; zulip-auth-test.el ends here
