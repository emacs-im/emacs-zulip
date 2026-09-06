;;; zulip-auth-test.el --- Auth-source credential tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'zulip-auth)
(require 'zulip)

(ert-deftest zulip-auth-configured-targets-normalize-origin-and-service ()
  (let ((zulip-accounts
         '((:name " Work "
            :server "https://Chat.Example.COM:443/"
            :email " me@example.com ")
           (:name "Private"
            :server "https://chat.example.com:8443"
            :email "other@example.com"))))
    (pcase-let ((`(,work ,private) (zulip-auth-configured-targets)))
      (should (equal (zulip-auth-target-name work) "Work"))
      (should (equal (zulip-auth-target-server work)
                     "https://chat.example.com"))
      (should (equal (zulip-auth-target-email work) "me@example.com"))
      (should
       (equal (zulip-auth-source-spec
               (zulip-auth-target-server work)
               (zulip-auth-target-email work))
              '(:host "chat.example.com"
                :user "me@example.com"
                :port "zulip")))
      (should
       (equal (plist-get
               (zulip-auth-source-spec
                (zulip-auth-target-server private)
                (zulip-auth-target-email private))
               :port)
              "zulip-8443")))))

(ert-deftest zulip-auth-configured-targets-reject-ambiguous-or-unsafe-input ()
  (dolist
      (accounts
       '(((:name "work" :server "http://chat.example.com"
           :email "me@example.com"))
         ((:name "work" :server "https://chat.example.com/path"
           :email "me@example.com"))
         ((:name "work" :server "https://chat.example.com?token=value"
           :email "me@example.com"))
         ((:name "work" :server "https://user@chat.example.com"
           :email "me@example.com"))
         ((:name "work" :name "duplicate"
           :server "https://chat.example.com" :email "me@example.com"))
         ((:name "work" :server "https://chat.example.com"
           :email "me@example.com" :api-key "must-not-live-here"))
         ((:name "work" :server "https://chat.example.com"
           :email "me@example.com")
          (:name "WORK" :server "https://other.example.com"
           :email "other@example.com"))
         ((:name "one" :server "https://chat.example.com"
           :email "me@example.com")
          (:name "two" :server "https://CHAT.example.com/"
           :email "ME@example.com"))))
    (let ((zulip-accounts accounts))
      (should-error (zulip-auth-configured-targets) :type 'user-error))))

(ert-deftest zulip-auth-target-selection-is-secret-free ()
  (let* ((first (zulip-auth-target--create
                 :name "one" :server "https://one.example"
                 :email "one@example.com"))
         (second (zulip-auth-target--create
                  :name "two" :server "https://two.example"
                  :email "two@example.com"))
         seen-collection)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _)
                 (setq seen-collection collection)
                 (caar (last collection)))))
      (should (eq (zulip-auth-select-target (list first second)) second)))
    (should (string-match-p "two@example.com"
                            (format "%S" (mapcar #'car seen-collection))))))

(ert-deftest zulip-auth-single-target-selection-does-not-prompt ()
  (let ((target
         (zulip-auth-target--create
          :name "work" :server "https://chat.example.com"
          :email "me@example.com")))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (&rest _) (ert-fail "Unexpected account prompt"))))
      (should (eq (zulip-auth-select-target (list target)) target)))))

(ert-deftest zulip-auth-api-key-uses-exact-locator-and-copies-secret ()
  (let ((stored-secret (copy-sequence "stored-secret"))
        seen-arguments)
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest arguments)
                 (setq seen-arguments arguments)
                 (list (list :secret (lambda () stored-secret))))))
      (let ((api-key
             (zulip-auth-api-key
              "https://chat.example.com" "me@example.com")))
        (should (equal api-key "stored-secret"))
        (should-not (eq api-key stored-secret))
        (should
         (equal seen-arguments
                '(:host "chat.example.com"
                  :user "me@example.com"
                  :port "zulip"
                  :require (:secret :port)
                  :max 1)))
        (clear-string api-key)
        (should (equal stored-secret "stored-secret"))))))

(ert-deftest zulip-auth-api-key-rejects-missing-or-invalid-secret ()
  (dolist (source '(nil ((:secret "bad\nsecret"))))
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest _) source)))
      (should-error
       (zulip-auth-api-key "https://chat.example.com" "me@example.com")
       :type 'user-error))))

(ert-deftest zulip-connect-or-reuse-retires-resolved-key-and-skips-live-account ()
  (let (passed-key passed-copy)
    (cl-letf (((symbol-function 'zulip-runtime-account)
               (lambda (&rest _) nil))
              ((symbol-function 'zulip-auth-api-key)
               (lambda (&rest _) (copy-sequence "private-key")))
              ((symbol-function 'zulip-connect)
               (lambda (_server _email api-key)
                 (setq passed-key api-key
                       passed-copy (copy-sequence api-key))
                 'connected)))
      (should
       (eq (zulip--connect-or-reuse
            "https://chat.example.com" "me@example.com" nil)
           'connected)))
    (should (equal passed-copy "private-key"))
    (should (seq-every-p #'zerop (string-to-list passed-key))))
  (cl-letf (((symbol-function 'zulip-runtime-account)
             (lambda (&rest _) 'live-account))
            ((symbol-function 'zulip-auth-api-key)
             (lambda (&rest _)
               (ert-fail "Live account must not query auth-source")))
            ((symbol-function 'zulip-connect)
             (lambda (&rest _)
               (ert-fail "Live account must not reconnect"))))
    (should
     (eq (zulip--connect-or-reuse
          "https://chat.example.com" "me@example.com" nil)
         'live-account))))

(ert-deftest zulip-default-entry-selects-configured-auth-source-target ()
  (let ((zulip-accounts
         '((:name "work" :server "https://chat.example.com"
            :email "me@example.com")))
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
      (should
       (equal connected
              '("https://chat.example.com" "me@example.com" nil))))))

(ert-deftest zulip-connect-is-programmatic-only ()
  (should (functionp #'zulip-connect))
  (should-not (commandp #'zulip-connect)))

(ert-deftest zulip-explicit-credentials-bypass-auth-source ()
  (let (connected)
    (cl-letf (((symbol-function 'zulip--connect-or-reuse)
               (lambda (&rest values)
                 (setq connected values)
                 'account))
              ((symbol-function 'zulip-auth-api-key)
               (lambda (&rest _)
                 (ert-fail "Explicit API key must bypass auth-source")))
              ((symbol-function 'zulip-root-open) #'identity))
      (should (eq (zulip "https://manual.example" "me@example.com" "key")
                  'account))
      (should
       (equal connected
              '("https://manual.example" "me@example.com" "key"))))))

(ert-deftest zulip-combined-feed-without-arguments-uses-default-target ()
  (let (opened-account
        opened-narrow)
    (cl-letf (((symbol-function 'zulip--connect-default)
               (lambda () 'default-account))
              ((symbol-function 'zulip--connect-or-reuse)
               (lambda (&rest _)
                 (ert-fail "No-argument combined feed must use default target")))
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
