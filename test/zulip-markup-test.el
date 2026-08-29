;;; zulip-markup-test.el --- Zulip semantic markup tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'appkit-markup-ui)
(require 'zulip-markup)
(require 'zulip-feed)

(defun zulip-markup-test--property-at (needle property)
  "Return PROPERTY on the final character of NEEDLE in the current buffer."
  (goto-char (point-min))
  (search-forward needle)
  (get-text-property (1- (point)) property))

(defun zulip-markup-test--collect-values (document)
  "Return all provider values recursively reachable from DOCUMENT."
  (let (values)
    (cl-labels
        ((inlines
          (nodes)
          (dolist (node nodes)
            (cond
             ((appkit-markup-object-p node)
              (push (appkit-markup-object-value node) values)
              (inlines (appkit-markup-object-fallback node)))
             ((appkit-markup-link-p node)
              (inlines (appkit-markup-link-children node))))))
         (blocks
          (nodes)
          (dolist (node nodes)
            (cond
             ((appkit-markup-paragraph-p node)
              (inlines (appkit-markup-paragraph-children node)))
             ((appkit-markup-heading-p node)
              (inlines (appkit-markup-heading-children node)))
             ((appkit-markup-quote-p node)
              (blocks (appkit-markup-quote-blocks node)))
             ((appkit-markup-list-p node)
              (dolist (item (appkit-markup-list-items node))
                (blocks (appkit-markup-list-item-blocks item))))
             ((appkit-markup-object-block-p node)
              (push (appkit-markup-object-block-value node) values)
              (blocks (appkit-markup-object-block-fallback node)))))))
      (blocks (appkit-markup-document-blocks document)))
    (nreverse values)))

(ert-deftest zulip-markup-adapts-basic-format-and-safe-links ()
  (let* ((document
          (zulip-markup-parse
           (concat "<p>Hello <strong>bold</strong> <em>soft</em> "
                   "<a href=\"/help\">docs</a></p>")
           "https://chat.example.test"))
         (paragraph (car (appkit-markup-document-blocks document)))
         (children (appkit-markup-paragraph-children paragraph)))
    (should (appkit-markup-paragraph-p paragraph))
    (should (equal (appkit-markup-link-url (car (last children)))
                   "https://chat.example.test/help"))
    (with-temp-buffer
      (appkit-markup-ui-insert-document
       document :final-newline-p nil :interactive-p t
       :link-action (lambda (_url) #'ignore))
      (should (equal (buffer-string) "Hello bold soft docs"))
      (should (memq 'bold
                    (let ((face (zulip-markup-test--property-at "bold" 'face)))
                      (if (listp face) face (list face)))))
      (should (functionp
               (zulip-markup-test--property-at
                "docs" appkit-ui-action-property))))))

(ert-deftest zulip-markup-preserves-provider-semantics-as-objects ()
  (let* ((document
          (zulip-markup-parse
           (concat
            "<p><span class=\"user-mention\" data-user-id=\"31\">@Ada</span> "
            "<span class=\"user-group-mention silent\" "
            "data-user-group-id=\"17\">support</span> "
            "<a class=\"stream-topic\" data-stream-id=\"9\" "
            "href=\"/#narrow/channel/9-dev/topic/build\">#dev &gt; build</a> "
            "<time datetime=\"2026-08-30T01:00:00Z\"></time> "
            "<span class=\"emoji emoji-263a\" title=\"smile\">:smile:</span></p>"
            "<div class=\"spoiler-block\"><div class=\"spoiler-header\">"
            "<p>Header</p></div><div class=\"spoiler-content\"><p>Secret</p>"
            "</div></div>"
            "<div class=\"message_inline_image\"><a href=\"/user_uploads/a.png\" "
            "title=\"a.png\"><img alt=\"a.png\" src=\"/thumb/a.png\"></a></div>")
           "https://chat.example.test"))
         (values (zulip-markup-test--collect-values document))
         (kinds (mapcar #'zulip-markup-provider-object-kind values)))
    (dolist (kind '(user-mention group-mention topic-link timestamp emoji
                                spoiler media))
      (should (memq kind kinds)))
    (let* ((mention
            (seq-find
             (lambda (value)
               (eq (zulip-markup-provider-object-kind value) 'user-mention))
             values))
           (group
            (seq-find
             (lambda (value)
               (eq (zulip-markup-provider-object-kind value) 'group-mention))
             values)))
      (should (equal (plist-get
                      (zulip-markup-provider-object-data mention) :id)
                     "31"))
      (should (plist-get
               (zulip-markup-provider-object-data group) :silent-p)))
    ;; Generic export uses safe fallback and never prints opaque structures.
    (should (string-match-p "@Ada" (appkit-markup-plain-text document)))
    (should (string-match-p "Header" (appkit-markup-plain-text document)))
    (should-not (string-match-p "Secret" (appkit-markup-plain-text document)))))

(ert-deftest zulip-markup-copy-policy-reveals-spoiler-content ()
  (let ((html
         (concat
          "<div class=\"spoiler-block\"><div class=\"spoiler-header\">"
          "<p>Header</p></div><div class=\"spoiler-content\">"
          "<p>Secret</p></div></div>")))
    (should-not
     (string-match-p "Secret" (zulip-markup-plain-text html)))
    (should
     (equal (zulip-markup-plain-text html nil t)
            "Header\nSecret"))))

(ert-deftest zulip-markup-discards-active-content-and-unsafe-links ()
  (let ((document
         (zulip-markup-parse
          (concat "<script>bad()</script><style>also-bad</style>"
                  "<p>ok <a href=\"javascript:alert(1)\">trap</a>"
                  " <img src=\"https://tracker.test/pixel\" alt=\":smile:\"></p>"))))
    (should-not (string-match-p "bad" (appkit-markup-plain-text document)))
    (should (equal (appkit-markup-plain-text document)
                   "ok trap :smile:"))
    (let* ((paragraph (car (appkit-markup-document-blocks document)))
           (children (appkit-markup-paragraph-children paragraph)))
      (should-not (seq-some #'appkit-markup-link-p children)))))

(ert-deftest zulip-markup-has-bounded-plain-text-fallback ()
  (cl-letf (((symbol-function 'zulip-markup-libxml-available-p)
             (lambda () nil)))
    (should
     (equal
      (zulip-markup-plain-text
       (concat "<script>bad()</script><p>Hello &amp; bye<br>"
               "<strong>bold</strong></p><ul><li>one</li><li>two</li></ul>"))
      "Hello & bye\nbold\n- one\n- two"))
    (should
     (equal
      (zulip-markup-plain-text
       "<p>&#65; &#x41; &#dead; &#0; &#xD800; &#x110000;</p>")
      "A A &#dead; � � �"))))

(ert-deftest zulip-feed-row-owns-anchor-around-native-markup-properties ()
  (with-temp-buffer
    (let ((zulip-feed--account
           (zulip-account--create
            :id '("https://chat.example.test" "ada@example.test")
            :server "https://chat.example.test"
            :email "ada@example.test"))
          (row
           (appkit-chat-timeline-row-create
            :key "42"
            :payload
            '((id . "42")
              (sender_full_name . "Ada")
              (content . "<p>See <a href=\"/help\">docs</a></p>")))))
      (zulip-feed--row-printer row)
      (should (equal (get-text-property (point-min) 'zulip-message-id) "42"))
      (should (get-text-property (point-min) 'read-only))
      (should (functionp
               (zulip-markup-test--property-at
                "docs" appkit-ui-action-property)))
      (should (equal (get-text-property (1- (point)) 'zulip-message-id) "42"))
      (should (get-text-property (1- (point)) 'read-only)))))

(ert-deftest zulip-feed-channel-object-uses-native-navigation ()
  (with-temp-buffer
    (let ((zulip-feed--account
           (zulip-account--create
            :id '("https://chat.example.test" "ada@example.test")
            :server "https://chat.example.test"
            :email "ada@example.test"))
          opened)
      (cl-letf (((symbol-function 'zulip-feed-open)
                 (lambda (account narrow)
                   (setq opened (list account narrow)))))
        (appkit-markup-ui-insert-document
         (zulip-markup-parse
          "<p><a class=\"stream\" data-stream-id=\"9\" href=\"/#narrow/channel/9-dev\">#dev</a></p>"
          "https://chat.example.test")
         :final-newline-p nil
         :interactive-p t
         :link-action #'zulip-feed--markup-link-action
         :object-inserter #'zulip-feed--insert-markup-object)
        (appkit-ui-activate-at (point-min))
        (should (eq (car opened) zulip-feed--account))
        (should (eq (zulip-narrow-kind (cadr opened)) 'channel))
        (should (= (zulip-narrow-channel-operand (cadr opened)) 9))))))

(provide 'zulip-markup-test)

;;; zulip-markup-test.el ends here
