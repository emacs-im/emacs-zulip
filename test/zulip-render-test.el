;;; zulip-render-test.el --- Tests for Zulip HTML rendering -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'zulip-render)
(require 'zulip-feed)

(defun zulip-render-test--property-at (needle property)
  "Return PROPERTY on the final character of NEEDLE in the current buffer."
  (goto-char (point-min))
  (search-forward needle)
  (get-text-property (1- (point)) property))

(ert-deftest zulip-render-preserves-basic-format-and-safe-links ()
  (with-temp-buffer
    (zulip-render-insert-html
     "<p>Hello <strong>bold</strong> <em>soft</em> <a href=\"/help\">docs</a></p>"
     :base-url "https://chat.example.test")
    (should (equal (buffer-string) "Hello bold soft docs"))
    (let ((bold-face (zulip-render-test--property-at "bold" 'face))
          (italic-face (zulip-render-test--property-at "soft" 'face)))
      (should (memq 'bold (if (listp bold-face) bold-face (list bold-face))))
      (should (memq 'italic
                    (if (listp italic-face) italic-face (list italic-face)))))
    (should (equal (zulip-render-test--property-at "docs" 'shr-url)
                   "https://chat.example.test/help"))
    ;; Message ownership belongs to the outer feed row, not this renderer.
    (should-not (text-property-not-all
                 (point-min) (point-max) 'zulip-message-id nil))
    (should-not (text-property-not-all
                 (point-min) (point-max) 'read-only nil))))

(ert-deftest zulip-render-discards-active-content-and-unsafe-link-targets ()
  (with-temp-buffer
    (zulip-render-insert-html
     (concat "<script>bad()</script><style>also-bad</style>"
             "<p>ok <a href=\"javascript:alert(1)\">trap</a>"
             " <img src=\"https://tracker.test/pixel\" alt=\":smile:\"></p>"))
    (should-not (string-match-p "bad" (buffer-string)))
    (should (string-match-p "ok trap :smile:" (buffer-string)))
    (should-not (zulip-render-test--property-at "trap" 'shr-url))
    (should-not (text-property-not-all
                 (point-min) (point-max) 'image-url nil))))

(ert-deftest zulip-render-has-plain-text-fallback-with-entities ()
  (with-temp-buffer
    (cl-letf (((symbol-function 'zulip-render-libxml-available-p)
               (lambda () nil)))
      (zulip-render-insert-html
       (concat "<script>bad()</script><p>Hello &amp; bye<br>"
               "<strong>bold</strong></p><ul><li>one</li><li>two</li></ul>")))
    (should (equal (buffer-string)
                   "Hello & bye\nbold\n- one\n- two"))))

(ert-deftest zulip-render-fallback-validates-numeric-entities ()
  (with-temp-buffer
    (cl-letf (((symbol-function 'zulip-render-libxml-available-p)
               (lambda () nil)))
      (zulip-render-insert-html
       "<p>&#65; &#x41; &#dead; &#0; &#xD800; &#x110000;</p>"))
    (should (equal (buffer-string)
                   "A A &#dead; � � �"))
    (should-not (string-match-p "\0" (buffer-string)))))

(ert-deftest zulip-feed-row-owns-anchor-around-shr-properties ()
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
      (should (equal (zulip-render-test--property-at "docs" 'shr-url)
                     "https://chat.example.test/help"))
      (should (equal (get-text-property (1- (point)) 'zulip-message-id) "42"))
      (should (get-text-property (1- (point)) 'read-only)))))

(provide 'zulip-render-test)

;;; zulip-render-test.el ends here
