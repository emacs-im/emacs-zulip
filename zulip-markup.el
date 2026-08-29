;;; zulip-markup.el --- Zulip HTML to Appkit semantic markup -*- lexical-binding: t; -*-

;;; Commentary:

;; Zulip message `content' is authoritative server-rendered HTML.  This module
;; parses that provider presentation directly into `appkit-markup-document'
;; values.  Generic HTML semantics become shared Appkit nodes; Zulip identities,
;; navigation, emoji, time, spoilers, and media remain opaque provider objects
;; with safe visible fallbacks.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'url-expand)
(require 'appkit-markup)

(defconst zulip-markup-max-source-length (* 1024 1024)
  "Maximum rendered message HTML accepted by the direct adapter.")

(defconst zulip-markup-max-depth 64
  "Maximum DOM nesting accepted by the direct adapter.")

(defconst zulip-markup-max-nodes 20000
  "Maximum DOM node count accepted by the direct adapter.")

(defconst zulip-markup--discard-tags
  '(script style head iframe frame frameset object embed form input textarea
           select option button video audio source track canvas svg math link
           meta base template noscript)
  "HTML elements whose complete subtrees are not message content.")

(defconst zulip-markup--block-tags
  '(address article aside blockquote div dl fieldset figure figcaption footer
            h1 h2 h3 h4 h5 h6 header hr li main nav ol p pre section table ul)
  "Elements that establish block boundaries in rendered Zulip HTML.")

(cl-defstruct (zulip-markup-provider-object
               (:constructor zulip-markup-provider-object-create)
               (:copier nil))
  "One client-owned semantic value embedded in Appkit markup."
  kind data)

(defvar zulip-markup--node-count 0)

(defun zulip-markup-libxml-available-p ()
  "Return non-nil when this Emacs can parse HTML with libxml."
  (and (fboundp 'libxml-parse-html-region)
       (or (not (fboundp 'libxml-available-p))
           (libxml-available-p))))

(defun zulip-markup--tick (depth)
  "Account for one DOM node at DEPTH or reject oversized input."
  (setq zulip-markup--node-count (1+ zulip-markup--node-count))
  (when (> zulip-markup--node-count zulip-markup-max-nodes)
    (error "Zulip message markup exceeds the node limit"))
  (when (> depth zulip-markup-max-depth)
    (error "Zulip message markup exceeds the nesting limit")))

(defun zulip-markup--element-p (node)
  "Return non-nil when NODE is a libxml element."
  (and (consp node) (symbolp (car node))))

(defun zulip-markup--attributes (node)
  "Return NODE's DOM attribute alist."
  (let ((candidate (and (zulip-markup--element-p node) (cadr node))))
    (if (and (listp candidate)
             (cl-every #'consp candidate))
        candidate
      nil)))

(defun zulip-markup--children (node)
  "Return NODE's DOM children."
  (if (zulip-markup--attributes node) (cddr node) (cdr node)))

(defun zulip-markup--attribute (node name)
  "Return DOM attribute NAME from NODE."
  (cdr (assq name (zulip-markup--attributes node))))

(defun zulip-markup--classes (node)
  "Return NODE's normalized class-name strings."
  (split-string (format "%s" (or (zulip-markup--attribute node 'class) ""))
                "[[:space:]]+" t))

(defun zulip-markup--class-p (node class)
  "Return non-nil when NODE carries CLASS."
  (member class (zulip-markup--classes node)))

(defun zulip-markup--clean-string (value)
  "Return VALUE as a property-free string."
  (substring-no-properties (format "%s" (or value ""))))

(defun zulip-markup--collapsed-text (text)
  "Collapse HTML whitespace in property-free TEXT."
  (replace-regexp-in-string
   "[[:space:]\r\n]+" " " (zulip-markup--clean-string text)))

(defun zulip-markup--text-content (node &optional depth)
  "Return exact descendant text of DOM NODE at DEPTH."
  (setq depth (or depth 0))
  (zulip-markup--tick depth)
  (cond
   ((stringp node) (zulip-markup--clean-string node))
   ((not (zulip-markup--element-p node)) "")
   ((memq (car node) zulip-markup--discard-tags) "")
   (t (mapconcat (lambda (child)
                   (zulip-markup--text-content child (1+ depth)))
                 (zulip-markup--children node) ""))))

(defun zulip-markup--safe-url (url base-url)
  "Return safe URL resolved against BASE-URL, or nil."
  (when (stringp url)
    (let ((url (string-trim (substring-no-properties url)))
          (case-fold-search t))
      (cond
       ((string-empty-p url) nil)
       ((string-match-p "\\`\\(?:https?\\|mailto\\):" url) url)
       ((string-match-p "\\`[[:alpha:]][[:alnum:]+.-]*:" url) nil)
       ((and (stringp base-url) (not (string-empty-p base-url)))
        (url-expand-file-name url (file-name-as-directory base-url)))
       (t url)))))

(defun zulip-markup--object (kind data fallback &optional styles)
  "Return inline provider object KIND with DATA, FALLBACK, and STYLES."
  (appkit-markup-object
   (zulip-markup-provider-object-create :kind kind :data data)
   fallback styles))

(defun zulip-markup--object-block (kind data fallback)
  "Return provider object block KIND with DATA and FALLBACK blocks."
  (appkit-markup-object-block
   (zulip-markup-provider-object-create :kind kind :data data)
   fallback))

(defun zulip-markup--add-styles (nodes styles)
  "Return inline NODES with semantic STYLES appended."
  (mapcar
   (lambda (node)
     (cond
      ((appkit-markup-text-p node)
       (appkit-markup-text
        (appkit-markup-text-text node)
        (append (appkit-markup-text-styles node) styles)))
      ((appkit-markup-object-p node)
       (appkit-markup-object
        (appkit-markup-object-value node)
        (appkit-markup-object-fallback node)
        (append (appkit-markup-object-styles node) styles)))
      (t node)))
   nodes))

(defun zulip-markup--inline-fallback-text (nodes)
  "Flatten inline NODES to styled text suitable for one link label."
  (let (result)
    (dolist (node nodes)
      (cond
       ((appkit-markup-text-p node) (push node result))
       ((appkit-markup-line-break-p node)
        (push (appkit-markup-text " ") result))
       ((appkit-markup-link-p node)
        (setq result
              (nconc (nreverse
                      (copy-sequence (appkit-markup-link-children node)))
                     result)))
       ((appkit-markup-object-p node)
        (setq result
              (nconc (nreverse
                      (zulip-markup--inline-fallback-text
                       (appkit-markup-object-fallback node)))
                     result)))))
    (nreverse result)))

(defun zulip-markup--inline-children (children base-url depth)
  "Adapt DOM CHILDREN to Appkit inline nodes using BASE-URL at DEPTH."
  (let (result)
    (dolist (child children)
      (setq result
            (nconc result
                   (zulip-markup--inline-node child base-url depth))))
    result))

(defun zulip-markup--mention-kind (node)
  "Return provider mention kind represented by NODE, or nil."
  (cond
   ((zulip-markup--class-p node "user-group-mention") 'group-mention)
   ((zulip-markup--class-p node "channel-wildcard-mention") 'wildcard-mention)
   ((zulip-markup--class-p node "user-mention") 'user-mention)
   ((zulip-markup--class-p node "topic-mention") 'wildcard-mention)
   (t nil)))

(defun zulip-markup--navigation-kind (node)
  "Return provider navigation kind represented by anchor NODE, or nil."
  (cond
   ((zulip-markup--class-p node "message-link") 'message-link)
   ((zulip-markup--class-p node "stream-topic") 'topic-link)
   ((zulip-markup--class-p node "stream") 'channel-link)
   (t nil)))

(defun zulip-markup--inline-node (node base-url depth)
  "Adapt one DOM NODE to zero or more inline nodes at DEPTH."
  (zulip-markup--tick depth)
  (cond
   ((stringp node)
    (let ((text (zulip-markup--collapsed-text node)))
      (unless (string-empty-p text)
        (list (appkit-markup-text text)))))
   ((not (zulip-markup--element-p node)) nil)
   ((memq (car node) zulip-markup--discard-tags) nil)
   ((eq (car node) 'br) (list (appkit-markup-line-break)))
   ((eq (car node) 'img)
    (let* ((emoji-p (zulip-markup--class-p node "emoji"))
           (alt (zulip-markup--clean-string
                 (or (zulip-markup--attribute node 'alt)
                     (zulip-markup--attribute node 'title)
                     (if emoji-p ":emoji:" "[image]"))))
           (src (zulip-markup--safe-url
                 (or (zulip-markup--attribute node 'data-original-src)
                     (zulip-markup--attribute node 'src))
                 base-url)))
      (list
       (zulip-markup--object
        (if emoji-p 'emoji 'image)
        (list :name (zulip-markup--clean-string
                     (zulip-markup--attribute node 'title))
              :url src)
        (list (appkit-markup-text alt))))))
   ((eq (car node) 'a)
    (let* ((children
            (zulip-markup--inline-children
             (zulip-markup--children node) base-url (1+ depth)))
           (label (zulip-markup--inline-fallback-text children))
           (url (zulip-markup--safe-url
                 (zulip-markup--attribute node 'href) base-url))
           (kind (zulip-markup--navigation-kind node)))
      (cond
       ((and kind label)
        (list
         (zulip-markup--object
          kind
          (list :url url
                :channel-id
                (zulip-markup--clean-string
                 (zulip-markup--attribute node 'data-stream-id)))
          label)))
       ((and url label) (list (appkit-markup-link url label)))
       (t label))))
   ((eq (car node) 'time)
    (let* ((fallback
            (zulip-markup--inline-children
             (zulip-markup--children node) base-url (1+ depth)))
           (datetime (zulip-markup--clean-string
                      (zulip-markup--attribute node 'datetime))))
      (list
       (zulip-markup--object
        'timestamp (list :datetime datetime)
        (or fallback
            (list (appkit-markup-text datetime)))))))
   ((and (eq (car node) 'span)
         (zulip-markup--mention-kind node))
    (let ((kind (zulip-markup--mention-kind node))
          (fallback
           (zulip-markup--inline-children
            (zulip-markup--children node) base-url (1+ depth))))
      (list
       (zulip-markup--object
        kind
        (list :id
              (zulip-markup--clean-string
               (or (zulip-markup--attribute node 'data-user-id)
                   (zulip-markup--attribute node 'data-user-group-id)))
              :silent-p (zulip-markup--class-p node "silent"))
        fallback))))
   ((and (eq (car node) 'span)
         (zulip-markup--class-p node "emoji"))
    (let ((fallback
           (zulip-markup--inline-children
            (zulip-markup--children node) base-url (1+ depth))))
      (list
       (zulip-markup--object
        'emoji
        (list :name (zulip-markup--clean-string
                     (zulip-markup--attribute node 'title)))
        fallback))))
   (t
    (let* ((children
            (zulip-markup--inline-children
             (zulip-markup--children node) base-url (1+ depth)))
           (styles
            (pcase (car node)
              ((or 'strong 'b) '(bold))
              ((or 'em 'i) '(italic))
              ('u '(underline))
              ((or 's 'strike 'del) '(strike))
              ('code '(code))
              (_ nil))))
      (if styles (zulip-markup--add-styles children styles) children)))))

(defun zulip-markup--find-descendant (node predicate &optional depth)
  "Return first descendant of NODE satisfying PREDICATE.

Reject traversal deeper than `zulip-markup-max-depth'."
  (setq depth (or depth 0))
  (when (> depth zulip-markup-max-depth)
    (error "Zulip message markup exceeds the nesting limit"))
  (when (zulip-markup--element-p node)
    (or (and (funcall predicate node) node)
        (seq-some (lambda (child)
                    (zulip-markup--find-descendant
                     child predicate (1+ depth)))
                  (zulip-markup--children node)))))

(defun zulip-markup--media-block (node base-url depth)
  "Return a media object block for DOM NODE at DEPTH."
  (ignore depth)
  (let* ((anchor
          (zulip-markup--find-descendant
           node (lambda (child) (eq (car child) 'a))))
         (image
          (zulip-markup--find-descendant
           node (lambda (child) (eq (car child) 'img))))
         (url (zulip-markup--safe-url
               (and anchor (zulip-markup--attribute anchor 'href)) base-url))
         (src (zulip-markup--safe-url
               (and image
                    (or (zulip-markup--attribute image 'data-original-src)
                        (zulip-markup--attribute image 'src)))
               base-url))
         (alt (zulip-markup--clean-string
               (or (and image (zulip-markup--attribute image 'alt))
                   (and anchor (zulip-markup--attribute anchor 'title))
                   "media")))
         (label (format "[Media: %s]" alt))
         (fallback-inline (list (appkit-markup-text label))))
    (when url
      (setq fallback-inline
            (list (appkit-markup-link url fallback-inline))))
    (list
     (zulip-markup--object-block
      'media (list :url url :preview-url src :alt alt)
      (list (appkit-markup-paragraph fallback-inline))))))

(defun zulip-markup--spoiler-block (node base-url depth)
  "Return a spoiler object block for DOM NODE at DEPTH."
  (let* ((header-node
          (seq-find (lambda (child)
                      (and (zulip-markup--element-p child)
                           (zulip-markup--class-p child "spoiler-header")))
                    (zulip-markup--children node)))
         (content-node
          (seq-find (lambda (child)
                      (and (zulip-markup--element-p child)
                           (zulip-markup--class-p child "spoiler-content")))
                    (zulip-markup--children node)))
         (header-blocks
          (and header-node
               (zulip-markup--blocks
                (zulip-markup--children header-node) base-url (1+ depth))))
         (content-blocks
          (and content-node
               (zulip-markup--blocks
                (zulip-markup--children content-node) base-url (1+ depth))))
         (header-document
          (appkit-markup-document
           (or header-blocks
               (list (appkit-markup-paragraph
                      (list (appkit-markup-text "Spoiler")))))))
         (content-document (appkit-markup-document content-blocks)))
    (list
     (zulip-markup--object-block
      'spoiler
      (list :header header-document :content content-document)
      (append (appkit-markup-document-blocks header-document)
              (list (appkit-markup-paragraph
                     (list (appkit-markup-text "[…]")))))))))

(defun zulip-markup--list-block (node base-url depth)
  "Return one semantic list block for DOM NODE at DEPTH."
  (let (items)
    (dolist (child (zulip-markup--children node))
      (when (and (zulip-markup--element-p child) (eq (car child) 'li))
        (push
         (appkit-markup-list-item
          (zulip-markup--blocks
           (zulip-markup--children child) base-url (1+ depth)))
         items)))
    (let ((start-value (zulip-markup--attribute node 'start)))
      (list
       (appkit-markup-list
        (if (eq (car node) 'ol) 'ordered 'unordered)
        (nreverse items)
        :start (and (eq (car node) 'ol)
                    (stringp start-value)
                    (string-match-p "\\`[1-9][0-9]*\\'" start-value)
                    (string-to-number start-value)))))))

(defun zulip-markup--block-node (node base-url depth)
  "Adapt block DOM NODE to zero or more Appkit blocks at DEPTH."
  (let ((tag (car node)))
    (cond
     ((memq tag zulip-markup--discard-tags) nil)
     ((memq tag '(p address figcaption))
      (list
       (appkit-markup-paragraph
        (zulip-markup--inline-children
         (zulip-markup--children node) base-url (1+ depth)))))
     ((memq tag '(h1 h2 h3 h4 h5 h6))
      (list
       (appkit-markup-heading
        (string-to-number (substring (symbol-name tag) 1))
        (zulip-markup--inline-children
         (zulip-markup--children node) base-url (1+ depth)))))
     ((eq tag 'blockquote)
      (list
       (appkit-markup-quote
        (zulip-markup--blocks
         (zulip-markup--children node) base-url (1+ depth)))))
     ((memq tag '(ul ol))
      (zulip-markup--list-block node base-url depth))
     ((or (eq tag 'pre)
          (zulip-markup--class-p node "codehilite"))
      (let* ((code-node
              (or (and (eq tag 'pre) node)
                  (zulip-markup--find-descendant
                   node (lambda (child) (eq (car child) 'pre)))))
             (language
              (or (zulip-markup--attribute node 'data-code-language)
                  (and code-node
                       (zulip-markup--attribute code-node 'data-code-language)))))
        (list
         (appkit-markup-preformatted
          (zulip-markup--text-content (or code-node node) (1+ depth))
          (and (stringp language) (not (string-empty-p language)) language)))))
     ((and (eq tag 'div) (zulip-markup--class-p node "spoiler-block"))
      (zulip-markup--spoiler-block node base-url depth))
     ((and (eq tag 'div)
           (or (zulip-markup--class-p node "message_inline_image")
               (zulip-markup--class-p node "message_embed")))
      (zulip-markup--media-block node base-url depth))
     ((eq tag 'table)
      (list
       (zulip-markup--object-block
        'table nil
        (list (appkit-markup-preformatted
               (string-trim
                (zulip-markup--text-content node (1+ depth))))))))
     ((eq tag 'hr)
      (list
       (zulip-markup--object-block
        'thematic-break nil
        (list (appkit-markup-paragraph
               (list (appkit-markup-text "────────")))))))
     (t
      (zulip-markup--blocks
       (zulip-markup--children node) base-url (1+ depth))))))

(defun zulip-markup--blocks (nodes base-url depth)
  "Adapt DOM NODES to Appkit blocks using BASE-URL at DEPTH."
  (let (blocks pending-inline)
    (cl-labels
        ((flush-inline
          ()
          (when pending-inline
            (push (appkit-markup-paragraph pending-inline) blocks)
            (setq pending-inline nil))))
      (dolist (node nodes)
        (zulip-markup--tick depth)
        (cond
         ((and (stringp node)
               (string-match-p "\\`[[:space:]\r\n]*\\'" node)) nil)
         ((and (zulip-markup--element-p node)
               (memq (car node) zulip-markup--block-tags))
          (flush-inline)
          (setq blocks
                (nconc (nreverse
                        (zulip-markup--block-node node base-url depth))
                       blocks)))
         (t
          (setq pending-inline
                (nconc pending-inline
                       (zulip-markup--inline-node node base-url depth))))))
      (flush-inline))
    (nreverse blocks)))

(defun zulip-markup--parse-html (html)
  "Parse HTML into a stable wrapped libxml DOM."
  (with-temp-buffer
    (insert "<!doctype html><html><body><div id=\"zulip-appkit-root\">")
    (insert html)
    (insert "</div></body></html>")
    (libxml-parse-html-region (point-min) (point-max))))

(defun zulip-markup--root (document)
  "Return the private wrapper element from parsed DOCUMENT."
  (zulip-markup--find-descendant
   document
   (lambda (node)
     (equal (zulip-markup--attribute node 'id) "zulip-appkit-root"))))

(defun zulip-markup--decode-numeric-entities (text)
  "Decode valid numeric entities in fallback TEXT."
  (let ((start 0))
    (while (string-match
            "&#\\(?:\\([xX]\\)\\([0-9A-Fa-f]+\\)\\|\\([0-9]+\\)\\);"
            text start)
      (let* ((hex-p (match-string 1 text))
             (digits (or (match-string 2 text) (match-string 3 text)))
             (number (string-to-number digits (if hex-p 16 10)))
             (replacement
              (if (or (= number 0)
                      (> number #x10ffff)
                      (and (<= #xd800 number) (<= number #xdfff)))
                  "�"
                (if (and (< number 32) (not (memq number '(9 10 13))))
                    "�"
                  (char-to-string number)))))
        (setq text (replace-match replacement t t text)
              start (+ (match-beginning 0) (length replacement)))))
    text))

(defun zulip-markup--decode-common-entities (text)
  "Decode conservative common entities in fallback TEXT."
  (dolist (mapping '(("&nbsp;" . " ") ("&amp;" . "&") ("&lt;" . "<")
                     ("&gt;" . ">") ("&quot;" . "\"") ("&#39;" . "'")
                     ("&apos;" . "'") ("&hellip;" . "…")
                     ("&mdash;" . "—") ("&ndash;" . "–"))
           text)
    (setq text (replace-regexp-in-string
                (regexp-quote (car mapping)) (cdr mapping) text t t))))

(defun zulip-markup--fallback-text (html)
  "Return conservative plain text when libxml cannot parse HTML."
  (let ((text (or html ""))
        (case-fold-search t))
    (setq text (replace-regexp-in-string
                "<!--\\(?:.\\|\n\\)*?-->" "" text t t))
    (dolist (tag zulip-markup--discard-tags)
      (setq text
            (replace-regexp-in-string
             (format "<%s\\(?:[[:space:]][^>]*\\)?>\\(?:.\\|\n\\)*?</%s[[:space:]]*>"
                     tag tag)
             "" text t t)))
    (setq text (replace-regexp-in-string
                "<br\\(?:[[:space:]][^>]*\\)?/?>" "\n" text t t)
          text (replace-regexp-in-string
                "<li\\(?:[[:space:]][^>]*\\)?>" "- " text t t))
    (dolist (tag zulip-markup--block-tags)
      (setq text (replace-regexp-in-string
                  (format "</%s[[:space:]]*>" tag) "\n" text t t)))
    (setq text (replace-regexp-in-string "<[^>]*>" "" text t t)
          text (zulip-markup--decode-numeric-entities text)
          text (zulip-markup--decode-common-entities text)
          text (replace-regexp-in-string "\r" "" text t t)
          text (replace-regexp-in-string "\n[[:space:]\n]*\n" "\n\n" text))
    (string-trim text)))

(defun zulip-markup--plain-document (text)
  "Return a semantic document for fallback plain TEXT."
  (appkit-markup-document
   (mapcar
    (lambda (line)
      (appkit-markup-paragraph (list (appkit-markup-text line))))
    (split-string text "\n" t))))

(defun zulip-markup-parse (html &optional base-url)
  "Return an Appkit document adapted from Zulip rendered HTML.

Resolve provider-relative URLs against BASE-URL.  Active/embed DOM subtrees are
never interpreted.  Missing libxml and parse failures produce conservative
plain-text blocks rather than a second HTML rendering path."
  (setq html (zulip-markup--clean-string html))
  (when (> (length html) zulip-markup-max-source-length)
    (error "Zulip message markup exceeds the source limit"))
  (let ((zulip-markup--node-count 0))
    (condition-case nil
        (if (not (zulip-markup-libxml-available-p))
            (zulip-markup--plain-document
             (zulip-markup--fallback-text html))
          (let* ((dom (zulip-markup--parse-html html))
                 (root (zulip-markup--root dom)))
            (unless root (error "Zulip markup wrapper was not parsed"))
            (appkit-markup-document
             (zulip-markup--blocks
              (zulip-markup--children root) base-url 0))))
      (error
       (zulip-markup--plain-document
        (zulip-markup--fallback-text html))))))

(defun zulip-markup--reveal-spoiler-blocks (blocks)
  "Return BLOCKS with spoiler objects expanded to their semantic content."
  (let (result)
    (dolist (block blocks)
      (setq
       result
       (nconc
        result
        (cond
         ((appkit-markup-quote-p block)
          (list
           (appkit-markup-quote
            (zulip-markup--reveal-spoiler-blocks
             (appkit-markup-quote-blocks block)))))
         ((appkit-markup-list-p block)
          (list
           (appkit-markup-list
            (appkit-markup-list-style block)
            (mapcar
             (lambda (item)
               (appkit-markup-list-item
                (zulip-markup--reveal-spoiler-blocks
                 (appkit-markup-list-item-blocks item))))
             (appkit-markup-list-items block))
            :start (appkit-markup-list-start block))))
         ((appkit-markup-object-block-p block)
          (let ((value (appkit-markup-object-block-value block)))
            (if (and (zulip-markup-provider-object-p value)
                     (eq (zulip-markup-provider-object-kind value) 'spoiler))
                (let* ((data (zulip-markup-provider-object-data value))
                       (header (plist-get data :header))
                       (content (plist-get data :content)))
                  (zulip-markup--reveal-spoiler-blocks
                   (append
                    (and (appkit-markup-document-p header)
                         (appkit-markup-document-blocks header))
                    (and (appkit-markup-document-p content)
                         (appkit-markup-document-blocks content)))))
              (list
               (appkit-markup-object-block
                value
                (zulip-markup--reveal-spoiler-blocks
                 (appkit-markup-object-block-fallback block)))))))
         (t (list block))))))
    result))

(defun zulip-markup-plain-text
    (html &optional base-url reveal-spoilers-p)
  "Return semantic plain text for Zulip rendered HTML and BASE-URL.

When REVEAL-SPOILERS-P is non-nil, include spoiler content for an explicit
message-copy operation.  Generic summaries and previews retain safe fallback."
  (let ((document (zulip-markup-parse html base-url)))
    (when reveal-spoilers-p
      (setq document
            (appkit-markup-document
             (zulip-markup--reveal-spoiler-blocks
              (appkit-markup-document-blocks document)))))
    (appkit-markup-plain-text document)))

(provide 'zulip-markup)

;;; zulip-markup.el ends here
