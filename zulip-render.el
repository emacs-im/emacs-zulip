;;; zulip-render.el --- Safe rendering for Zulip message HTML -*- lexical-binding: t; -*-

;;; Commentary:

;; Zulip servers return message bodies as rendered HTML.  This module keeps
;; that presentation boundary separate from timeline ownership: it inserts
;; links and basic formatting, but deliberately never installs message anchor
;; or read-only properties.  Callers remain responsible for row semantics.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'shr)
(require 'subr-x)
(require 'url-util)

(defconst zulip-render--safe-tags
  '(html body div p span a strong b em i u s strike del code pre blockquote
         ul ol li br hr h1 h2 h3 h4 h5 h6 table thead tbody tfoot tr th td
         dl dt dd kbd samp var sub sup time details summary)
  "HTML elements retained before passing a document to SHR.")

(defconst zulip-render--discard-tags
  '(script style head iframe frame frameset object embed form input textarea
           select option button video audio source track canvas svg math
           link meta base template noscript)
  "HTML elements whose complete subtrees are discarded.")

(defconst zulip-render--block-tags
  '(p div li pre blockquote h1 h2 h3 h4 h5 h6 tr table ul ol dl dt dd)
  "Elements treated as line boundaries by the plain-text fallback.")

(defun zulip-render-libxml-available-p ()
  "Return non-nil when this Emacs can parse HTML with libxml."
  (and (fboundp 'libxml-parse-html-region)
       (or (not (fboundp 'libxml-available-p))
           (libxml-available-p))))

(defun zulip-render--safe-url (url base-url)
  "Return safe URL resolved against BASE-URL, or nil."
  (when (stringp url)
    (let ((url (string-trim url))
          (case-fold-search t))
      (cond
       ((string-empty-p url) nil)
       ((string-match-p "\\`\\(?:https?\\|mailto\\):" url) url)
       ((string-match-p "\\`[[:alpha:]][[:alnum:]+.-]*:" url) nil)
       ((and base-url (not (string-empty-p base-url)))
        (url-expand-file-name url
                              (file-name-as-directory base-url)))
       (t url)))))

(defun zulip-render--attribute (attributes name)
  "Return NAME from DOM ATTRIBUTES."
  (cdr (assq name attributes)))

(defun zulip-render--sanitize-attributes (tag attributes base-url)
  "Return safe ATTRIBUTES for TAG, resolving links against BASE-URL."
  (let (result)
    (when (eq tag 'a)
      (when-let* ((href (zulip-render--safe-url
                         (zulip-render--attribute attributes 'href)
                         base-url)))
        (push (cons 'href href) result))
      (when-let* ((title (zulip-render--attribute attributes 'title)))
        (push (cons 'title (format "%s" title)) result)))
    ;; Language is useful for pre/code display and carries no active content.
    (when (memq tag '(code pre))
      (when-let* ((class (zulip-render--attribute attributes 'class)))
        (when (string-match-p
               "\\`\\(?:codehilite\\|language-[[:alnum:]_+.-]+\\)\\'"
               (format "%s" class))
          (push (cons 'class (format "%s" class)) result))))
    (nreverse result)))

(defun zulip-render--sanitize-children (children base-url)
  "Return sanitized CHILDREN for BASE-URL."
  (let (result)
    (dolist (child children (nreverse result))
      (let ((sanitized (zulip-render--sanitize-node child base-url)))
        (cond
         ((null sanitized) nil)
         ((and (listp sanitized) (eq (car-safe sanitized) :splice))
          (dolist (node (cdr sanitized))
            (push node result)))
         (t (push sanitized result)))))))

(defun zulip-render--sanitize-node (node base-url)
  "Return a safe copy of DOM NODE for BASE-URL.

Unknown passive tags are replaced by their children.  Active or embedding
tags are removed with their complete subtrees."
  (cond
   ((stringp node) node)
   ((not (consp node)) nil)
   (t
    (let* ((tag (car node))
           (attributes (and (listp (cadr node)) (cadr node)))
           (children (if attributes (cddr node) (cdr node))))
      (cond
       ((memq tag zulip-render--discard-tags) nil)
       ((eq tag 'img)
        (when-let* ((alt (zulip-render--attribute attributes 'alt)))
          (format "%s" alt)))
       ((memq tag zulip-render--safe-tags)
        (cons tag
              (cons (zulip-render--sanitize-attributes
                     tag attributes base-url)
                    (zulip-render--sanitize-children children base-url))))
       (t
        (cons :splice
              (zulip-render--sanitize-children children base-url))))))))

(defun zulip-render--parse-html (html)
  "Parse HTML into a libxml DOM document."
  (with-temp-buffer
    ;; A wrapper gives fragments with several top-level nodes a stable body.
    (insert "<!doctype html><html><body><div>")
    (insert html)
    (insert "</div></body></html>")
    (libxml-parse-html-region (point-min) (point-max))))

(defun zulip-render--decode-numeric-entities (text)
  "Decode numeric HTML entities in TEXT."
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

(defun zulip-render--decode-common-entities (text)
  "Decode common named HTML entities in TEXT."
  (dolist (mapping '(("&nbsp;" . " ")
                     ("&amp;" . "&")
                     ("&lt;" . "<")
                     ("&gt;" . ">")
                     ("&quot;" . "\"")
                     ("&#39;" . "'")
                     ("&apos;" . "'")
                     ("&hellip;" . "…")
                     ("&mdash;" . "—")
                     ("&ndash;" . "–"))
           text)
    (setq text (replace-regexp-in-string
                (regexp-quote (car mapping)) (cdr mapping) text t t))))

(defun zulip-render-plain-text (html)
  "Return a conservative plain-text fallback for rendered HTML."
  (let ((text (or html ""))
        (case-fold-search t))
    (setq text
          (replace-regexp-in-string
           "<!--\\(?:.\\|\n\\)*?-->" "" text t t))
    (dolist (tag zulip-render--discard-tags)
      (setq text
            (replace-regexp-in-string
             (format "<%s\\(?:[[:space:]][^>]*\\)?>\\(?:.\\|\n\\)*?</%s[[:space:]]*>"
                     tag tag)
             "" text t t)))
    (setq text (replace-regexp-in-string "<br\\(?:[[:space:]][^>]*\\)?/?>"
                                         "\n" text t t)
          text (replace-regexp-in-string "<li\\(?:[[:space:]][^>]*\\)?>"
                                         "- " text t t))
    (dolist (tag zulip-render--block-tags)
      (setq text
            (replace-regexp-in-string
             (format "</%s[[:space:]]*>" tag) "\n" text t t)))
    (setq text (replace-regexp-in-string "<[^>]*>" "" text t t)
          text (zulip-render--decode-numeric-entities text)
          text (zulip-render--decode-common-entities text)
          text (replace-regexp-in-string "\r" "" text t t)
          text (replace-regexp-in-string "\n[[:space:]\n]*\n" "\n\n" text))
    (string-trim text)))

(defun zulip-render--trim-inserted-boundaries (start)
  "Trim horizontal/vertical whitespace around content inserted after START."
  (let ((end (point)))
    (save-excursion
      (goto-char end)
      (skip-chars-backward " \t\n\r" start)
      (delete-region (point) end)
      (goto-char start)
      (skip-chars-forward " \t\n\r")
      (delete-region start (point)))))

(cl-defun zulip-render-insert-html (html &key base-url width)
  "Insert server-rendered HTML at point and return its buffer bounds.

Links are resolved against BASE-URL.  WIDTH overrides SHR's render width.
Images and active/embed content are suppressed.  Parse failures and Emacs
builds without libxml use `zulip-render-plain-text'.  This function never sets
timeline anchor or read-only properties."
  (let ((start (point))
        (html (if (stringp html) html (format "%s" (or html "")))))
    (condition-case nil
        (if (not (zulip-render-libxml-available-p))
            (insert (zulip-render-plain-text html))
          (let* ((document (zulip-render--parse-html html))
                 (safe-document
                  (zulip-render--sanitize-node document base-url))
                 (shr-base base-url)
                 (shr-width (or width
                                (and (boundp 'fill-column) fill-column)
                                80))
                 (shr-inhibit-images t)
                 (shr-use-fonts t)
                 (shr-use-colors nil)
                 ;; Let the feed buffer perform ordinary visual-line wrapping;
                 ;; hard SHR folding is unstable when rendering off-screen.
                 (shr-fill-text nil)
                 (shr-external-rendering-functions nil))
            (shr-insert-document safe-document)))
      (error
       (delete-region start (point))
       (insert (zulip-render-plain-text html))))
    (zulip-render--trim-inserted-boundaries start)
    (cons start (point))))

(provide 'zulip-render)

;;; zulip-render.el ends here
