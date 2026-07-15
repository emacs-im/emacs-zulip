;;; zulip-completion.el --- Zulip composer completion -*- lexical-binding: t; -*-

;;; Commentary:

;; Appkit-backed completion for Zulip user mentions and Unicode emoji.  User
;; candidates are built lazily per account because a realm can contain tens of
;; thousands of users.  Call `zulip-completion-invalidate-account-cache' after
;; a same-size realm-user update; registration replacement and user-count
;; changes are detected automatically.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'appkit-chat-completion)
(require 'appkit-chat-emoji)
(require 'appkit-chatbuf)
(require 'zulip-runtime)
(require 'zulip-state)

(cl-defstruct
    (zulip-completion--cache-entry
     (:constructor zulip-completion--cache-entry-create))
  "Cached mention candidates for one account."
  register-data
  user-count
  candidates)

(defvar zulip-completion--account-cache
  (make-hash-table :test #'eq :weakness 'key)
  "Weak account-to-mention-candidate cache.")

(defvar-local zulip-completion--account nil
  "Zulip account supplying completion data in the current buffer.")

(defconst zulip-completion--mention-object-kind 'zulip-mention
  "Structured Appkit composer object kind used for Zulip mentions.")

(defun zulip-completion--json-true-p (value)
  "Return non-nil only when JSON VALUE represents true."
  (and value
       (not (memq value '(:false :json-false json-false false)))))

(defun zulip-completion--user-active-p (user)
  "Return non-nil when normalized USER is active or has no activity field."
  (or (not (zulip-state-object-has-key-p user 'is_active))
      (zulip-completion--json-true-p
       (zulip-state-object-get user 'is_active))))

(defun zulip-completion--nonempty-string (value)
  "Return VALUE as trimmed plain text, or nil when it is not useful text."
  (when (stringp value)
    (let ((text (string-trim (substring-no-properties value))))
      (unless (string-empty-p text)
        text))))

(defun zulip-completion--user-id (table-id user)
  "Return normalized USER identity, falling back to TABLE-ID."
  (let ((raw (or (zulip-state-object-get user 'id)
                 (zulip-state-object-get user 'user_id)
                 table-id)))
    (condition-case nil
        (zulip-state-normalize-id raw)
      (error nil))))

(defun zulip-completion--collect-user-records (users)
  "Return active, mentionable records collected from USERS."
  (let ((name-counts (make-hash-table :test #'equal))
        records)
    (when (hash-table-p users)
      (maphash
       (lambda (table-id user)
         (when (and user (zulip-completion--user-active-p user))
           (let* ((id (zulip-completion--user-id table-id user))
                  (name (zulip-completion--nonempty-string
                         (zulip-state-object-get user 'full_name)))
                  (email (zulip-completion--nonempty-string
                          (zulip-state-object-get user 'email))))
             (when (and id name)
               (let ((name-key (downcase name)))
                 (puthash name-key (1+ (gethash name-key name-counts 0))
                          name-counts)
                 (push (list :id id :name name :name-key name-key :email email)
                       records))))))
       users))
    (list
     (sort records
           (lambda (left right)
             (let ((left-name (plist-get left :name-key))
                   (right-name (plist-get right :name-key)))
               (if (equal left-name right-name)
                   (string-lessp (plist-get left :id) (plist-get right :id))
                 (string-lessp left-name right-name)))))
     name-counts)))

(defun zulip-completion--unique-label (base id collision-p seen)
  "Return a unique completion label from BASE and ID against SEEN.

COLLISION-P means all records sharing BASE should carry their stable ID."
  (let* ((seed (if collision-p (format "%s (%s)" base id) base))
         (label seed)
         (index 1))
    ;; Completion is case-insensitive by default, so uniqueness must be too.
    (while (gethash (downcase label) seen)
      (setq index (1+ index)
            label (format "%s (%s#%d)" base id index)))
    (puthash (downcase label) t seen)
    label))

(defun zulip-completion--candidate (record collision-p seen)
  "Return one Appkit mention candidate for RECORD.

COLLISION-P and SEEN determine its unique visible label."
  (let* ((id (plist-get record :id))
         (name (plist-get record :name))
         (email (plist-get record :email))
         (label (zulip-completion--unique-label
                 (concat "@" name) id collision-p seen)))
    (appkit-chat-completion-candidate-create
     :label label
     :insert (format "@**%s|%s**" name id)
     :annotation (if email
                     (format "  %s · id:%s" email id)
                   (format "  id:%s" id))
     :search-terms (delq nil (list name email id))
     ;; Keep only public realm-user data in candidate values.  In particular,
     ;; never retain the account object, whose transport contains the API key.
     :value (list :kind 'mention :user-id id :full-name name :email email))))

(defun zulip-completion--mention-object-p (object)
  "Return non-nil when OBJECT is a structured Zulip mention."
  (and (listp object)
       (eq (plist-get object :kind)
           zulip-completion--mention-object-kind)
       (stringp (plist-get object :wire))))

(defun zulip-completion--insert-mention (candidate)
  "Insert CANDIDATE as one atomic, human-readable Appkit mention object."
  (let* ((value (appkit-chat-completion-candidate-value candidate))
         (id (plist-get value :user-id))
         (name (plist-get value :full-name))
         (email (plist-get value :email)))
    (unless (and (stringp id) (stringp name))
      (error "Invalid Zulip mention candidate: %S" candidate))
    (appkit-chatbuf-input-insert
     (concat "@" name)
     :object (list :kind zulip-completion--mention-object-kind
                   :user-id id
                   :full-name name
                   :wire (format "@**%s|%s**" name id))
     :properties
     (append (list 'face 'font-lock-keyword-face)
             (when email
               (list 'help-echo (format "%s · Zulip user %s" email id)))))))

(defun zulip-completion-serialize-input (input)
  "Serialize Appkit composer INPUT into Zulip Markdown.

Plain text is preserved without text properties.  Structured mention objects
remain concise `@name' tokens in the buffer but become Zulip's stable
ID-qualified mention syntax on the wire."
  (let ((input (or input ""))
        (property appkit-chatbuf-input-object-property)
        pieces)
    (dolist (piece (appkit-chatbuf-split-by-text-property input property))
      (let ((object (and (not (string-empty-p piece))
                         (get-text-property 0 property piece))))
        (cond
         ((null object)
          (push (substring-no-properties piece) pieces))
         ((zulip-completion--mention-object-p object)
          ;; Appkit's atomic object owns one trailing boundary spacer.  Keep
          ;; that separator on the wire so following text cannot join the
          ;; mention token.
          (push (concat (plist-get object :wire) " ") pieces))
         (t
          (user-error "Unsupported Zulip composer object: %S"
                      (plist-get object :kind))))))
    (apply #'concat (nreverse pieces))))

(defun zulip-completion--build-mention-candidates (state)
  "Build sorted active-user mention candidates from STATE."
  (let* ((collected (zulip-completion--collect-user-records
                     (and (zulip-state-p state) (zulip-state-users state))))
         (records (car collected))
         (name-counts (cadr collected))
         (seen (make-hash-table :test #'equal))
         candidates)
    (dolist (record records (nreverse candidates))
      (push
       (zulip-completion--candidate
        record
        (> (gethash (plist-get record :name-key) name-counts 0) 1)
        seen)
       candidates))))

(defun zulip-completion-invalidate-account-cache (&optional account)
  "Invalidate cached mention candidates for ACCOUNT.

When ACCOUNT is nil, clear every account's completion cache.  Call this after
a realm-user update that can change a name or active flag without changing the
number of cached users."
  (interactive)
  (if account
      (remhash account zulip-completion--account-cache)
    (clrhash zulip-completion--account-cache))
  t)

(defun zulip-completion-mention-candidates (account)
  "Return cached active-user mention candidates for ACCOUNT."
  (unless (zulip-account-p account)
    (error "Zulip completion requires an account"))
  (let* ((state (zulip-account-state account))
         (users (and (zulip-state-p state) (zulip-state-users state)))
         (user-count (if (hash-table-p users) (hash-table-count users) 0))
         (register-data (and (zulip-state-p state)
                             (zulip-state-register-data state)))
         (cached (gethash account zulip-completion--account-cache)))
    (if (and (zulip-completion--cache-entry-p cached)
             (= user-count
                (zulip-completion--cache-entry-user-count cached))
             (eq register-data
                 (zulip-completion--cache-entry-register-data cached)))
        (zulip-completion--cache-entry-candidates cached)
      (let ((candidates (zulip-completion--build-mention-candidates state)))
        (puthash
         account
         (zulip-completion--cache-entry-create
          :register-data register-data
          :user-count user-count
          :candidates candidates)
         zulip-completion--account-cache)
        candidates))))

(defun zulip-completion--mention-token-at-point ()
  "Return Appkit token metadata for a Zulip mention at point."
  (when-let* ((token (appkit-chat-completion-token-bounds ?@)))
    (plist-put (copy-sequence token) :kind 'mention)))

(defun zulip-completion--emoji-token-at-point ()
  "Return Appkit token metadata for a Unicode emoji at point."
  (when-let* ((token (appkit-chat-completion-delimited-token-bounds ?:)))
    (plist-put (copy-sequence token) :kind 'emoji)))

(defun zulip-completion-token-at-point ()
  "Return Zulip composer completion token metadata at point, or nil."
  (or (zulip-completion--mention-token-at-point)
      (zulip-completion--emoji-token-at-point)))

(defun zulip-completion-mention-capf ()
  "Return CAPF data for an active Zulip user mention at point."
  (when-let* ((account zulip-completion--account)
              (token (zulip-completion--mention-token-at-point))
              (candidates (zulip-completion-mention-candidates account)))
    (appkit-chat-completion-capf
     (plist-get token :start)
     (plist-get token :end)
     candidates
     :insert-function #'zulip-completion--insert-mention
     :suffix " ")))

(defun zulip-completion-complete ()
  "Complete the Zulip mention or Unicode emoji token at point."
  (interactive)
  (let ((handled (appkit-chat-completion-complete)))
    (unless (or handled (not (called-interactively-p 'interactive)))
      (message "No Zulip completion at point"))
    handled))

(defun zulip-completion-setup (account)
  "Install mention and Unicode emoji completion for ACCOUNT in this buffer."
  (unless (zulip-account-p account)
    (error "Zulip completion requires an account"))
  (setq-local zulip-completion--account account)
  ;; Make repeated mode/setup calls idempotent while retaining unrelated CAPFs
  ;; and Appkit dispatch functions installed by other composer features.
  (setq-local completion-at-point-functions
              (cl-remove-if
               (lambda (function)
                 (memq function
                       '(zulip-completion-mention-capf
                         appkit-chat-emoji-capf)))
               completion-at-point-functions))
  (setq-local appkit-chat-completion-functions
              (delq #'appkit-chat-completion-at-point
                    appkit-chat-completion-functions))
  (appkit-chat-completion-setup
   :capf-functions '(zulip-completion-mention-capf
                     appkit-chat-emoji-capf)
   :append t)
  account)

(provide 'zulip-completion)

;;; zulip-completion.el ends here
