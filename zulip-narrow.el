;;; zulip-narrow.el --- Canonical Zulip message narrows -*- lexical-binding: t; -*-

;;; Commentary:

;; Zulip uses JSON arrays of operator/operand objects for message queries.
;; Keep that wire representation at the API boundary and use small, equal-able
;; keys everywhere else.  A narrow is immutable by convention after creation.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)

(cl-defstruct (zulip-narrow
               (:constructor zulip-narrow--create))
  "One canonical Zulip feed selection."
  kind
  channel-operand
  topic-name
  recipient-ids
  display-title
  search-query
  key)

(defun zulip-narrow--channel-p (channel)
  "Return non-nil when CHANNEL is a valid Zulip channel operand."
  (or (and (integerp channel) (> channel 0))
      (and (stringp channel) (not (string-empty-p channel)))))

(defun zulip-narrow--normalize-recipients (recipients)
  "Return a sorted, duplicate-free list of RECIPIENTS."
  (let ((recipients
         (cond
          ((integerp recipients) (list recipients))
          ((vectorp recipients) (append recipients nil))
          ((listp recipients) (copy-sequence recipients))
          (t (error "Invalid Zulip direct-message recipients: %S" recipients)))))
    (unless (and recipients
                 (seq-every-p (lambda (id) (and (integerp id) (> id 0)))
                              recipients))
      (error "Zulip direct-message recipients must be positive integer IDs"))
    (sort (delete-dups recipients) #'<)))

(defun zulip-narrow-all ()
  "Return the canonical combined-feed narrow."
  ;; State uses nil as the registered combined-feed index, matching the empty
  ;; API narrow.  Nil is an opaque, stable `equal' key just like any list.
  (zulip-narrow--create :kind 'all :key nil))

(defun zulip-narrow-channel (channel)
  "Return a narrow for Zulip CHANNEL.

CHANNEL may be a positive channel ID or a non-empty channel name."
  (unless (zulip-narrow--channel-p channel)
    (error "Invalid Zulip channel operand: %S" channel))
  (zulip-narrow--create
   :kind 'channel :channel-operand channel
   :key (list (cons 'channel channel))))

(defun zulip-narrow-topic (channel topic)
  "Return a narrow for TOPIC in Zulip CHANNEL."
  (unless (zulip-narrow--channel-p channel)
    (error "Invalid Zulip channel operand: %S" channel))
  ;; Empty topic names are valid on current Zulip servers.
  (unless (stringp topic)
    (error "Invalid Zulip topic operand: %S" topic))
  (zulip-narrow--create
   :kind 'topic :channel-operand channel :topic-name topic
   :key (list (cons 'channel channel) (cons 'topic topic))))

(defun zulip-narrow-direct (recipients &optional display-title)
  "Return a direct-message narrow for RECIPIENTS.

RECIPIENTS is a user ID, list of user IDs, or vector of user IDs.  The
authenticated user's own ID is not included in this operand.  DISPLAY-TITLE,
when non-nil, supplies a human-readable participant label without changing
the stable narrow identity."
  (let ((recipients (zulip-narrow--normalize-recipients recipients)))
    (zulip-narrow--create
     :kind 'direct :recipient-ids recipients :display-title display-title
     :key (list (cons 'dm recipients)))))

(defun zulip-narrow-mentioned ()
  "Return the canonical feed of messages mentioning the current user."
  (zulip-narrow--create
   :kind 'mentioned :key (list (cons 'is "mentioned"))))

(defun zulip-narrow-starred ()
  "Return the canonical feed of messages starred by the current user."
  (zulip-narrow--create
   :kind 'starred :key (list (cons 'is "starred"))))

(defun zulip-narrow-search (query)
  "Return a server text-search narrow for non-empty QUERY."
  (unless (and (stringp query)
               (not (string-empty-p (string-trim query))))
    (error "Invalid Zulip search query: %S" query))
  (setq query (string-trim query))
  (zulip-narrow--create
   :kind 'search :search-query query
   :key (list (cons 'search query))))

(defun zulip-narrow-api-operators (narrow)
  "Return Zulip API operator objects for NARROW.

The return value is a list of alists suitable for `json-serialize'."
  (unless (zulip-narrow-p narrow)
    (error "Invalid Zulip narrow: %S" narrow))
  (pcase (zulip-narrow-kind narrow)
    ('all nil)
    ('channel
     (list (list (cons 'operator "channel")
                 (cons 'operand (zulip-narrow-channel-operand narrow)))))
    ('topic
     (list (list (cons 'operator "channel")
                 (cons 'operand (zulip-narrow-channel-operand narrow)))
           (list (cons 'operator "topic")
                 (cons 'operand (zulip-narrow-topic-name narrow)))))
    ('direct
     (list (list (cons 'operator "dm")
                 (cons 'operand
                       (vconcat (zulip-narrow-recipient-ids narrow))))))
    ('mentioned
     (list (list (cons 'operator "is") (cons 'operand "mentioned"))))
    ('starred
     (list (list (cons 'operator "is") (cons 'operand "starred"))))
    ('search
     (list (list (cons 'operator "search")
                 (cons 'operand (zulip-narrow-search-query narrow)))))
    (_ (error "Unsupported Zulip narrow kind: %S"
              (zulip-narrow-kind narrow)))))

(defun zulip-narrow-api-json (narrow)
  "Return the JSON-encoded Zulip API narrow for NARROW."
  ;; Native `json-serialize' is optional in Emacs 27 builds.  Narrows are tiny
  ;; vectors/alists, so the portable encoder is both sufficient and reliable.
  (let ((json-false :json-false)
        (json-null nil))
    (json-encode (vconcat (zulip-narrow-api-operators narrow)))))

(defun zulip-narrow-send-target (narrow)
  "Return an unambiguous send target plist for NARROW, or nil.

Only topic and direct-message narrows identify a complete Zulip destination.
The result has `:type', `:to', and `:topic' members expected by the send API."
  (pcase (and (zulip-narrow-p narrow) (zulip-narrow-kind narrow))
    ('topic
     (list :type "stream"
           :to (zulip-narrow-channel-operand narrow)
           :topic (zulip-narrow-topic-name narrow)))
    ('direct
     (list :type "direct"
           :to (vconcat (zulip-narrow-recipient-ids narrow))
           :topic nil))
    (_ nil)))

(defun zulip-narrow-title (narrow)
  "Return a compact human-readable title for NARROW."
  (pcase (zulip-narrow-kind narrow)
    ('all "All messages")
    ('channel (format "Channel %s" (zulip-narrow-channel-operand narrow)))
    ('topic (format "%s > %s"
                    (zulip-narrow-channel-operand narrow)
                    (zulip-narrow-topic-name narrow)))
    ('direct (or (zulip-narrow-display-title narrow)
                 (format "DM %s"
                         (mapconcat #'number-to-string
                                    (zulip-narrow-recipient-ids narrow) ","))))
    ('mentioned "Mentions")
    ('starred "Starred messages")
    ('search (format "Search: %s" (zulip-narrow-search-query narrow)))
    (_ "Messages")))

(provide 'zulip-narrow)

;;; zulip-narrow.el ends here
