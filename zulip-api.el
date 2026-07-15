;;; zulip-api.el --- Asynchronous Zulip REST wrappers -*- lexical-binding: t; -*-

;;; Commentary:

;; Small wire-level wrappers over `zulip-http-request'.  These functions never
;; coerce message IDs, local IDs, or queue IDs to numbers.  Endpoint-specific
;; serializers may validate and format their textual values at the HTTP
;; boundary when Zulip requires JSON numeric tokens.

;;; Code:

(require 'cl-lib)
(require 'zulip-http)

(declare-function zulip-account-longpoll-timeout "zulip-runtime" account)

(defconst zulip-api-default-event-types
  ["message"
   "update_message"
   "delete_message"
   "update_message_flags"
   "reaction"
   "subscription"
   "stream"
   "user_topic"
   "realm_user"]
  "Event types requested by the default emacs-zulip event queue.")

(defconst zulip-api-default-fetch-event-types
  (vconcat zulip-api-default-event-types
           ["recent_private_conversations" "realm"])
  "Initial state types fetched by the default emacs-zulip event queue.

This is a superset of `zulip-api-default-event-types' so events arriving
during registration can be applied to the initial snapshot before the server
advances `last_event_id'.  `recent_private_conversations' is initial-state
data maintained thereafter by message and delete-message events.  `realm' is
initial-only here and supplies, among other metadata, the server's recommended
event-queue long-poll timeout.")

(defconst zulip-api-default-client-capabilities
  '(("notification_settings_null" . t)
    ("bulk_message_deletion" . t)
    ("empty_topic_name" . t))
  "Wire capabilities supported by the emacs-zulip state reducer.

In particular, the reducer accepts both bulk and legacy delete-message events
and preserves empty topic names.  Do not advertise `user_list_incomplete'
until every UI lookup has a verified missing-user fallback.")

(defun zulip-api--request-with-owner
    (account method endpoint params callback owner)
  "Request ACCOUNT ENDPOINT with METHOD, PARAMS, CALLBACK, and optional OWNER."
  (if owner
      (zulip-http-request
       account method endpoint params callback :owner owner)
    (zulip-http-request account method endpoint params callback)))

(defun zulip-api--message-endpoint (message-id &optional suffix)
  "Return the message endpoint for opaque string MESSAGE-ID and SUFFIX."
  (unless (stringp message-id)
    (error "Zulip message ID must be an opaque string: %S" message-id))
  (concat "/messages/" message-id (or suffix "")))

(defun zulip-api--wire-message-id-array (message-ids)
  "Serialize MESSAGE-IDS as an exact JSON integer array for the HTTP boundary.

The domain values must remain decimal strings.  This function validates them
without numeric conversion, then emits their digits directly as JSON numeric
tokens."
  (unless (or (listp message-ids) (vectorp message-ids))
    (error "Zulip message IDs must be a list or vector: %S" message-ids))
  (concat
   "["
   (mapconcat
    (lambda (message-id)
      (unless (and (stringp message-id)
                   (string-match-p
                    "\\`\\(?:0\\|[1-9][0-9]*\\)\\'" message-id))
        (error "Invalid Zulip message ID for JSON: %S" message-id))
      message-id)
    message-ids
    ",")
   "]"))

(defun zulip-api-get-profile (account callback)
  "Fetch ACCOUNT's `/users/me' profile and call CALLBACK with its result."
  (zulip-http-request account 'get "/users/me" nil callback))

(defalias 'zulip-api-get-me #'zulip-api-get-profile)
(defalias 'zulip-api-users-me #'zulip-api-get-profile)

(defun zulip-api--fetch-event-types (event-types)
  "Return initial-state types corresponding to EVENT-TYPES.

The returned vector retains every live event type so the server can fold
registration-race events into the snapshot, and adds the initial-only direct
conversation summary and realm queue metadata used by the runtime."
  (vconcat
   (delete-dups
    (append event-types '("recent_private_conversations" "realm")))))

(defun zulip-api-register (account callback &optional event-types)
  "Register ACCOUNT's event queue and call CALLBACK with its result.

EVENT-TYPES defaults to `zulip-api-default-event-types'."
  (let* ((event-types (or event-types zulip-api-default-event-types))
         (fetch-event-types
          (if (eq event-types zulip-api-default-event-types)
              zulip-api-default-fetch-event-types
            (zulip-api--fetch-event-types event-types))))
    (zulip-http-request
     account 'post "/register"
     `(("event_types" . ,event-types)
       ("fetch_event_types" . ,fetch-event-types)
       ("client_capabilities" . ,zulip-api-default-client-capabilities)
       ("apply_markdown" . t)
       ("client_gravatar" . t))
     callback)))

(defun zulip-api-get-events
    (account queue-id last-event-id callback)
  "Long-poll ACCOUNT queue QUEUE-ID after LAST-EVENT-ID.

QUEUE-ID and LAST-EVENT-ID are sent without normalization.  CALLBACK always
receives a `zulip-api-result'."
  (zulip-http-request
   account 'get "/events"
   `(("queue_id" . ,queue-id)
     ("last_event_id" . ,last-event-id)
     ("dont_block" . :json-false))
   callback
   :timeout (and (fboundp 'zulip-account-longpoll-timeout)
                 (zulip-account-longpoll-timeout account))))

(defun zulip-api-delete-queue (account queue-id callback)
  "Delete ACCOUNT event queue QUEUE-ID and call CALLBACK.

The HTTP layer puts DELETE form parameters in the query string because the
supported plz release does not send DELETE request bodies."
  (zulip-http-request
   account 'delete "/events"
   `(("queue_id" . ,queue-id))
   callback))

(cl-defun zulip-api-get-messages
    (account narrow anchor num-before num-after callback &key owner)
  "Fetch ACCOUNT messages matching NARROW around ANCHOR.

NARROW may be a pre-encoded JSON string or a Lisp vector/list/hash-table for
the HTTP parameter layer to JSON-encode.  NUM-BEFORE and NUM-AFTER bound the
page.  CALLBACK always receives a `zulip-api-result'.  OWNER, when non-nil,
owns the HTTP request instead of ACCOUNT's Appkit application."
  (let ((params `(("num_before" . ,num-before)
                  ("num_after" . ,num-after)
                  ("apply_markdown" . t))))
    (when anchor
      (push (cons "anchor" anchor) params))
    (when narrow
      (push (cons "narrow" narrow) params))
    (zulip-api--request-with-owner
     account 'get "/messages" (nreverse params) callback owner)))

(cl-defun zulip-api-get-message
    (account message-id callback
             &key (apply-markdown t) (allow-empty-topic-name t) owner)
  "Fetch opaque string MESSAGE-ID from ACCOUNT and call CALLBACK.

APPLY-MARKDOWN defaults to non-nil because emacs-zulip's normal display path
consumes the server-rendered HTML message content.  Pass it explicitly as nil
to request raw Zulip-flavored Markdown; nil is encoded as JSON false rather
than as JSON null.  ALLOW-EMPTY-TOPIC-NAME defaults to non-nil because this
client advertises and preserves native empty topic names.  Pass it as nil for
the server's display-name substitution.  OWNER, when non-nil, owns the HTTP
request instead of ACCOUNT's Appkit application."
  (zulip-api--request-with-owner
   account 'get (zulip-api--message-endpoint message-id)
   `(("apply_markdown"
      . ,(if apply-markdown t :json-false))
     ("allow_empty_topic_name"
      . ,(if allow-empty-topic-name t :json-false)))
   callback owner))

(cl-defun zulip-api-get-topics
    (account stream-id callback &key (allow-empty-topic-name t) owner)
  "Fetch topics visible in ACCOUNT channel STREAM-ID and call CALLBACK.

STREAM-ID is an ordinary Zulip channel identifier, accepted as a positive
integer or its decimal string representation.  ALLOW-EMPTY-TOPIC-NAME
defaults to non-nil because this client preserves native empty topic names;
an explicitly nil value is encoded as JSON false.  OWNER, when non-nil, owns
the HTTP request instead of ACCOUNT's Appkit application."
  (unless (or (and (integerp stream-id) (> stream-id 0))
              (and (stringp stream-id)
                   (string-match-p "\\`[0-9]+\\'" stream-id)
                   (not (string-match-p "\\`0+\\'" stream-id))))
    (error "Invalid Zulip stream ID: %S" stream-id))
  (zulip-api--request-with-owner
   account 'get (format "/users/me/%s/topics" stream-id)
   `(("allow_empty_topic_name"
      . ,(if allow-empty-topic-name t :json-false)))
   callback owner))

(cl-defun zulip-api-send-message
    (account type to topic content callback &key local-id queue-id)
  "Send an ACCOUNT message and call CALLBACK with its result.

TYPE is `channel'/`stream' or `direct'.  TO is a channel name/ID or a direct
recipient vector.  TOPIC is nil for direct messages.  CONTENT is Markdown.
LOCAL-ID and QUEUE-ID enable Zulip local echo correlation and are transmitted
unchanged."
  (let ((params `(("type" . ,type)
                  ("to" . ,to)
                  ("content" . ,content))))
    (when topic
      (setq params (append params `(("topic" . ,topic)))))
    (when local-id
      (setq params (append params `(("local_id" . ,local-id)))))
    (when queue-id
      (setq params (append params `(("queue_id" . ,queue-id)))))
    (zulip-http-request account 'post "/messages" params callback)))

(cl-defun zulip-api-update-message-flags
    (account message-ids operation flag callback &key owner)
  "Apply OPERATION for FLAG to ACCOUNT MESSAGE-IDS and call CALLBACK.

MESSAGE-IDS is a list or vector of opaque decimal strings.  At the HTTP
boundary they are emitted as exact JSON integer tokens, as required by Zulip,
without converting them to Emacs numbers.  OPERATION is `add' or `remove'.
OWNER, when non-nil, owns the HTTP request."
  (zulip-api--request-with-owner
   account 'post "/messages/flags"
   `(("messages" . ,(zulip-api--wire-message-id-array message-ids))
     ("op" . ,operation)
     ("flag" . ,flag))
   callback owner))

(cl-defun zulip-api-update-message-flags-for-narrow
    (account narrow anchor num-before num-after operation flag callback
             &key (include-anchor t include-anchor-supplied-p) owner)
  "Apply OPERATION for FLAG within ACCOUNT NARROW and call CALLBACK.

ANCHOR is transmitted unchanged.  NUM-BEFORE and NUM-AFTER bound the update
range.  INCLUDE-ANCHOR defaults to the server default; when explicitly nil it
is encoded as JSON false.  OWNER, when non-nil, owns the HTTP request."
  (let ((params `(("anchor" . ,anchor))))
    (when include-anchor-supplied-p
      (setq params
            (append params
                    `(("include_anchor"
                       . ,(if include-anchor t :json-false))))))
    (setq params
          (append params
                  `(("num_before" . ,num-before)
                    ("num_after" . ,num-after)
                    ("narrow" . ,narrow)
                    ("op" . ,operation)
                    ("flag" . ,flag))))
    (zulip-api--request-with-owner
     account 'post "/messages/flags/narrow" params callback owner)))

(cl-defun zulip-api-update-message
    (account message-id callback
             &key
             (topic nil topic-supplied-p)
             (propagate-mode nil propagate-mode-supplied-p)
             (send-notification-to-old-thread
              nil send-notification-to-old-thread-supplied-p)
             (send-notification-to-new-thread
              nil send-notification-to-new-thread-supplied-p)
             (content nil content-supplied-p)
             (prev-content-sha256 nil prev-content-sha256-supplied-p)
             (stream-id nil stream-id-supplied-p)
             owner)
  "Edit opaque string MESSAGE-ID in ACCOUNT and call CALLBACK.

The optional form fields mirror the Zulip `PATCH /messages/{message_id}'
schema.  Explicit nil notification values are encoded as JSON false; omitted
notification keywords retain server defaults.  OWNER, when non-nil, owns the
HTTP request."
  (let (params)
    (when topic-supplied-p
      (push (cons "topic" topic) params))
    (when propagate-mode-supplied-p
      (push (cons "propagate_mode" propagate-mode) params))
    (when send-notification-to-old-thread-supplied-p
      (push (cons "send_notification_to_old_thread"
                  (if send-notification-to-old-thread t :json-false))
            params))
    (when send-notification-to-new-thread-supplied-p
      (push (cons "send_notification_to_new_thread"
                  (if send-notification-to-new-thread t :json-false))
            params))
    (when content-supplied-p
      (push (cons "content" content) params))
    (when prev-content-sha256-supplied-p
      (push (cons "prev_content_sha256" prev-content-sha256) params))
    (when stream-id-supplied-p
      (push (cons "stream_id" stream-id) params))
    (zulip-api--request-with-owner
     account 'patch (zulip-api--message-endpoint message-id)
     (nreverse params) callback owner)))

(cl-defun zulip-api-delete-message (account message-id callback &key owner)
  "Permanently delete opaque string MESSAGE-ID in ACCOUNT and call CALLBACK."
  (zulip-api--request-with-owner
   account 'delete (zulip-api--message-endpoint message-id)
   nil callback owner))

(cl-defun zulip-api-add-reaction
    (account message-id emoji-name callback
             &key emoji-code reaction-type owner)
  "Add EMOJI-NAME reaction to opaque string MESSAGE-ID and call CALLBACK.

ACCOUNT is the authenticated Zulip account.  EMOJI-CODE and REACTION-TYPE are
optional Zulip reaction identifiers.  OWNER, when non-nil, owns the HTTP
request."
  (let ((params `(("emoji_name" . ,emoji-name))))
    (when emoji-code
      (setq params (append params `(("emoji_code" . ,emoji-code)))))
    (when reaction-type
      (setq params
            (append params `(("reaction_type" . ,reaction-type)))))
    (zulip-api--request-with-owner
     account 'post
     (zulip-api--message-endpoint message-id "/reactions")
     params callback owner)))

(cl-defun zulip-api-remove-reaction
    (account message-id callback
             &key emoji-name emoji-code reaction-type owner)
  "Remove a reaction from opaque string MESSAGE-ID and call CALLBACK.

ACCOUNT is the authenticated Zulip account.  At least EMOJI-NAME or EMOJI-CODE
should identify the reaction.  REACTION-TYPE is optional.  OWNER, when non-nil,
owns the HTTP request."
  (let (params)
    (when emoji-name
      (push (cons "emoji_name" emoji-name) params))
    (when emoji-code
      (push (cons "emoji_code" emoji-code) params))
    (when reaction-type
      (push (cons "reaction_type" reaction-type) params))
    (zulip-api--request-with-owner
     account 'delete
     (zulip-api--message-endpoint message-id "/reactions")
     (nreverse params) callback owner)))

(provide 'zulip-api)

;;; zulip-api.el ends here
