;;; zulip-customize.el --- User options for emacs-zulip -*- lexical-binding: t; -*-

;;; Commentary:

;; User-facing configuration shared by the protocol and view layers.

;;; Code:

(defgroup zulip nil
  "An Appkit-based Zulip client for Emacs."
  :group 'applications
  :prefix "zulip-")

(defcustom zulip-default-server nil
  "Default Zulip realm URL, for example `https://chat.example.com'."
  :type '(choice (const :tag "Prompt every time" nil) string)
  :group 'zulip)

(defcustom zulip-default-email nil
  "Default Zulip login email.

The API key is intentionally not stored in a customization variable."
  :type '(choice (const :tag "Prompt every time" nil) string)
  :group 'zulip)

(defcustom zulip-rc-file "~/.zuliprc"
  "File from which `zulip' discovers account credentials.

The file uses INI sections.  A standard `[api]' section and additional named
sections are both accepted.  Set this option to nil to disable credential-file
discovery and retain the manual server, email, and API-key prompts."
  :type '(choice (const :tag "Disable zuliprc discovery" nil) file)
  :group 'zulip)

(defcustom zulip-event-long-poll-timeout 90
  "Seconds an event queue long-poll may wait before timing out."
  :type 'integer
  :group 'zulip)

(defcustom zulip-event-retry-delay 2
  "Seconds to wait before retrying a failed event request."
  :type 'number
  :group 'zulip)

(defcustom zulip-history-page-size 50
  "Number of messages requested by one history page operation."
  :type 'integer
  :group 'zulip)

(defcustom zulip-history-auto-load-threshold 800
  "Character distance from a feed edge that triggers history paging.

Set this to nil to disable automatic paging.  The shared Appkit history
controller still suppresses paging while another request owns the window."
  :type '(choice (const :tag "Disabled" nil) integer)
  :group 'zulip)

(defcustom zulip-root-topic-hydration-concurrency 4
  "Maximum concurrent topic metadata requests in one Zulip navigator.

Topic discovery can involve one request per subscribed channel.  Keeping this
limit modest prevents a large realm from flooding Emacs with simultaneous
HTTP process callbacks and projection work on Emacs's main execution thread."
  :type '(integer 1 *)
  :group 'zulip)

(defcustom zulip-root-visible-topics-per-channel 25
  "Maximum topic rows normally shown per channel in the navigator.

Topics with cached unread messages or mentions are always shown, even when
that makes a channel exceed this limit.  Other rows are chosen by recent
message ID, without converting Zulip's opaque decimal message IDs to numbers.
The complete topic cache remains available to topic and destination
completion.  Set this option to nil to show every known topic in the root."
  :type '(choice (const :tag "Show every topic" nil)
                 (integer :tag "Topics per channel" 0 *))
  :group 'zulip)

(defcustom zulip-root-topic-hydration-sync-delay 0.2
  "Seconds to coalesce navigator syncs after topic metadata responses.

This delay applies only to background topic hydration completions.  Initial
registration and interactive navigator commands continue to request an
immediate Appkit sync."
  :type '(number 0 *)
  :group 'zulip)

(defcustom zulip-message-compact-seconds 300
  "Maximum gap in seconds for compact consecutive messages by one sender."
  :type 'integer
  :group 'zulip)

(defcustom zulip-show-date-separators t
  "Whether feed timelines show a divider when the calendar day changes."
  :type 'boolean
  :group 'zulip)

(defcustom zulip-show-unread-divider t
  "Whether feed timelines show a divider before their first unread message."
  :type 'boolean
  :group 'zulip)

(defcustom zulip-show-avatar-images t
  "Whether graphical feed buffers fetch and display sender avatars.

Avatar acquisition is account-owned and uses Appkit's shared media cache.
Text initials remain the stable fallback while an image is unavailable."
  :type 'boolean
  :group 'zulip)

(defcustom zulip-media-cache-directory
  (locate-user-emacs-file "zulip-media-cache/")
  "Directory used for cached Zulip media needed by inline presentation."
  :type 'directory
  :group 'zulip)

(defcustom zulip-auto-mark-read t
  "Whether observing messages in a feed marks them as read.

Like telega, emacs-zulip advances the read frontier from the message under
point and from the visible end reached by deliberate scrolling.  Only unread
server messages in the current contiguous Appkit history window are sent to
Zulip; cached gaps and opaque message IDs are never guessed."
  :type 'boolean
  :group 'zulip)

(defface zulip-message-sender-face
  '((t :inherit font-lock-keyword-face :weight semi-bold))
  "Face used for another user's message heading."
  :group 'zulip)

(defface zulip-message-self-face
  '((t :inherit font-lock-variable-name-face :weight semi-bold))
  "Face used for the current user's message heading."
  :group 'zulip)

(defface zulip-message-context-face
  '((t :inherit shadow :slant italic))
  "Face used for channel and topic breadcrumbs in a feed."
  :group 'zulip)

(defface zulip-message-timestamp-face
  '((t :inherit shadow))
  "Face used for message timestamps."
  :group 'zulip)

(defface zulip-message-unread-divider-face
  '((t :inherit font-lock-warning-face :weight bold))
  "Face used for the first-unread divider."
  :group 'zulip)

(defface zulip-message-failed-face
  '((t :inherit error))
  "Face used for failed optimistic sends."
  :group 'zulip)

(provide 'zulip-customize)

;;; zulip-customize.el ends here
