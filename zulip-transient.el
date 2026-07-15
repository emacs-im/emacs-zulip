;;; zulip-transient.el --- Transient menus for emacs-zulip -*- lexical-binding: t; -*-

;;; Commentary:

;; Discoverable, message-at-point actions for Zulip feeds.  Timeline
;; single-key bindings remain available for frequently used navigation, while
;; this menu gathers less frequent state-changing operations in one place.

;;; Code:

(require 'transient)
(require 'zulip-feed)
(require 'zulip-state)

(defun zulip-transient--message-at-point ()
  "Return the canonical Zulip message at point without signaling."
  (ignore-errors (zulip-feed-message-at-point)))

(defun zulip-transient--message-inapt-reason ()
  "Return why server-backed message actions are unavailable at point.

The return value is nil for an authoritative server message and a human
readable reason otherwise.  In particular, optimistic `local-*' rows must not
escape into APIs that require a server message ID."
  (let ((message (zulip-transient--message-at-point)))
    (cond
     ((null message) "No Zulip message at point")
     ((not (zulip-state-server-message-id-p
            (ignore-errors (zulip-state-message-id message))))
      "Message is still local; wait for server acknowledgement")
     (t nil))))

(defun zulip-transient--retry-inapt-reason ()
  "Return why retrying a failed local message is unavailable at point."
  (let ((message (zulip-transient--message-at-point)))
    (cond
     ((null message) "No Zulip message at point")
     ((zulip-state-server-message-id-p
       (ignore-errors (zulip-state-message-id message)))
      "Server messages do not need send retry")
     ((not (zulip-state-object-get message 'failed))
      "Local message has not failed")
     (t nil))))

;; Magit-style autoload: a bare `;;;###autoload' above a
;; `transient-define-prefix' form would copy the whole form into loaddefs,
;; before `transient' itself is loaded.
;;;###autoload(autoload 'zulip-message-transient "zulip-transient" nil t)
(transient-define-prefix zulip-message-transient ()
  "Message actions for the Zulip feed message at point."
  [["Navigate"
    ("o" "Open context" zulip-feed-open-message-context
     :inapt-if zulip-transient--message-inapt-reason)
    ("t" "Open topic" zulip-feed-open-topic
     :inapt-if zulip-transient--message-inapt-reason)
    ("c" "Copy text" zulip-feed-copy-message
     :inapt-if zulip-transient--message-inapt-reason)]
   ["Status"
    ("r" "Mark read" zulip-feed-mark-read
     :inapt-if zulip-transient--message-inapt-reason)
    ("u" "Mark unread" zulip-feed-mark-unread
     :inapt-if zulip-transient--message-inapt-reason)
    ("s" "Toggle starred" zulip-feed-toggle-star
     :inapt-if zulip-transient--message-inapt-reason)]
   ["Modify"
    ("R" "Retry failed send" zulip-feed-retry-send
     :inapt-if zulip-transient--retry-inapt-reason)
    ("e" "Edit" zulip-feed-edit-message
     :inapt-if zulip-transient--message-inapt-reason)
    ("d" "Delete" zulip-feed-delete-message
     :inapt-if zulip-transient--message-inapt-reason)
    ("+" "Toggle reaction" zulip-feed-toggle-reaction
     :inapt-if zulip-transient--message-inapt-reason)]])

(provide 'zulip-transient)

;;; zulip-transient.el ends here
