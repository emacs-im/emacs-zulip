;;; zulip.el --- Appkit-based Zulip client -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (appkit "0.2.18") (plz "0.8") (transient "0.7"))
;; Keywords: comm
;; URL: https://github.com/0WD0/emacs-zulip

;;; Commentary:

;; A multi-account Zulip client built on Appkit's lifecycle, owned views,
;; invalidation, timeline, exact history, composer, completion, layout, and
;; mode-line primitives.  Zulip-owned modules provide authentication, REST and
;; event transport, canonical domain state, narrows, safe HTML rendering, and
;; an account-owned adapter for Appkit's progressive avatar cache.  Attachment,
;; embedded-media, upload, and realm-emoji adapters are not implemented yet.

;;; Code:

(require 'subr-x)
(require 'zulip-customize)
(require 'zulip-auth)
(require 'zulip-runtime)
(require 'zulip-state)
(require 'zulip-events)
(require 'zulip-feed)
(require 'zulip-root)
(require 'zulip-modes)

(defconst zulip-version "0.1.0"
  "Current emacs-zulip package version.")

;;;###autoload
(defun zulip-connect (server email api-key)
  "Connect to a Zulip SERVER as EMAIL using API-KEY.

Return the account immediately; registration continues asynchronously."
  (interactive
   (list (read-string "Zulip server: " zulip-default-server)
         (read-string "Zulip email: " zulip-default-email)
         (read-passwd "Zulip API key: ")))
  ;; Runtime creation validates credentials for both first connect and
  ;; reconnect.  A live account ignores the fresh initializer and preserves
  ;; canonical state/views until the register epoch atomically replaces them.
  (let ((account
         (zulip-runtime-create-account
          :server server :email email :api-key api-key
          :state (zulip-state-create))))
    (zulip-events-start account)
    account))

(defun zulip--connect-or-reuse (server email api-key)
  "Return the live account for SERVER and EMAIL, connecting with API-KEY."
  (let* ((server (or server
                     (read-string "Zulip server: " zulip-default-server)))
         (email (or email
                    (read-string "Zulip email: " zulip-default-email)))
         (account (zulip-runtime-account server email)))
    (unless account
      (setq account
            (zulip-connect
             server email (or api-key (read-passwd "Zulip API key: ")))))
    account))

(defun zulip--connect-default ()
  "Connect the default account selected from `zulip-rc-file' or prompts.

A single complete zuliprc profile is used without an account-selection prompt.
When the configured file has no complete profiles, retain the manual connection
flow used by `zulip--connect-or-reuse'."
  (if-let* ((profile
             (zulip-auth-select-profile (zulip-auth-read-profiles))))
      (zulip--connect-or-reuse
       (zulip-auth-profile-server profile)
       (zulip-auth-profile-email profile)
       (zulip-auth-profile-api-key profile))
    (zulip--connect-or-reuse nil nil nil)))

;;;###autoload
(defun zulip (&optional server email api-key)
  "Connect to Zulip and open the account navigator.

Without explicit credentials, prefer complete accounts discovered in
`zulip-rc-file': use one account directly or select among several.  If no
complete file account is available, prompt for SERVER, EMAIL, and API-KEY.
Supplying any explicit credential retains the manual connection behavior.  The
root opens immediately while registration continues asynchronously."
  (interactive)
  (zulip-root-open
   (if (and (null server) (null email) (null api-key))
       (zulip--connect-default)
     (zulip--connect-or-reuse server email api-key))))

;;;###autoload
(defun zulip-open-combined-feed (&optional server email api-key)
  "Connect to Zulip and explicitly open the combined message feed.

SERVER, EMAIL, and API-KEY follow `zulip'.  This command preserves direct
access to the all-messages feed now that `zulip' opens the navigator."
  (interactive)
  (zulip-feed-open
   (if (and (null server) (null email) (null api-key))
       (zulip--connect-default)
     (zulip--connect-or-reuse server email api-key))
   (zulip-narrow-all)))

(defalias 'zulip-combined-feed #'zulip-open-combined-feed)

;;;###autoload
(defun zulip-disconnect (server email)
  "Disconnect the account identified by SERVER and EMAIL."
  (interactive
   (list (read-string "Zulip server: " zulip-default-server)
         (read-string "Zulip email: " zulip-default-email)))
  (if-let* ((account (zulip-runtime-account server email)))
      (zulip-runtime-stop-account account)
    (user-error "No live Zulip account for %s" email)))

(provide 'zulip)

;;; zulip.el ends here
