;;; zulip.el --- Appkit-based Zulip client -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (appkit "0.2.19") (plz "0.8") (transient "0.7"))
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
(require 'zulip-evil)

(defconst zulip-version "0.1.0"
  "Current emacs-zulip package version.")

;;;###autoload
(defun zulip-connect (server email api-key)
  "Connect to a Zulip SERVER as EMAIL using explicit API-KEY.

API-KEY is programmatic input for integrations; interactive entry points use
auth-source.  Return the account immediately while registration continues
asynchronously."
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
  "Return the live account for SERVER and EMAIL.

When API-KEY is nil, resolve it from auth-source only if a new connection is
required."
  (let* ((server (or server
                     (read-string "Zulip HTTPS server: "
                                  zulip-default-server)))
         (email (or email
                    (read-string "Zulip email: " zulip-default-email)))
         (account (zulip-runtime-account server email)))
    (or account
        (if api-key
            (zulip-connect server email api-key)
          (let ((resolved (zulip-auth-api-key server email)))
            (unwind-protect
                (zulip-connect server email resolved)
              (clear-string resolved)))))))

(defun zulip--connect-default ()
  "Connect the selected configured account through auth-source.

When `zulip-accounts' is empty, prompt for the HTTPS server and email, then
resolve that exact target through auth-source."
  (if-let* ((target
             (zulip-auth-select-target
              (zulip-auth-configured-targets))))
      (zulip--connect-or-reuse
       (zulip-auth-target-server target)
       (zulip-auth-target-email target)
       nil)
    (zulip--connect-or-reuse nil nil nil)))

;;;###autoload
(defun zulip (&optional server email api-key)
  "Connect to Zulip and open the account navigator.

Without explicit arguments, select a non-secret target from `zulip-accounts'
and resolve its API key through auth-source.  With no configured targets,
prompt for SERVER and EMAIL and query auth-source.  Supplying API-KEY is an
explicit programmatic credential override."
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
