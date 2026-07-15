;; -*- mode: eask; lexical-binding: t -*-

(package "zulip"
         "0.1.0"
         "Appkit-based Zulip client")

(website-url "https://github.com/0WD0/emacs-zulip")
(keywords "comm")

(package-file "zulip.el")

(source "gnu")
(source 'melpa)

(depends-on "emacs" "27.1")
(depends-on "plz" "0.8")
(depends-on "transient" "0.7")
(depends-on "appkit" :file "../appkit.el")

(script "refresh-appkit" "eask -f install-file ../appkit.el")
(script "test" "eask test ert test/*.el")
(script "test-local"
        "eask -f install-file ../appkit.el && eask recompile && eask test ert test/*.el")
