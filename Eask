;; -*- mode: eask; lexical-binding: t -*-

(package "org-drill"
         "2.7.0"
         "Self-testing using spaced repetition")

(website-url "https://github.com/cjennings/org-drill")
(keywords "games" "outlines" "multimedia")

(package-file "org-drill.el")

(source "gnu")
(source "nongnu")
(source "melpa")
(source "org")

(depends-on "emacs" "25.3")
(depends-on "seq" "2.14")
(depends-on "org" "9.6")
(depends-on "persist" "0.3")

(development
 (depends-on "assess")
 (depends-on "undercover")
 (depends-on "package-lint")
 (depends-on "elisp-lint")
 ;; Transitive deps that Eask doesn't auto-install:
 ;;   dash      ← undercover, elisp-lint
 ;;   m-buffer  ← assess
 ;;   shut-up   ← undercover
 (depends-on "dash")
 (depends-on "m-buffer")
 (depends-on "shut-up"))
