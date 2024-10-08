;;; Add more package archives, since the GNU archive's policy exludes many useful packages.   -*- lexical-binding: t -*-
(require 'conleym-init-utils)
(require 'package)
(require 'gnutls)

;; customizable, but I want this to be set up even if we’re starting from scratch
;; with no customizations at all.
(setq package-user-dir (conleym:dot-dir-file "lisp/elpa/"))
(setq package-gnupghome-dir (conleym:persistence-dir-file "elpa/gnupg/"))

(mapc (lambda(x) (add-to-list 'package-archives x))
      '(("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")))

;; Set an explicit order for archive priority in case a package is available in more than one place.
;; In particular, this is true of magit, and it has caused issues in the past:
;; https://www.reddit.com/r/emacs/comments/1f6hemt/magit_issues/
(customize-set-variable 'package-archive-priorities '(("gnu"    . 99)
                                                      ("nongnu" . 10)
                                                      ("org"    . 70)
                                                      ("melpa"  . 98)))

(setq gnutls-verify-error t)

(package-initialize)

(provide 'conleym-elpa)
