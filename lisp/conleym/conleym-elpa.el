;;; Add more package archives, since the GNU archive's policy exludes many useful packages.
(require 'conleym-init-utils)
(require 'package)

; customizable, but I want this to be set up even if we’re starting from scratch
; with no customizations at all.
(setq package-user-dir (conleym:dot-dir-file "lisp/elpa/"))

(mapc (lambda(x) (add-to-list 'package-archives x))
      '(
        ("elpy" . "http://jorgenschaefer.github.io/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
;;        ("melpa-stable" . "http://stable.melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")
        ("sunrise" . "http://joseito.republika.pl/sunrise-commander/")
     ))

(package-initialize)

(provide 'conleym-elpa)
