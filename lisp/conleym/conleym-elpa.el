;;; Add more package archives, since the GNU archive's policy exludes many useful packages.
(require 'conleym-init-utils)
(require 'package)

;; customizable, but I want this to be set up even if we’re starting from scratch
;; with no customizations at all.
(setq package-user-dir (conleym:dot-dir-file "lisp/elpa/"))

(mapc (lambda(x) (add-to-list 'package-archives x))
      '(("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")))

(package-initialize)

(provide 'conleym-elpa)
