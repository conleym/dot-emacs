(require 'conleym-elpa)
(require 'package)

;; Ensure package archive contents are current to avoid 404s from
;; repositories due to rapidly-changing packages.
(package-refresh-contents)

;; Ensure req-package and its dependencies are installed.
(unless (require 'use-package "use-package" t)
  (progn
    (package-install 'use-package)))

(require 'use-package)

;; use-package configuration

(setq use-package-verbose t)
(setq use-package-always-ensure t)

;; implements the :delight keyword
(use-package delight)

;; implements the :ensure-system-package keyword. Mac only for now.
(if (conleym:is-darwin)
    (use-package use-package-ensure-system-package
      :config
      (setq system-packages-use-sudo t)
      (setq system-packages-package-manager 'port)))

;; implements the :chord keyword
(use-package use-package-chords
  :ensure t
  :config (key-chord-mode 1))


(require 'conleym-builtin-use-package)

;; (require 'conleym-req-package)
;; (require 'conleym-darwin-req-package)

(provide 'conleym-packages)
