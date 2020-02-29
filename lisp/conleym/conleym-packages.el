;; Set up package, repositories, and use-package.
;; Then load all the packages, installing or upgrading as needed.

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


;; Make sure we load this before trying to configure anything that might need
;; to find executables.
(use-package exec-path-from-shell
  ;; https://github.com/purcell/exec-path-from-shell
  ;; Get environment variables from a login shell. Necessary for a reasonable
  ;;   Emacs.app setup on Mac.
  :if (conleym:is-mac-app)
  :config
  ;;  (setq exec-path-from-shell-debug t)
  (setq exec-path-from-shell-arguments '("-l"))
  (setq exec-path-from-shell-variables
        '("AWS_ACCESS_KEY_ID" "AWS_DEFAULT_PROFILE" "AWS_PROFILE" "AWS_SECRET_ACCESS_KEY"
          "MANPATH" "PATH" "PKG_CONFIG_PATH" "PYTHONPATH" "WORKON_HOME"))
  (exec-path-from-shell-initialize))


(require 'conleym-builtin-use-package)
(require 'conleym-use-package)

(provide 'conleym-packages)
