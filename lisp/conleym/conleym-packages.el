;; Set up package, repositories, and use-package.  -*- lexical-binding: t -*-
;;
;; Then load all the packages, installing or upgrading as needed.
(require 'conleym-elpa)
(require 'package)



;; Ensure package archive contents are current to avoid 404s from
;; repositories due to rapidly-changing packages.
;;
;; Small variation on this
;; https://github.com/jwiegley/use-package/issues/256#issuecomment-263313693
(defun conleym:package-install-refresh-contents (&rest args)
  (package-refresh-contents)
  (advice-remove #'package-install #'conleym:package-install-refresh-contents))

(advice-add #'package-install :before #'conleym:package-install-refresh-contents)

;; use-package configuration


(setq use-package-verbose t)
(setq use-package-always-ensure t)
(setq use-package-compute-statistics t)

;; implements the :delight keyword
(use-package delight)

;; implements the :chord keyword
(use-package use-package-chords
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
          "MANPATH" "PATH" "PKG_CONFIG_PATH" "WORKON_HOME"))
  (exec-path-from-shell-initialize))


;; Add my own site-lisp directory, then load packages.
(conleym:add-lisp-dir "lisp/site-lisp")
;; use-package-always-ensure is special, so this is still a dynamic binding, as desired.
(let ((use-package-always-ensure nil))
  ;; Don't try to install anything over built in packages.
  (require 'conleym-builtin-packages))
;; Install and configure (M)ELPA packages.
(require 'conleym-elpa-packages)



(provide 'conleym-packages)
