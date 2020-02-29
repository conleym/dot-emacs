(require 'conleym-init-utils)

(use-package ns-auto-titlebar
  ;; https://github.com/purcell/ns-auto-titlebar
  :if (conleym:is-darwin)
  :config (ns-auto-titlebar-mode))


(use-package reveal-in-osx-finder
  ;; https://github.com/kaz-yos/reveal-in-osx-finder
  :if (conleym:is-mac-app))


(provide 'conleym-use-package)
