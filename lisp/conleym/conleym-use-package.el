(require 'conleym-init-utils)


(use-package 2048-game
  ;; https://bitbucket.org/zck/2048.el
  ;; It's a game. It's fun.
  :hook (2048-mode . conleym:disable-display-line-numbers-mode))


(use-package manage-minor-mode
  ;; https://github.com/ShingoFukuyama/manage-minor-mode
  ;; List, activate and deactivate minor modes easily.
)


(use-package ns-auto-titlebar
  ;; https://github.com/purcell/ns-auto-titlebar
  :if (conleym:is-darwin)
  :config (ns-auto-titlebar-mode))


(use-package reveal-in-osx-finder
  ;; https://github.com/kaz-yos/reveal-in-osx-finder
  :if (conleym:is-mac-app))


(provide 'conleym-use-package)
