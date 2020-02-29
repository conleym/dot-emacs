(require 'conleym-init-utils)


(use-package 2048-game
  ;; https://bitbucket.org/zck/2048.el
  ;; It's a game. It's fun.
  :hook (2048-mode . conleym:disable-display-line-numbers-mode))


(use-package ag
  ;; https://github.com/Wilfred/ag.el
  ;; Silver searcher front end.
  :config
  (setq ag-highlight-search 't)
  ;; TODO do I want this?
  ;; (setq ag-reuse-window 't)
  ;; (setq ag-reuse-buffers 't)
  )


(use-package all-the-icons
  ;; https://github.com/domtronn/all-the-icons.el
  ;; Fonts containing icons
  :init
  ;; https://github.com/domtronn/all-the-icons.el/issues/120#issuecomment-480342779
  (unless (member "all-the-icons" (font-family-list))
    (all-the-icons-install-fonts t)))


(use-package all-the-icons-dired
  ;; Add icons to dired-mode
  :after (all-the-icons dired)
  :hook (dired-mode . all-the-icons-dired-mode)
  :delight
  :config
  (add-to-list 'all-the-icons-icon-alist
               '("\\.dtx\\'" all-the-icons-fileicon "tex" :face all-the-icons-lblue)))


(use-package apples-mode
  ;; https://github.com/tequilasunset/apples-mode
  ;; Major mode for applescript.
  :mode "\\.applescript\\'")


(use-package auctex-latexmk
  ;; https://github.com/tom-tan/auctex-latexmk
  ;; Sets auctex up to use the latexmk command.
  :config
  (setq auctex-latexmk-inherit-TeX-PDF-mode t))


(use-package auto-compile
  ;; https://github.com/tarsius/auto-compile
  ;; Automatically byte (re)compile elisp files.
  :hook (emacs-lisp-mode . auto-compile-mode)
  :config
  (setq auto-compile-display-buffer nil
        auto-compile-mode-line-counter t
        auto-compile-on-load-mode t
        auto-compile-on-save-mode t
        auto-compile-update-autoloads t))


(use-package auto-package-update
  ;; https://github.com/rranelli/auto-package-update.el
  ;; Automatically update installed packages
  :init
  ;; update daily. remove old versions.
  (setq auto-package-update-delete-old-versions t
        auto-package-update-interval 1)
  (auto-package-update-maybe))



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
