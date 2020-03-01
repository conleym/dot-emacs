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


(use-package company
  ;; https://github.com/company-mode/company-mode
  ;; Autocompletion
  :after (tern) ;; company-tern needs tern, so load that first.
  :config
  (global-company-mode))


(use-package company-auctex
  ;; https://github.com/alexeyr/company-auctex
  ;; auctex completion
  :after (company))


(use-package company-tern
  ;; https://github.com/proofit404/company-tern
  ;; Company mode backend for tern (javascript completion).
  :after (company tern)
  :defer t
  :init
  (add-to-list 'company-backends #'company-tern))


(use-package flycheck
  ;; http://flycheck.readthedocs.org/en/latest/
  ;; On the fly syntax checking. Easier to configure and use than flymake.
  ;;  
  :delight
  :config
  (use-package flycheck-pos-tip
    :config (flycheck-pos-tip-mode))
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)) ;; Checkdoc just annoys me.
  (setq-default flycheck-emacs-lisp-load-path 'inherit)
  (setq-default flycheck-display-errors-delay 1)
  (global-flycheck-mode))


(use-package manage-minor-mode
  ;; https://github.com/ShingoFukuyama/manage-minor-mode
  ;; List, activate and deactivate minor modes easily.
)


(use-package ns-auto-titlebar
  ;; https://github.com/purcell/ns-auto-titlebar
  :if (conleym:is-darwin)
  :config (ns-auto-titlebar-mode))


(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  ;; Disable line numbers. I don't want them. Also make sure linum mode is off,
  ;; because it can break this mode:
  ;; https://github.com/politza/pdf-tools#linum-mode
  :hook (pdf-view-mode . conleym:disable-line-numbers)
  :config
  (pdf-tools-install :no-query))


(use-package prettier-js
  ;; https://github.com/prettier/prettier-emacs
  ;; use prettier to format javascript code.
  :ensure-system-package (prettier . "npm install -g prettier")
  :hook (js-mode . prettier-js-mode)
  :init
  (setq prettier-js-args
        '("--bracket-spacing" "false"
          "--single-quote" "true"
          "--print-width" "80"
          "--jsx-bracket-same-line")))


(use-package reveal-in-osx-finder
  ;; https://github.com/kaz-yos/reveal-in-osx-finder
  :if (conleym:is-mac-app))


(use-package smooth-scroll
  :delight
  :config
  (smooth-scroll-mode 1)
  (setq smooth-scroll/vscroll-step-size 3
        smooth-scroll/hscroll-step-size 1))


(use-package tern
  ;; http://ternjs.net/doc/manual.html#emacs
  ;; Code completion and other useful things for javascript.
  ;;
  :after (js-mode)
  :delight
  :ensure-system-package (tern . "npm i -g tern")
  :hook (js-mode . (lambda() (tern-mode t))))


(use-package tex-site
  :ensure auctex
  :after (auctex-latexmk reftex company-auctex)
  :hook ((TeX-mode . TeX-source-correlate-mode)
         (LaTeX-mode . LaTeX-math-mode))
  :bind (:map TeX-mode-map
              ;; cmd-shift-click = TeX-view
              ("<S-s-mouse-1>" . TeX-view))
  :init
  (auctex-latexmk-setup)
  (company-auctex-init)
  ;; Suggestion from pdf-tools readme:
  ;; https://github.com/politza/pdf-tools#auto-revert
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  ;; I can't figure out how to shoehorn these into :hook keyword above.
  (add-hook 'TeX-mode-hook (lambda() (TeX-fold-mode t)))
  (add-hook 'LaTeX-mode-hook (lambda() (setq TeX-command-default "LatexMk")))
  :config
  (setq-default TeX-master nil)
  (setq LaTeX-math-menu-unicode t
        preview-auto-cache-preamble t
        TeX-auto-save t
        TeX-auto-untabify t
        ;; Don't ask if I want to clean. Of course I want to clean.
        TeX-clean-confirm nil
        TeX-complete-expert-commands t
        ;; Don't ask me if I want to see the errors. Of course I want to see the errors.
        TeX-error-overview-open-after-TeX-run t
        ;; Don't automatically insert braces when inserting macros.
        TeX-insert-braces nil
        TeX-parse-self t
        ;; Don't ask me if I want to save. Just save.
        TeX-save-query nil
        TeX-source-correlate-start-server t
        TeX-view-program-list '(("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")
                                (("pdf-tools" "TeX-pdf-tools-sync-view")))
        TeX-view-program-selection '((output-pdf "Skim")
                                     (output-dvi "Skim")
                                     (output-html "open"))))





(provide 'conleym-use-package)
