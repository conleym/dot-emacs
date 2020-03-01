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
    (all-the-icons-install-fonts t))
  :config
    (add-to-list 'all-the-icons-icon-alist
               '("\\.dtx\\'" all-the-icons-fileicon "tex" :face all-the-icons-lblue)))


(use-package all-the-icons-dired
  ;; https://github.com/jtbm37/all-the-icons-dired
  ;; Add icons to dired-mode
  :after (all-the-icons dired)
  :hook (dired-mode . all-the-icons-dired-mode)
  :delight)


(use-package all-the-icons-ibuffer
  ;; https://github.com/seagle0128/all-the-icons-ibuffer
  ;; Add icons to ibuffer.
  :config
  (setq  all-the-icons-ibuffer-human-readable-size t)
  :init (all-the-icons-ibuffer-mode 1))


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
  :delight
  :config
  (global-company-mode))


(use-package company-auctex
  ;; https://github.com/alexeyr/company-auctex
  ;; auctex completion
  :after (company))


;; TODO use this or get rid of it.
(use-package crux
  ;; https://github.com/bbatsov/crux
)


(use-package define-word
  ;; https://github.com/abo-abo/define-word
)


(use-package dockerfile-mode
  ;; https://github.com/spotify/dockerfile-mode
  :mode "Dockerfile[a-zA-Z.-]*\\'"
  :config
  (put 'dockerfile-image-name 'safe-local-variable #'stringp))


(use-package editorconfig
  ;; https://github.com/editorconfig/editorconfig-emacs
  ;; editorconfig support for emacs.
  :delight
  :config
  (editorconfig-mode 1))


(use-package eimp
  ;; http://mph-emacs-pkgs.alioth.debian.org/EimpEl.html
  ;; Image manipulation using ImageMagick, which must be installed and available
  ;;   in the $PATH.
  :hook (image-mode . eimp-mode)
  :config
  (setq eimp-enable-undo t))


(use-package elpy
  ;; https://github.com/jorgenschaefer/elpy
  ;; Python development env
  :hook (elpy-mode . flycheck-mode)
  :config
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (setq elpy-rpc-virtualenv-path (conleym:persistence-dir-file "elpy"))
  :init
  (elpy-enable))


(use-package emmet-mode
  ;; https://github.com/smihica/emmet-mode
  ;; emmet.io stuff (web editing shortcuts) for emacs.
  :delight
  :hook ((sgml-mode . emmet-mode)
         (css-mode . emmet-mode)))

         
(use-package emr
  ;; https://github.com/chrisbarrett/emacs-refactor
  ;; Refactoring library
  :hook (prog-mode . emr-initialize)
  :bind (:map prog-mode-map
              ("M-RET" . #'emr-show-refactor-menu)))


(use-package es-mode
  ;; https://github.com/dakrone/es-mode
  ;; Elasticsearch stuff.
  :defer t)


(use-package fill-column-indicator
  ;; https://github.com/alpaker/Fill-Column-Indicator
  ;; Draw a line at a given column.
  :defer t
  :hook (python-mode . conleym:fci-80-mode))


(use-package flx-ido
  ;; https://github.com/lewang/flx
  ;; Fuzzy matching for ido-mode.
  :after (ido)
  :config
  ;; disable ido's faces so we can see flx's highlighting instead
  (setq ido-use-faces nil
        flx-ido-threshold 10000)
  (flx-ido-mode t))


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


(use-package git-timemachine
  ;; https://gitlab.com/pidu/git-timemachine/
  ;; go forward and backward in git history with "n" and "p", etc.
  :defer t)


(use-package gitattributes-mode
  ;; https://github.com/magit/git-modes
  ;; .gitattributes major mode.
  :defer t)


(use-package gitconfig-mode
  ;; https://github.com/magit/git-modes
  ;; .gitconfig major mode.
  :hook (gitconfig-mode . (lambda()
                            ;; I don't want to indent with tabs. Tabs are stupid.
                            (setq indent-tabs-mode nil))))


(use-package github-browse-file
  ;; https://github.com/osener/github-browse-file
  ;; View files on github.com
  :defer t
  :config
  (setq github-browse-file-show-line-at-point t))


(use-package gitignore-mode
  ;; https://github.com/magit/git-modes
  ;; .gitignore major mode.
  :mode
  ;; My global settings.
  "\\.gitignore_global\\'")


(use-package hungry-delete
  ;; https://github.com/nflath/hungry-delete
  ;; Deletes all the whitespace at once.
  :delight
  :config
  (global-hungry-delete-mode))


(use-package ido-completing-read+
  ;; https://github.com/DarwinAwardWinner/ido-ubiquitous
  ;; Even more ido.
  :after (ido)
  :config
  (ido-ubiquitous-mode t))


(use-package json-mode
  ;; https://github.com/joshwnj/json-mode
  ;; Major mode for editing JSON.
  :mode
  ;; HTTP archives
  "\\.har\\'")


(use-package manage-minor-mode
  ;; https://github.com/ShingoFukuyama/manage-minor-mode
  ;; List, activate and deactivate minor modes easily.
)


(use-package markdown-mode
  ;; http://jblevins.org/projects/markdown-mode/
  ;; Major mode for editing markdown. Includes gfm-mode for github-flavored
  ;;   markdown. An external markdown program must be installed for preview
  ;;   functionality. I've installed `multimarkdown` with macports.
  :config
  (setq markdown-command "multimarkdown"))


(use-package ns-auto-titlebar
  ;; https://github.com/purcell/ns-auto-titlebar
  :if (conleym:is-darwin)
  :config (ns-auto-titlebar-mode))


(use-package noflet
  :config
  ;; Eliminate prompt when exiting emacs when processes are running.
  (defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
    "Prevent annoying \"Active processes exist\" query when you quit Emacs."
    (noflet ((process-list ())) ad-do-it)))


(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (custom-set-variables '(nov-save-place-file (conleym:persistence-dir-file "nov-places"))))


(use-package nyan-mode
  ;; http://nyan-mode.buildsomethingamazing.com
  ;; The most useful thing ever.
  :config
  (setq nyan-animation-frame-interval 0.1
        nyan-wavy-trail t)
  (nyan-mode)
  ;; Customizing nyan-animate-nyancat calls this, but I don't want
  ;; to use customize for packages.
  (nyan-start-animation))


(use-package paradox
  ;; https://github.com/Malabarba/paradox
  ;; Better package management, with asynchrony.
  :config
  (setq paradox-automatically-star nil
        paradox-column-width-package 36
        paradox-column-width-version 16
        paradox-column-width-download 8
        paradox-display-download-count t
        paradox-execute-asynchronously t
        paradox-lines-per-entry 2)
  ;; paradox-enable makes package-list-packages use the paradox-menu,
  ;; but it doesn't fetch star counts...
  (paradox-enable))


(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  ;; Disable line numbers. I don't want them. Also make sure linum mode is off,
  ;; because it can break this mode:
  ;; https://github.com/politza/pdf-tools#linum-mode
  :hook (pdf-view-mode . conleym:disable-line-numbers)
  :config
  (pdf-tools-install :no-query))


(use-package pip-requirements
  ;; https://github.com/Wilfred/pip-requirements.el
  ;; requirements.txt highlighting and autocompletion.
  :defer t)


(use-package prettier-js
  ;; https://github.com/prettier/prettier-emacs
  ;; use prettier to format javascript code.
  :delight
  :ensure-system-package (prettier . "npm install -g prettier")
  :hook (js-mode . prettier-js-mode)
  :init
  (setq prettier-js-args
        '("--bracket-spacing" "false"
          "--single-quote" "true"
          "--print-width" "80"
          "--jsx-bracket-same-line")))


(use-package rainbow-mode
  ;; https://julien.danjou.info/projects/emacs-packages#rainbow-mode
  ;; Show strings representing colors in the color they represent.
  :delight rainbow-mode
  :defer t
  :config
  ;; Automatically start rainbow-mode in any mode that it supports.
  ;;
  ;; Overwrought, overcomplicated, and simultaneously over-and-
  ;; underthought. Probably not necessary or desirable, it goes in
  ;; anyhow.
  (let* (
         ;; Generate a list, each element of which is the (list) value of
         ;; one of the rainbow-*-colors-major-mode-list variables. These
         ;; variables are customized
         (rainbow-modes
          (mapcar #'(lambda(color-type)
                      (eval
                       (intern-soft
                        (concat "rainbow-"
                                color-type
                                "-colors-major-mode-list"))))
                  '("ansi" "html" "latex" "r" "x")))
         ;; Flatten it.
         (rainbow-modes-flat (apply #'append rainbow-modes))
         ;; Get the hook for each mode in previous line.
         (rainbow-mode-hooks
          (mapcar #'(lambda(mode)
                      (intern (concat (symbol-name mode) "-hook")))
                  rainbow-modes-flat)))
    ;; Add rainbow-mode to each supported hook.
    (dolist (hook rainbow-mode-hooks)
      (add-hook hook #'rainbow-mode))))


(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))


(use-package restclient
  ;; https://github.com/pashky/restclient.el
  :config
  (use-package company-restclient
    ;; https://github.com/iquiw/company-restclient
    :after (company restclient)
    :init
    (add-to-list 'company-backends #'company-restclient)))


(use-package reveal-in-osx-finder
  ;; https://github.com/kaz-yos/reveal-in-osx-finder
  :if (conleym:is-mac-app))


(use-package smex
  ;; https://github.com/nonsequitur/smex
  ;; Better M-x, built on ido.
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands))
  :config
  (setq smex-history-length 200
        smex-save-file (conleym:persistence-dir-file "smex-items"))
  (smex-initialize))


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
  :delight
  :ensure-system-package (tern . "npm i -g tern")
  :hook (js-mode . (lambda() (tern-mode t)))
  :config
  (use-package company-tern
    ;; https://github.com/proofit404/company-tern
    ;; Company mode backend for tern (javascript completion).
    :defer t
    :init
    (add-to-list 'company-backends #'company-tern)))


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


(use-package tide
  ;; https://github.com/ananthakumaran/tide/
  ;; typescript support
  :hook ((typescript-mode . conleym:setup-tide-mode)
         (before-save . tide-format-before-save)))
 

(use-package web-mode
  ;; http://web-mode.org
  ;; Major mode for various web template languages.
  :after (flycheck tide)
  :mode (("\\.hbs\\'" . web-mode) ;; handlebars.js templates
         ("\\.tsx\\'" . web-mode)) ;; typescript react/jsx.
  :init
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (conleym:setup-tide-mode))))
  :config
  (setq web-mode-enable-current-element-highlight t)
  (flycheck-add-mode 'typescript-tslint 'web-mode))


(use-package xkcd
  ;; https://github.com/vibhavp/emacs-xkcd
  ;; Read XKCD in Emacs.

  ;; I want to be able to resize the images. Note that xkcd-mode must be
  ;; added to eimp-ignore-readonly-modes for that to work (it's
  ;; customized).
  :hook (xkcd-mode . eimp-mode)
  ;; Rebind keys, since eimp uses the arrows.
  :bind (:map xkcd-mode-map
              ("n" . #'xkcd-next)
              ("p" . #'xkcd-prev))
  :config
  (setq xkcd-cache-dir
        (conleym:persistence-dir-file "xkcd/"))
  ;; Avoid stupid mkdir-RET-RET message when directory doesn't exist.
  (conleym:maybe-mkdir xkcd-cache-dir)
  (setq xkcd-cache-latest (concat xkcd-cache-dir "latest")))


(use-package yaml-mode
  ;; https://github.com/yoshiki/yaml-mode
  ;; Major mode for editing YAML.
  :mode "\\.ya?ml\\'"
  :config
  (setq yaml-indent-offset tab-width))


(use-package zone-nyan
  :after (zone)
  :defer t
  :config
  (add-to-list 'zone-programs #'zone-nyan))


(provide 'conleym-elpa-packages)
