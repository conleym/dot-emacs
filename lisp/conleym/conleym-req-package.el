(require 'req-package)
(require 'conleym-init-utils)


(req-package 2048-game
  ;; https://bitbucket.org/zck/2048.el
  ;; It's a game. It's fun.
  :defer t)


(req-package ace-jump-mode
  ;; https://github.com/winterTTr/ace-jump-mode
  :config
  (ace-jump-mode-enable-mark-sync))


(req-package adoc-mode
  ;; https://github.com/sensorflo/adoc-mode
  ;; asciidoc mode
  :mode "\\.adoc\\'")


(req-package ag
  ;; https://github.com/Wilfred/ag.el
  ;; Silver searcher front end.
  :require (exec-path-from-shell)
  :ensure-system-package (ag . the_silver_searcher)
  :commands (ag ag-files ag-regexp ag-project ag-project-files ag-project-regexp))


(req-package all-the-icons
  ;; https://github.com/domtronn/all-the-icons.el
  ;; Fonts containing icons
  :init
  ;; https://github.com/domtronn/all-the-icons.el/issues/120#issuecomment-480342779
  (unless (member "all-the-icons" (font-family-list))
    (all-the-icons-install-fonts t)))


(req-package all-the-icons-dired
  ;; Add icons to dired-mode
  :require (all-the-icons dired)
  :delight
  :config
  (add-to-list 'all-the-icons-icon-alist
               '("\\.dtx\\'" all-the-icons-fileicon "tex" :face all-the-icons-lblue))
  :init
  (add-hook 'dired-mode-hook
            #'all-the-icons-dired-mode))


(req-package ansible-doc
  ;; https://github.com/lunaryorn/ansible-doc.el
  ;; Ansible documentation lookup with C-c ?
)


(req-package anzu
  ;; https://github.com/syohex/emacs-anzu
  ;; Shows number of matches and the number of the current match when searching.
  :delight
  :init (global-anzu-mode))


(req-package apples-mode
  ;; https://github.com/tequilasunset/apples-mode
  ;; Major mode for applescript.
  :mode "\\.applescript\\'")


(req-package auctex-latexmk
  ;; https://github.com/tom-tan/auctex-latexmk
  ;; Sets auctex up to use the latexmk command.
  :config
  (setq auctex-latexmk-inherit-TeX-PDF-mode t))


(req-package auto-compile
  ;; https://github.com/tarsius/auto-compile
  ;; Automatically byte (re)compile elisp files.
  :config
  (setq auto-compile-display-buffer nil
        auto-compile-on-load-mode t
        auto-compile-on-save-mode t
        auto-compile-update-autoloads t))


(req-package auto-package-update
  ;; https://github.com/rranelli/auto-package-update.el
  ;; Automatically update installed packages
  :init
  (setq auto-package-update-delete-old-versions t
    auto-package-update-interval 1)
  (auto-package-update-maybe))


(req-package bug-hunter
  ;; https://github.com/Malabarba/elisp-bug-hunter
  ;; Helps find bugs in init
  :defer t)


(req-package cider
  ;; https://github.com/clojure-emacs/cider
  ;; Clojure IDE.
  :require
  (clojure-mode)
  :defer t
  :init
  (add-hook 'clojure-mode-hook
            #'cider-mode)
  :config
  (setq cider-auto-mode nil ;; We take care of it ourselves with a clojure-mode hook.
        cider-auto-select-error-buffer nil ;; I don't want to automatically switch buffers on errors.
        cider-prompt-save-file-on-load nil ;; Of course I want to save it before running. Don't ask me.
        cider-repl-history-file (conleym:persistence-dir-file "cider-history")
        cider-repl-history-size 1000 ;; the default is 500
        cider-repl-pop-to-buffer-on-connect t ;; I do want to switch to the REPL buffer on connect.
        cider-repl-wrap-history t
        cider-show-error-buffer nil
        nrepl-log-messages t))


(req-package clj-refactor
  :defer t)


(req-package clojure-mode
  ;; https://github.com/clojure-emacs/clojure-mode
  ;; Major mode for clojure programming.
  :defer t
  :require
  (clj-refactor)
  :config
  (add-hook 'clojure-mode-hook (lambda()
                                 (clj-refactor-mode 1))))


(req-package company
  ;; https://github.com/company-mode/company-mode
  ;; Autocompletion
  :config
  (global-company-mode))


(req-package company-auctex)

(req-package company-restclient
  ;; https://github.com/iquiw/company-restclient
  :require (company restclient)
  :init
  (add-to-list 'company-backends #'company-restclient))

(req-package company-tern
  ;; https://github.com/proofit404/company-tern
  ;; Company mode backend for tern (javascript completion).
  :require (company tern)
  :defer t
  :init
  (add-to-list 'company-backends #'company-tern))


(req-package crux
  ;; https://github.com/bbatsov/crux
)


(req-package define-word)


(req-package dired-imenu
  ;; https://github.com/DamienCassou/dired-imenu
  ;; Fill imenu with the list of files in the current directory when in dired
  ;;   mode.
  )

(req-package dockerfile-mode
  ;;
  ;; Edit Dockerfiles.
  :mode "Dockerfile[a-zA-Z.-]*\\'"
  :config
  (put 'dockerfile-image-name 'safe-local-variable #'stringp))


(req-package editorconfig
  ;; https://github.com/editorconfig/editorconfig-emacs
  ;; editorconfig support for emacs.
  :delight
  :config
  (editorconfig-mode 1))


(req-package eimp
  ;; http://mph-emacs-pkgs.alioth.debian.org/EimpEl.html
  ;; Image manipulation using ImageMagick, which must be installed and available
  ;;   in the $PATH.
  :ensure-system-package (mogrify . ImageMagick)
  :require
  ;; Need $PATH to find ImageMagick tools.
  (exec-path-from-shell)
  :init
  (add-hook 'image-mode-hook
            #'eimp-mode)
  :config
  (setq eimp-enable-undo t))


(req-package elpy
  ;; https://github.com/jorgenschaefer/elpy
  ;; Python development env
  :require (exec-path-from-shell flycheck)
  :config
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook #'flycheck-mode)
  (setq elpy-rpc-virtualenv-path (conleym:persistence-dir-file "elpy"))
  :init
  (elpy-enable))


(req-package emmet-mode
  ;; https://github.com/smihica/emmet-mode
  ;; emmet.io stuff (web editing shortcuts) for emacs.
  :defer t
  :delight
  :init
  (conleym:add-function-to-hooks #'emmet-mode
                                 'sgml-mode-hook
                                 'css-mode-hook))


(req-package emr
  ;; https://github.com/chrisbarrett/emacs-refactor
  ;; Refactoring library
  :init
  (define-key prog-mode-map (kbd "M-RET") 'emr-show-refactor-menu)
  (add-hook 'prog-mode-hook #'emr-initialize))


(req-package es-mode
  ;; https://github.com/dakrone/es-mode
  ;; Elasticsearch stuff.
  :defer t)


(req-package ess
  ;; http://ess.r-project.org/
  ;; S/R support.
  :defer t)


(req-package exec-path-from-shell
  ;; https://github.com/purcell/exec-path-from-shell
  ;; Get environment variables from a login shell. Necessary for a reasonable
  ;;   Emacs.app setup on Mac.
  :if (conleym:is-mac-app)
  ;; Seems this must be :init rather than :config. Otherwise eimp can't find
  ;; mogrify and complains.
  :init
  ;;  (setq exec-path-from-shell-debug t)
  (setq exec-path-from-shell-arguments '("-l"))
  (setq exec-path-from-shell-variables
        '("AWS_ACCESS_KEY_ID" "AWS_DEFAULT_PROFILE" "AWS_PROFILE" "AWS_SECRET_ACCESS_KEY"
          "MANPATH" "PATH" "PKG_CONFIG_PATH" "PYTHONPATH" "WORKON_HOME"))
  (exec-path-from-shell-initialize))


(req-package fill-column-indicator
  ;; https://github.com/alpaker/Fill-Column-Indicator
  ;; Draw a line at a given column.
  :defer t
  :init
  (defun conleym:fci-80-mode ()
    (setq fci-rule-column 80) ;; becomes local when set.
    (fci-mode 1))
  (conleym:add-function-to-hooks #'conleym:fci-80-mode
                                 'python-mode-hook))


(req-package format-sql
  ;; https://github.com/paetzke/format-sql.el
  ;; Format SQL embedded in python source code.
  :require (exec-path-from-shell)
  :ensure-system-package (format-sql . "pip install --user format-sql")
  :defer t)


(req-package flx-ido
  ;; https://github.com/lewang/flx
  ;; Fuzzy matching for ido-mode.
  :require (ido)
  :config
  ;; disable ido's faces so we can see flx's highlighting instead
  (setq ido-use-faces nil
        flx-ido-threshold 10000)
  (flx-ido-mode t))


(req-package flycheck
  ;; http://flycheck.readthedocs.org/en/latest/
  ;; On the fly syntax checking. Easier to configure and use than flymake.
  ;;
  ;; Most checkers call external programs. Need $PATH to find them.
  :require
  (exec-path-from-shell flycheck-pos-tip)
  :delight
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)) ;; Checkdoc just annoys me.
  (setq-default flycheck-emacs-lisp-load-path 'inherit)
  (setq-default flycheck-display-errors-delay 1)
  (global-flycheck-mode))


(req-package flycheck-clojure
  :require
  (flycheck cider)
  :config
  (flycheck-clojure-setup))


(req-package flycheck-pos-tip
  :config
  (custom-set-variables '(flycheck-display-errors-function
                           #'flycheck-pos-tip-error-messages)))


(req-package git-timemachine
  :defer t)


(req-package gitattributes-mode
  ;; https://github.com/magit/git-modes
  ;; .gitattributes major mode.
  :defer t)


(req-package gitconfig-mode
  ;; https://github.com/magit/git-modes
  ;; .gitconfig major mode.
  :defer t
  :init
  ;; I don't want to indent with tabs. Tabs are stupid.
  (add-hook 'gitconfig-mode-hook (lambda()
                                   (setq indent-tabs-mode nil))))


(req-package github-browse-file
  ;; https://github.com/osener/github-browse-file
  ;; View files on github.com
  :defer t
  :config
  (setq github-browse-file-show-line-at-point t))


(req-package gitignore-mode
  ;; https://github.com/magit/git-modes
  ;; .gitignore major mode.
  :mode
  ;; My global settings.
  "\\.gitignore_global\\'")


(req-package graphviz-dot-mode
  ;; https://github.com/ppareit/graphviz-dot-mode
  ;; Support for the .dot file format.
  :defer t)


(use-package hl-line
  :config
  (global-hl-line-mode))


(req-package hungry-delete
  ;; https://github.com/nflath/hungry-delete
  ;; Deletes all the whitespace at once.
  :delight
  :config
  (global-hungry-delete-mode))


(req-package ido-completing-read+
  ;; https://github.com/DarwinAwardWinner/ido-ubiquitous
  ;; Even more ido.
  :require (ido)
  :config
  (ido-ubiquitous-mode t))


(req-package json-mode
  ;; https://github.com/joshwnj/json-mode
  ;; Major mode for editing JSON.
  :mode
  ;; HTTP archives
  "\\.har\\'")


(req-package less-css-mode
  ;; https://github.com/purcell/less-css-mode
  ;; Major mode for less css preprocessor language.
  ;;
  :ensure-system-package (lessc . "npm install -g less")
  :defer t)


(req-package manage-minor-mode
  ;; https://github.com/ShingoFukuyama/manage-minor-mode
  ;; List, activate and deactivate minor modes easily.
  :defer t)


(req-package markdown-mode
  ;; http://jblevins.org/projects/markdown-mode/
  ;; Major mode for editing markdown. Includes gfm-mode for github-flavored
  ;;   markdown. An external markdown program must be installed for preview
  ;;   functionality. I've installed `multimarkdown` with macports.
  ;;
  ;; It's possible to specify the whole path to the markdown command,
  ;; but I prefer not to. Instead, use $PATH.
  :require
  (exec-path-from-shell)
  :ensure-system-package multimarkdown
  :defer t
  :config
  (setq markdown-command "multimarkdown"))


(req-package minesweeper
  :defer t)


(req-package noflet
  :config
  ;; Eliminate prompt when exiting emacs when processes are running.
  (defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
    "Prevent annoying \"Active processes exist\" query when you quit Emacs."
    (noflet ((process-list ())) ad-do-it)))


(req-package nyan-mode
  ;; http://nyan-mode.buildsomethingamazing.com
  ;; The most useful thing ever.
  :config
  (setq nyan-animation-frame-interval 0.1
        nyan-wavy-trail t)
  (nyan-mode)
  ;; Customizing nyan-animate-nyancat calls this, but I don't want
  ;; to use customize for packages.
  (nyan-start-animation))


(req-package oauth
  :require
  (sasl)) ;; Require sasl so that oauth will use it to generate nonces.


(req-package paradox
  ;; https://github.com/Bruce-Connor/paradox
  ;; Better package management, with asynchrony.
  :require
  (async)
  :config
  ;; let-binding to prevent infinite recursion.
  (let ((list-packages #'package-list-packages))
    (defun conleym:list-packages (&optional no-fetch)
      "Replacement for `package-list-packages` that also updates the github star count."
      (interactive)
      (unless no-fetch
        (paradox--refresh-star-count))
      (list-packages no-fetch)))
  ;; paradox-enable makes package-list-packages use the paradox-menu,
  ;; but doesn't update the star counts. This fixes it.
;;  (defalias #'package-list-packages #'conleym:list-packages)
  (paradox-enable)
  (setq paradox-automatically-star nil
        paradox-column-width-package 36
        paradox-column-width-version 16
        paradox-column-width-download 8
        paradox-display-download-count t
        paradox-execute-asynchronously t
        paradox-lines-per-entry 2))


(req-package pdf-tools
  ;; https://github.com/politza/pdf-tools
  ;; PDF rendering and such.
  :config
  ;; Turn line numbers off in the pdf-view-mode.
  ;; See pdf-tools readme.
  (conleym:add-functions-to-hook
   'pdf-view-mode-hook
   #'conleym:disable-line-numbers)
  (pdf-tools-install))


(req-package pip-requirements
  ;; https://github.com/Wilfred/pip-requirements.el
  ;; requirements.txt highlighting and autocompletion.
  :defer t)


(req-package prettier-js
  ;; https://github.com/prettier/prettier-emacs
  ;; use prettier to format javascript code.
  :ensure-system-package (prettier . "npm install -g prettier")
  :init
  (setq prettier-js-args
        '("--bracket-spacing" "false"
          "--single-quote" "true"
          "--print-width" "80"
          "--jsx-bracket-same-line"))
  :config
  (add-hook #'js-mode-hook #'prettier-js-mode))


(req-package pydoc
  ;; https://github.com/statmobile/pydoc
  ;; Nicely formatted, linkable buffer display of pydoc.
  :defer t)


(req-package rainbow-mode
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


(req-package restclient
  ;; https://github.com/pashky/restclient.el
)


(req-package sass-mode
  ;; https://github.com/nex3/sass-mode
  ;; Major mode for SASS with the SASS syntax.
  ;;
  :ensure-system-package (sass . "gem install --user sass")
  :defer t)


(req-package scss-mode
  ;; https://github.com/antonj/scss-mode
  ;; Major mode for SASS with the SCSS syntax.
  ;;
  :ensure-system-package (scss . "gem install --user sass")
  :defer t)


(req-package smartparens-config
  :ensure smartparens
  ;; https://github.com/Fuco1/smartparens
  ;; Pair completion.
  :delight smartparens-mode
  :config
  (smartparens-global-mode 1))


(req-package smex
  ;; https://github.com/nonsequitur/smex
  ;; Better M-x, built on ido.
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands))
  :config
  (setq smex-history-length 200
        smex-save-file (conleym:persistence-dir-file "smex-items"))
  (smex-initialize))


(req-package smooth-scroll
  :delight
  :config
  (smooth-scroll-mode 1)
  (setq smooth-scroll/vscroll-step-size 3
        smooth-scroll/hscroll-step-size 1))


(req-package sourcemap
  ;; https://github.com/syohex/emacs-sourcemap
  ;; sourcemap support.
)


(req-package sr-speedbar
  ;;
  ;; Speedbar without the separate frame.
  :defer t
  :config
  (setq sr-speedbar-right-side nil))


(req-package swift-mode
  :defer t
  :require
  (flycheck))


(req-package tern
  ;; http://ternjs.net/doc/manual.html#emacs
  ;; Code completion and other useful things for javascript.
  ;;
  :ensure-system-package (tern . "npm i -g tern")
  :init
  (add-hook #'js-mode-hook (lambda() (tern-mode t))))


;; AucTeX does some weird stuff...apparently this is how you get it loaded with
;; {req,use}-package.
(req-package tex-site
  :ensure auctex
  :defer t
  ;; There are lots of TeX command line tools and environment variables....
  :require
  (exec-path-from-shell auctex-latexmk reftex company-auctex)
  :init
  (auctex-latexmk-setup)
  (company-auctex-init)
  (conleym:add-functions-to-hook 'TeX-mode-hook
                                 #'TeX-source-correlate-mode
                                 (lambda()
                                   (TeX-fold-mode t)
                                   ;; cmd-shift-click = TeX-view
                                   (bind-keys :map TeX-mode-map
                                              ("<S-s-mouse-1>" . TeX-view))))
  (conleym:add-functions-to-hook 'LaTeX-mode-hook
                                 #'LaTeX-math-mode
                                 (lambda()
                                   (setq TeX-command-default "LatexMk")))
  ;; suggestion from pdf-tools readme.
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
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


(req-package tide
  ;; https://github.com/ananthakumaran/tide/
  ;; typescript support
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save))
  :init
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  :config
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (tide-setup))))
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  (conleym:add-functions-to-hook 'typescript-mode-hook
                                 #'tide-setup
                                 (lambda() (eldoc-mode +1))))


(req-package tumblesocks
  :require (oauth)
  :config
  (setq tumblesocks-token-file (conleym:persistence-dir-file "tumblr-oauth-token")))


(req-package twittering-mode
  :defer t
  :config
  (setq twittering-display-remaining t   ;; Show # of remaining API calls.
        twittering-icon-mode t           ;; Show icons
        twittering-icon-storage-file (conleym:persistence-dir-file "twittering-mode-icons.gz")
        twittering-use-icon-storage t
        twittering-use-master-password t)) ;; Store oauth token.


(req-package typescript-mode
)


(req-package unicode-troll-stopper
  ;; https://github.com/camsaul/emacs-unicode-troll-stopper
  ;; Highlight homoglpyhs.
  ;; TODO turn on global mode when available. See issue #2.
  )


(req-package vagrant
  ;; https://github.com/ottbot/vagrant.el
  ;; Manage vagrant boxes from emacs.
  :defer t)


(req-package vagrant-tramp
  ;; https://github.com/dougm/vagrant-tramp
  ;; vagrant-ssh for tramp.
  :defer t)


(req-package web-mode
  ;; http://web-mode.org
  ;; Major mode for various web template languages.
  :mode (
         ;; handlebars.js templates
         ("\\.hbs\\'" . web-mode))
  :config
  (setq web-mode-enable-current-element-highlight t))


(req-package xkcd
  ;; https://github.com/vibhavp/emacs-xkcd
  ;; Read XKCD in Emacs.
  :require (eimp)
  :config
  (setq xkcd-cache-dir
        (conleym:persistence-dir-file "xkcd/"))
  ;; Avoid stupid mkdir-RET-RET message when directory doesn't exist.
  (conleym:maybe-mkdir xkcd-cache-dir)
  (setq xkcd-cache-latest (concat xkcd-cache-dir "latest"))
  ;; I want to be able to resize the images. Note that xkcd-mode must be
  ;; added to eimp-ignore-readonly-modes for that to work (it's
  ;; customized).
  (add-hook 'xkcd-mode-hook
            #'eimp-mode)
  ;; Rebind keys, since eimp uses the arrows.
  (bind-key "n" #'xkcd-next xkcd-mode-map)
  (bind-key "p" #'xkcd-prev xkcd-mode-map))


(req-package which-key
  ;; https://github.com/justbur/emacs-which-key
  ;; Helps me with keybindings I can't remember.
  :config
  (setq which-key-lighter "")
  (setq which-key-show-remaining-keys t)
  (which-key-mode))


(req-package yagist
  ;; https://github.com/mhayashi1120/yagist.el
  ;; Create and manage gists on github.
  :defer t)


(req-package yaml-mode
  ;; https://github.com/yoshiki/yaml-mode
  ;; Major mode for editing YAML.
  :require (ansible-doc)
  :mode "\\.ya?ml\\'"
  :init
  (add-hook 'yaml-mode-hook #'ansible-doc-mode)
  :config
  (setq yaml-indent-offset tab-width))


(req-package yasnippet
  :delight yas-minor-mode
  :config
  (let ((user-yas-snippets-dir (conleym:dot-dir-file "snippets/")))
    (setq yas-snippet-dirs
          (list user-yas-snippets-dir 'yas-installed-snippets-dir))
    (conleym:maybe-mkdir user-yas-snippets-dir))
  ;; Do not bind to tab. I want to indent with it. Always.
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "C-<tab>") #'yas-expand)
  (yas-global-mode))


(req-package zone-nyan
  :require (zone)
  :defer t
  :config
  (add-to-list 'zone-programs #'zone-nyan))


(provide 'conleym-req-package)
