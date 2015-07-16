(require 'req-package)
(require 'conleym-init-utils)


(req-package 2048-game
  ;; https://bitbucket.org/zck/2048.el
  ;; It's a game. It's fun.
)


(req-package ag
  ;; https://github.com/Wilfred/ag.el
  ;; Silver searcher front end
  :require (exec-path-from-shell)
  :commands (ag ag-files ag-regexp ag-project ag-project-files ag-project-regexp))


(req-package anzu
  ;; https://github.com/syohex/emacs-anzu
  ;; Shows number of matches and the number of the current match when searching.
  :diminish ""
  :init (global-anzu-mode))


(req-package cider
  ;; https://github.com/clojure-emacs/cider
  ;; Clojure IDE.
  :require (eldoc)
  :defer t
  :config
  (add-hook 'cider-mode-hook
            #'eldoc-mode)
  (setq cider-repl-history-file (conleym:persistence-dir-file "cider-history")
        cider-repl-history-size 1000 ;; the default is 500
        cider-repl-wrap-history t
        nrepl-log-messages t))


(req-package clojure
  ;; https://github.com/clojure-emacs/clojure-mode
  ;; Major mode for clojure programming.
  :defer t)


(req-package bug-hunter
  ;; https://github.com/Malabarba/elisp-bug-hunter
  ;; Helps find bugs in init
)


(req-package auto-compile
  ;; https://github.com/tarsius/auto-compile
  ;; Automatically byte (re)compile elisp files.
  :config
  (setq auto-compile-display-buffer nil
        auto-compile-on-load-mode t
        auto-compile-on-save-mode t
        auto-compile-update-autoloads t))


(req-package apples-mode
  ;; https://github.com/tequilasunset/apples-mode
  ;; Major mode for applescript.
  :mode "\\.applescript$")


(req-package coffee-mode
  ;; https://github.com/defunkt/coffee-mode
  ;; Coffeescript major mode
  :require
  (exec-path-from-shell sourcemap)
  :config
  (setq coffee-args-compile '("-c" "-m"))
  (coffee-cos-mode t)
  (bind-key "M-r" #'coffee-compile-buffer coffee-mode-map)
  (add-hook 'coffee-after-compile-hook
            #'sourcemap-goto-corresponding-point))


(req-package company
  ;; https://github.com/company-mode/company-mode
  ;; Autocompletion
  :init
  (global-company-mode))


(req-package dired-imenu
  ;; https://github.com/DamienCassou/dired-imenu
  ;; Fill imenu with the list of files in the current directory when in dired
  ;;   mode.
)


(req-package eimp
  ;; http://mph-emacs-pkgs.alioth.debian.org/EimpEl.html
  ;; Image manipulation using ImageMagick, which must be installed and available
  ;;   in the $PATH.
  :require
  ;; Need $PATH to find ImageMagick tools.
  (exec-path-from-shell)
  :init
  (add-hook 'image-mode-hook
            #'eimp-mode)
  :config
  (setq eimp-enable-undo t))


(req-package elpy
  :require (exec-path-from-shell)
  :init
  (elpy-enable))


(req-package emr
  ;; https://github.com/chrisbarrett/emacs-refactor
  ;; Refactoring library
  :init
  (define-key prog-mode-map (kbd "M-RET") 'emr-show-refactor-menu)
  (add-hook 'prog-mode-hook #'emr-initialize))


(req-package exec-path-from-shell
  ;; https://github.com/purcell/exec-path-from-shell
  ;; Get environment variables from a login shell. Necessary for a reasonable
  ;;   Emacs.app setup on Mac.
  :if (conleym:is-mac-app)
  ;; Seems this must be :init rather than :config. Otherwise eimp can't find
  ;; mogrify and complains.
  :init
  (setq exec-path-from-shell-variables '("MANPATH" "PATH" "PYTHONPATH" "WORKON_HOME"))
  (exec-path-from-shell-initialize))


(req-package fill-column-indicator
  ;; https://github.com/alpaker/Fill-Column-Indicator
  ;; Draw a line at a given column.
  :config
  (defun conleym:fci-80-mode ()
    (setq fci-rule-column 80) ;; becomes local when set.
    (fci-mode 1))
  (conleym:add-function-to-hooks #'conleym:fci-80-mode
                                 'python-mode-hook
                                 'coffee-mode-hook))


(req-package floobits
  ;; https://github.com/Floobits/floobits-emacs
  ;; Floobits integration.
)


(req-package flx-ido
  ;; https://github.com/lewang/flx
  ;; Fuzzy matching for ido-mode.
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
  :require (exec-path-from-shell)
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (setq-default flycheck-emacs-lisp-load-path 'inherit)
  (setq-default flycheck-display-errors-delay 1)
  (global-flycheck-mode))


(req-package flycheck-pos-tip
  :require (flycheck)
  :config
  (eval-after-load 'flycheck
    (custom-set-variables '(flycheck-display-errors-function
                            #'flycheck-pos-tip-error-messages))))


(req-package git-timemachine)


(req-package gitattributes-mode
  ;; https://github.com/magit/git-modes
  ;; .gitattributes major mode.
)


(req-package gitconfig-mode
  ;; https://github.com/magit/git-modes
  ;; .gitconfig major mode.
)


(req-package github-browse-file
  ;; https://github.com/osener/github-browse-file
  ;; View files on github.com
  :config
  (setq github-browse-file-show-line-at-point t))


(req-package gitignore-mode
  ;; https://github.com/magit/git-modes
  ;; .gitignore major mode.
  :mode "\\.gitignore_global" ;; My global settings.
)


(use-package hl-line
  :config
  (global-hl-line-mode))


(req-package hungry-delete
  ;; https://github.com/nflath/hungry-delete
  ;; Deletes all the whitespace at once.
  :config
  (global-hungry-delete-mode))


(req-package ido-ubiquitous
  ;; https://github.com/DarwinAwardWinner/ido-ubiquitous
  ;; Even more ido.
  :init
  (ido-ubiquitous-mode t))


(req-package json-mode
  ;; https://github.com/joshwnj/json-mode
  ;; Major mode for editing JSON.
  :defer t)


(req-package key-chord
  :config
  (key-chord-mode 1))


(req-package less-css-mode)


(req-package manage-minor-mode
  ;; https://github.com/ShingoFukuyama/manage-minor-mode
  ;; List, activate and deactivate minor modes easily.
)


(req-package markdown-mode
  ;; http://jblevins.org/projects/markdown-mode/
  ;; Major mode for editing markdown. Includes gfm-mode for github-flavored
  ;;   markdown. An external markdown program must be installed for preview
  ;;   functionality. I've installed `multimarkdown` with homebrew.
  ;;
  ;; It's possible to specify the whole path to the markdown command,
  ;; but I prefer not to. Instead, use $PATH.
  :require (exec-path-from-shell)
  :defer t
  :config
  (setq markdown-command "multimarkdown"))


(req-package minesweeper)


(req-package noflet
  :config
  ;; Eliminate prompt when processes are running.
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


(req-package paradox
  ;; https://github.com/Bruce-Connor/paradox
  ;; Better package management, with asynchrony.
  :require (async)
  :commands (paradox-enable)
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
  (defalias #'package-list-packages #'conleym:list-packages)
  (paradox-enable)
  (setq paradox-automatically-star nil
        paradox-column-width-package 36
        paradox-column-width-version 16
        paradox-column-width-download 8
        paradox-display-download-count t
        paradox-execute-asynchronously t
        paradox-lines-per-entry 2))


(req-package puppet-mode
  ;; https://github.com/lunaryorn/puppet-mode
  ;; Major mode for editing puppet manifests.
  :defer t)


(req-package pydoc
  ;; https://github.com/statmobile/pydoc
  ;; Nicely formatted, linkable buffer display of pydoc.
  :defer t)


(req-package rainbow-mode
  ;; https://julien.danjou.info/projects/emacs-packages#rainbow-mode
  ;; Show strings representing colors in the color they represent.
  :diminish rainbow-mode
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


(req-package smex
  ;; https://github.com/nonsequitur/smex
  ;; Better M-x, built on ido.
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands))
  :config
  (setq smex-history-length 200)
  (setq smex-save-file
        (conleym:persistence-dir-file "smex-items"))
  (smex-initialize))


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
  :require (flycheck))


(req-package syslog-mode
  ;; https://github.com/vapniks/syslog-mode
  ;; Fontifies system logs.
  :defer t
  :init
  (add-to-list 'auto-mode-alist
               '("/var/log/.*\\.log.*\\'" . syslog-mode)))


;; AucTeX does some weird stuff...apparently this is how you get it loaded with
;; {req,use}-package.
(req-package tex-site
  :ensure auctex
  :defer t
  ;; There are lots of TeX command line tools and environment variables....
  :require (exec-path-from-shell reftex auctex-latexmk)
  :config
  (auctex-latexmk-setup)
  (TeX-global-PDF-mode t)
  (conleym:add-functions-to-hook 'LaTeX-mode-hook
                                 #'TeX-source-correlate-mode
                                 (lambda()
                                   (setq TeX-command-default "LatexMk")
                                   (bind-keys :map LaTeX-mode-map
                                              ("<S-s-mouse-1>" . TeX-view))))
  (setq-default TeX-master nil)
  (setq preview-auto-cache-preamble t
        TeX-auto-save t
        TeX-auto-untabify t
        TeX-parse-self t
        TeX-source-correlate-method 'synctex
        TeX-source-correlate-start-server t
        TeX-view-program-list '(("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b"))
        TeX-view-program-selection '((output-pdf "Skim")
                                     (output-dvi "Skim")
                                     (output-html "open"))))


(req-package undo-tree
  ;; http://www.dr-qubit.org/emacs.php#undo-tree
  ;; Minor mode that makes undo and redo easier to understand, use, and visualize.
  :diminish ""
  :config
  (setq undo-tree-history-directory-alist
        `(( ".*" . ,(conleym:persistence-dir-file "undo/")))
        undo-tree-auto-save-history t
        undo-tree-visualizer-diff t
        undo-tree-visualizer-timestamps t)
  (global-undo-tree-mode))


(req-package web-mode
  ;; http://web-mode.org
  ;; Major mode for various web template languages.
  :mode (("\\.erb$" . web-mode) ;; ruby templates used by puppet
         ;; handlebars.js templates
         ("\\.hbs$" . web-mode)))


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


(req-package yagist
  ;; https://github.com/mhayashi1120/yagist.el
  ;; Create and manage gists on github.
)


(req-package yaml-mode
  ;; https://github.com/yoshiki/yaml-mode
  ;; Major mode for editing YAML.
  :config
  (setq yaml-indent-offset tab-width))


(req-package yasnippet
  :diminish yas-minor-mode
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


(provide 'conleym-req-package)
