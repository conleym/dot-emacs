(require 'req-package)
(require 'conleym-init-utils)


(req-package 2048-game
  ;; https://bitbucket.org/zck/2048.el
  ;; It's a game. It's fun.
)

(req-package anzu
  ;; https://github.com/syohex/emacs-anzu
  ;; Shows number of matches and the number of the current match when searching.
  :diminish ""
  :init (global-anzu-mode))

(req-package auto-compile
  ;; https://github.com/tarsius/auto-compile
  ;; Automatically byte (re)compile elisp files.
  :init (progn
          (setq auto-compile-on-load-mode t
                auto-compile-on-save-mode t))
  :config (progn
            (setq auto-compile-display-buffer nil
                  auto-compile-update-autoloads t)))

(req-package apples-mode
  ;; https://github.com/tequilasunset/apples-mode
  ;; Major mode for applescript.
  :mode "\\.applescript$")

(req-package coffee-mode
  ;; https://github.com/defunkt/coffee-mode
  ;; Coffeescript major mode
  :config (progn
            (setq coffee-args-compile '("-c" "-m"))
            (bind-key "M-r" #'coffee-compile-buffer coffee-mode-map)
            (add-hook 'coffee-after-compile-hook
                      #'sourcemap-goto-corresponding-point))
  :require (exec-path-from-shell sourcemap))

(req-package dired-imenu
  ;; https://github.com/DamienCassou/dired-imenu
  ;; Fill imenu with the list of files in the current directory when in dired
  ;;   mode.
)

(req-package eimp
  ;; http://mph-emacs-pkgs.alioth.debian.org/EimpEl.html
  ;; Image manipulation using ImageMagick, which must be installed and available
  ;;   in the $PATH.
  :init (progn
          (add-hook 'image-mode-hook
                    'eimp-mode))
  :config (progn
            (setq eimp-enable-undo t))
  ;; Need $PATH to find ImageMagick tools.
  :require (exec-path-from-shell))

(req-package elpy
  :init (progn
          (elpy-enable)))

(req-package exec-path-from-shell
  ;; https://github.com/purcell/exec-path-from-shell
  ;; Get environment variables from a login shell. Necessary for a reasonable
  ;;   Emacs.app setup on Mac. Variables to copy are customized.
  :if (conleym:is-mac-app)
  :init (progn
          (exec-path-from-shell-initialize))
  :config (progn
            (setq exec-path-from-shell-variables '("PATH" "MANPATH" "WORKON_HOME" "PYTHONPATH"))))

(req-package fill-column-indicator
  ;; https://github.com/alpaker/Fill-Column-Indicator
  ;; Draw a line at a given column.
  :init (progn
          (defun conleym:fci-80-mode()
            (setq fci-rule-column 80) ;; becomes local when set.
            (fci-mode 1))
          (conleym:add-function-to-hooks #'conleym:fci-80-mode
                                         'python-mode-hook
                                         'coffee-mode-hook)))

(req-package floobits
  ;; https://github.com/Floobits/floobits-emacs
  ;; Floobits integration.
)

(req-package flx-ido
  ;; https://github.com/lewang/flx
  ;; Fuzzy matching for ido-mode.
  :require (ido)
  :init (progn
          (flx-ido-mode t))
  :config (progn
            ;; disable ido's faces so we can see flx's highlighting instead
            (setq ido-use-faces nil
                  flx-ido-threshold 10000)))

(req-package flycheck
  ;; http://flycheck.readthedocs.org/en/latest/
  ;; On the fly syntax checking. Easier to configure and use than flymake.
  ;;
  ;; Most checkers call external programs. Need $PATH to find them.
  :require (exec-path-from-shell)
  :init (progn
          (global-flycheck-mode))
  :config (progn
            (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
            (setq-default flycheck-emacs-lisp-load-path 'inherit)
            (setq-default flycheck-display-errors-delay 0)
            (req-package flycheck-pos-tip
              :init (progn
                      (setq flycheck-display-errors-function
                            #'flycheck-pos-tip-error-messages)))))

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
  :config (progn
            (setq github-browse-file-show-line-at-point t)))

(req-package gitignore-mode
  ;; https://github.com/magit/git-modes
  ;; .gitignore major mode.
  :mode "\\.gitignore_global" ;; My global settings.
)

(req-package hungry-delete
  ;; https://github.com/nflath/hungry-delete
  ;; Deletes all the whitespace at once.
  :init (progn
          (global-hungry-delete-mode)))

(req-package ido-ubiquitous
  ;; https://github.com/DarwinAwardWinner/ido-ubiquitous
  ;; Even more ido.
  :init (progn
          (ido-ubiquitous-mode t)))

(req-package json-mode
  ;; https://github.com/joshwnj/json-mode
  ;; Major mode for editing JSON.
)

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
  :config (progn
            (setq markdown-command "multimarkdown")))

(req-package nyan-mode
  ;; http://nyan-mode.buildsomethingamazing.com
  ;; The most useful thing ever.
  :init (progn
          (nyan-mode))
  :config (progn
            (setq nyan-animation-frame-interval 0.1
                  nyan-wavy-trail t)
            ;; Customizing nyan-animate-nyancat calls this, but I don't want
            ;; to use customize for packages.
            (nyan-start-animation)))

(req-package paradox
  ;; https://github.com/Bruce-Connor/paradox
  ;; Better package management, with asynchrony.
  :require(async)
  :init (progn
          ;; let-binding to prevent infinite recursion.
          (let ((list-packages #'package-list-packages))
            (defun conleym:list-packages (&optional no-fetch)
              "Replacement for `package-list-packages` that also updates the github star count."
              (interactive)
              (unless no-fetch
                (paradox--refresh-star-count))
              (list-packages no-fetch)))
          (paradox-enable)
          ;; paradox-enable makes package-list-packages use the paradox-menu,
          ;; but doesn't update the star counts. This fixes it.
          (defalias #'package-list-packages #'conleym:list-packages))
  :config (progn
            (setq paradox-column-width-package 36
                  paradox-column-width-version 16
                  paradox-column-width-download 8
                  paradox-display-download-count t
                  paradox-lines-per-entry 2)))

(req-package puppet-mode
 ;; https://github.com/lunaryorn/puppet-mode
 ;; Major mode for editing puppet manifests.
)

(req-package rainbow-mode
  ;; https://julien.danjou.info/projects/emacs-packages#rainbow-mode
  ;; Show strings representing colors in the color they represent.
  :init (progn
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
              (add-hook hook #'rainbow-mode)))))

(req-package smex
  ;; https://github.com/nonsequitur/smex
  ;; Better M-x, built on ido.
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands))
  :config (progn
            (setq smex-history-length 200)
            (setq smex-save-file
                  (conleym:persistence-dir-file "smex-items")))
  :idle (smex-initialize))

(req-package sourcemap
  ;; https://github.com/syohex/emacs-sourcemap
  ;; sourcemap support.
)

(req-package sr-speedbar
  ;;
  ;; Speedbar without the separate frame.
  :config (progn
            (setq sr-speedbar-right-side nil)))

(req-package swift-mode)

(req-package syslog-mode
  ;; https://github.com/vapniks/syslog-mode
  ;; Fontifies system logs.
  :init (progn
          (add-to-list 'auto-mode-alist
                       '("/var/log/.*\\.log.*\\'" . syslog-mode))))

;; AucTeX does some weird stuff...apparently this is how you get it loaded with
;; {req,use}-package.
(req-package tex-site
  :ensure auctex
  ;; There are lots of TeX command line tools and environment variables....
  :require (exec-path-from-shell)
  :config (progn
            (setq preview-auto-cache-preamble t)))

(req-package undo-tree
  ;; http://www.dr-qubit.org/emacs.php#undo-tree
  ;; Minor mode that makes undo and redo easier to understand, use, and visualize.
  :diminish ""
  :init (progn
          (global-undo-tree-mode))
  :config (progn
            (setq undo-tree-history-directory-alist
                  `(( ".*" . ,(conleym:persistence-dir-file "undo/")))
                  undo-tree-auto-save-history t
                  undo-tree-visualizer-diff t
                  undo-tree-visualizer-timestamps t)))

(req-package web-mode
  ;; http://web-mode.org
  ;; Major mode for various web template languages.
  :mode (("\\.erb$" . web-mode) ;; ruby templates used by puppet
         ("\\.hbs$" . web-mode) ;; handlebars.js templates
))

(req-package xkcd
  ;; https://github.com/vibhavp/emacs-xkcd
  ;; Read XKCD in Emacs.
  :require (eimp)
  :init (progn
          ;; I want to be able to resize the images. Note that xkcd-mode must be
          ;; added to eimp-ignore-readonly-modes for that to work (it's
          ;; customized).
          (add-hook 'xkcd-mode-hook
                    #'eimp-mode)
          ;; It appears that these variables must be set in :init rather than
          ;; :config. Otherwise, it tries to open the latest from the default
          ;; cache.
          (setq xkcd-cache-dir
                (conleym:persistence-dir-file "xkcd/"))
          ;; Avoid stupid mkdir-RET-RET message when directory doesn't exist.
          (conleym:maybe-mkdir xkcd-cache-dir)
          (setq xkcd-cache-latest (concat xkcd-cache-dir "latest")))
  :config (progn
            ;; Rebind keys, since eimp uses the arrows.
            (bind-key "n" #'xkcd-next xkcd-mode-map)
            (bind-key "p" #'xkcd-prev xkcd-mode-map)))

(req-package yagist
  ;; https://github.com/mhayashi1120/yagist.el
  ;; Create and manage gists on github.
)

(req-package yaml-mode
  ;; https://github.com/yoshiki/yaml-mode
  ;; Major mode for editing YAML.
  :config (progn
            (setq yaml-indent-offset tab-width)))

(req-package yasnippet
  :diminish yas-minor-mode
  :config (progn
            (let ((user-yas-snippets-dir (conleym:dot-dir-file "snippets/")))
              (setq yas-snippet-dirs
                    (list user-yas-snippets-dir 'yas-installed-snippets-dir))
              (conleym:maybe-mkdir user-yas-snippets-dir))
            ;; Do not bind to tab. I want to indent with it. Always.
            (define-key yas-minor-mode-map (kbd "<tab>") nil)
            (define-key yas-minor-mode-map (kbd "TAB") nil)
            (define-key yas-minor-mode-map (kbd "C-<tab>") #'yas-expand)))


(provide 'conleym-req-package)
