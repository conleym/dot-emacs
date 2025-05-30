;; configure packages that aren't builtin.   -*- lexical-binding: t -*-

(require 'conleym-init-utils)


(use-package 2048-game
  ;; https://bitbucket.org/zck/2048.el
  ;; It's a game. It's fun.
  :hook
  (2048-mode . conleym:disable-display-line-numbers-mode))


(use-package ag
  ;; https://github.com/Wilfred/ag.el
  ;; Silver searcher front end.
  :custom
  (ag-highlight-search 't)
  ;; TODO do I want this?
  ;; (ag-reuse-window 't)
  ;; (ag-reuse-buffers 't)
  )


(use-package all-the-icons
  ;; https://github.com/domtronn/all-the-icons.el
  ;; Fonts containing icons
  :init
  ;; https://github.com/domtronn/all-the-icons.el/issues/120#issuecomment-480342779
  (let ((font-families (font-family-list)))
    ;; font-family-list returns nil on terminal! We won't try to install the
    ;; icon fonts unless loaded graphically.
    (unless (or (null font-families) (member "all-the-icons" font-families))
      (all-the-icons-install-fonts t)))
  :config
  (add-to-list 'all-the-icons-icon-alist
               '("\\.dtx\\'" all-the-icons-fileicon "tex" :face all-the-icons-lblue)))


(use-package all-the-icons-dired
  ;; https://github.com/wyuenho/all-the-icons-dired
  ;; Add icons to dired-mode
  :delight
  :after
  (all-the-icons dired)
  :custom
  (all-the-icons-dired-monochrome nil)
  :hook
  (dired-mode . all-the-icons-dired-mode))


(use-package all-the-icons-ibuffer
  ;; https://github.com/seagle0128/all-the-icons-ibuffer
  ;; Add icons to ibuffer.
  :custom
  (all-the-icons-ibuffer-human-readable-size t "Use human readable file sizes in ibuffer-mode.")
  :init (all-the-icons-ibuffer-mode 1))


(use-package aggressive-indent
  ;; https://github.com/Malabarba/aggressive-indent-mode
  ;; Keep shit indented properly.
  :config
  (global-aggressive-indent-mode 1))


(use-package auctex-latexmk
  ;; https://github.com/tom-tan/auctex-latexmk
  ;; Sets auctex up to use the latexmk command.
  :custom
  (auctex-latexmk-inherit-TeX-PDF-mode t))


(use-package auto-compile
  ;; https://github.com/tarsius/auto-compile
  ;; Automatically byte (re)compile elisp files.
  :hook
  (emacs-lisp-mode . auto-compile-mode)
  :custom
  (auto-compile-display-buffer nil "Don't bring up the compile log buffer.")
  (auto-compile-mode-line-counter t "Show warnings on modeline.")
  (auto-compile-update-autoloads t)
  (auto-compile-on-load-mode t)
  (auto-compile-on-save-mode t))


(use-package auto-package-update
  ;; https://github.com/rranelli/auto-package-update.el
  ;; Automatically update installed packages
  :custom
  (auto-package-update-delete-old-versions t "Clean up old versions")
  (auto-package-update-interval 1 "Update daily.")
  :init
  (auto-package-update-maybe))


(use-package beacon
  ;; https://github.com/Malabarba/beacon
  ;; Make cursor position obvious when it might otherwise not be.
  :delight
  :custom
  (beacon-blink-when-focused t)
  :config
  (beacon-mode 1))


(use-package browse-at-remote
  ;; https://github.com/rmuslimov/browse-at-remote
  ;; Show the current file in the remote repository.
  :custom
  (browse-at-runtime-prefer-symbolic nil "Use commit hashes for longer-lived links."))


(use-package company
  ;; https://github.com/company-mode/company-mode
  ;; Autocompletion
  :delight
  :config
  (global-company-mode))


(use-package company-auctex
  ;; https://github.com/alexeyr/company-auctex
  ;; auctex completion
)


(use-package company-emoji
  :init
  (add-to-list 'company-backends #'company-emoji))


;; TODO use this or get rid of it.
(use-package crux
  ;; https://github.com/bbatsov/crux
)


(use-package define-word
  ;; https://github.com/abo-abo/define-word
)


(use-package dired-imenu
  :after
  (dired imenu))


(use-package docker)


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
  :hook
  (image-mode . eimp-mode)
  :custom
  (eimp-enable-undo t))


(use-package elpy
  ;; https://github.com/jorgenschaefer/elpy
  ;; Python development env
  :hook
  (elpy-mode . flycheck-mode)
  :custom
  (elpy-rpc-virtualenv-path (conleym:persistence-dir-file "elpy") "Keep the elpy venv in the persistence directory.")
  :config
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  ;; Avoid confirmation prompt if virtualenv doesn't exist.
  (conleym:maybe-mkdir elpy-rpc-virtualenv-path)
  :init
  (advice-add #'python-mode :before #'elpy-enable))


(use-package emojify
  ;; https://github.com/iqbalansari/emacs-emojify
  ;; Convert some sequences of characters to emojis.
  :custom
  (emojify-emojis-dir (conleym:persistence-dir-file "emojis/") "Keep the emoji images in the persistence directory.")
  (emojify-download-emojis-p t "Download emoji images without asking. I hate prompts.")
  (emojify-emoji-styles '(github) "Show only github emojis. Emoticons are too common in documents, and unicode chars are already handled by fonts.")
  ;; TODO be more sophisticated about this: is a suitable font available?
  (emojify-display-style 'unicode "Use unicode fonts, not the images.")
  :init
  (global-emojify-mode))


(use-package emr
  ;; https://github.com/chrisbarrett/emacs-refactor
  ;; Refactoring library
  :hook
  (prog-mode . emr-initialize)
  :bind
  (:map prog-mode-map
        ("M-RET" . #'emr-show-refactor-menu)))


(use-package es-mode
  ;; https://github.com/dakrone/es-mode
  ;; Elasticsearch stuff.
  :defer t)


(use-package fill-column-indicator
  ;; https://github.com/alpaker/Fill-Column-Indicator
  ;; Draw a line at a given column.
  :defer t
  :config
  (defun conleym:fci-80-mode ()
    "Draw the fill column indicator in column 80."
    (interactive)
    (setq fci-rule-column 80) ;; becomes local when set.
    (fci-mode 1))  
  :hook (python-mode . conleym:fci-80-mode))


(use-package flx-ido
  ;; https://github.com/lewang/flx
  ;; Fuzzy matching for ido-mode.
  :after
  (ido)
  :custom
  (ido-use-faces nil "Disable ido's faces so we can see flx's highlighting instead.")
  (flx-ido-threshold 10000)
  :config
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


(use-package flycheck-relint
  ;; https://github.com/purcell/flycheck-relint
  ;; flycheck for elisp regexes.
  :after
  (flycheck relint)
  :init
  (flycheck-relint-setup))


(use-package git-timemachine
  ;; https://codeberg.org/pidu/git-timemachine
  ;; go forward and backward in git history with "n" and "p", etc.
  :defer t)


(use-package git-modes
  ;; https://github.com/magit/git-modes
  ;; Automatically installs gitconfig, gitignore, gitattributes modes.
  :init
  (use-package gitconfig-mode
    ;; .gitconfig major mode.
    :ensure f
    :hook
    (gitconfig-mode . (lambda()
                        ;; I don't want to indent with tabs. Tabs are stupid.
                        (setq indent-tabs-mode nil))))
  (use-package gitignore-mode
    ;; .gitignore major mode.
    :ensure f
    :mode
    ;; My global settings.
    "\\.gitignore_global\\'"))

(use-package hungry-delete
  ;; https://github.com/nflath/hungry-delete
  ;; Deletes all the whitespace at once.
  :delight
  :config
  (global-hungry-delete-mode))


(use-package ido-completing-read+
  ;; https://github.com/DarwinAwardWinner/ido-completing-read-plus
  ;; Even more ido.
  :after
  (ido)
  :config
  (ido-ubiquitous-mode t))


(use-package imenu-list
  ;; https://github.com/bmag/imenu-list
  :after
  (imenu)
  :hook
  (imenu-list-major-mode . conleym:disable-display-line-numbers-mode))


(use-package jq-mode
  ;; https://github.com/ljos/jq-mode
)


(use-package json-mode
  ;; https://github.com/joshwnj/json-mode
  ;; Major mode for editing JSON.
  :mode
  ;; HTTP archives
  ("\\.har\\'" "\\.json\\'"))


(use-package lsp-mode
  ;; https://github.com/emacs-lsp/lsp-mode
  ;; lsp support for emacs.
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  :commands
  (lsp lsp-deferred lsp-enable-which-key-integration)
  :config
  (use-package lsp-ui :commands lsp-ui-mode))


(use-package magit)


;; https://www.reddit.com/r/emacs/comments/1kq6i4f/comment/mt3nboq/
;;(setq package-install-upgrade-built-in t)
;; Pin to melpa, since builtin and gnu versions are too old to work w/ magit.
;;(use-package transient
;;  :pin melpa)



(use-package manage-minor-mode
  ;; https://github.com/ShingoFukuyama/manage-minor-mode
  ;; List, activate and deactivate minor modes easily.
  )


(use-package markdown-mode
  ;; http://jblevins.org/projects/markdown-mode/
  ;; Major mode for editing markdown. Includes gfm-mode for github-flavored
  ;;   markdown. An external markdown program must be installed for preview
  ;;   functionality. I've installed `multimarkdown` with macports.
  :custom
  (markdown-command "multimarkdown"))


(use-package ns-auto-titlebar
  ;; https://github.com/purcell/ns-auto-titlebar
  :if
  (conleym:is-darwin)
  :config
  (ns-auto-titlebar-mode))


(use-package nov
  ;; https://depp.brause.cc/nov.el/
  :mode
  ("\\.epub\\'" . nov-mode)
  :custom
  (nov-save-place-file
   (conleym:persistence-dir-file "nov-places") "Keep saved places in persistence dir."))


(use-package nyan-mode
  ;; http://nyan-mode.buildsomethingamazing.com
  ;; The most useful thing ever.
  :custom
  (nyan-animation-frame-interval 0.1 "Set reasonable frame interval for nyan.")
  (nyan-wavy-trail t "Make nyan's trail wavy.")
  (nyan-animate-nyancat t "Animate nyancat.")
  :config
  (nyan-mode))


(use-package osx-location
  ;; https://github.com/purcell/osx-location
  ;; Location services for emacs.
  :if (conleym:is-darwin)
  :config
  (use-package solar
    :ensure nil)
  ;; using a named function here seems to cause the package to break.
  :hook
  (osx-location-changed . (lambda () 
                            (setq calendar-latitude osx-location-latitude
                                  calendar-longitude osx-location-longitude
                                  calendar-location-name
                                  (format "%s, %s" osx-location-latitude osx-location-longitude)))))


(use-package pdf-tools
  ;; https://github.com/vedang/pdf-tools
  :magic
  ("%PDF" . pdf-view-mode)
  ;; Disable line numbers. I don't want them, because it can break this mode:
  ;; https://github.com/vedang/pdf-tools#display-line-numbers-mode
  :hook
  (pdf-view-mode . conleym:disable-line-numbers)
  :config
  (pdf-tools-install :no-query))


(use-package pip-requirements
  ;; https://github.com/Wilfred/pip-requirements.el
  ;; requirements.txt highlighting and autocompletion.
  :defer t)


(use-package prettier-js
  ;; https://github.com/prettier/prettier-emacs
  ;; use prettier to format javascript code.
  ;; prettier must be installed (use `npm install -g prettier').
  :delight
  :hook
  (js-mode . prettier-js-mode)
  :custom
  (prettier-js-args
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
  ;; https://github.com/Fanael/rainbow-delimiters
  ;; Color-codes delimiters according to depth.
  :hook
  (prog-mode . rainbow-delimiters-mode))


(use-package relint
  ;; https://github.com/mattiase/relint
  ;; elisp regex linter. See also flycheck-relint.
  )


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


(use-package smartparens-config
  :ensure smartparens
  ;; https://github.com/Fuco1/smartparens
  ;; Pair completion.
  :delight smartparens-mode
  :config
  (smartparens-global-mode 1))


(use-package smex
  ;; https://github.com/nonsequitur/smex
  ;; Better M-x, built on ido.
  :bind
  (("M-x" . smex)
   ("M-X" . smex-major-mode-commands))
  :custom
  (smex-history-length 200)
  (smex-save-file (conleym:persistence-dir-file "smex-items"))
  :config
  (smex-initialize))


(use-package sr-speedbar
  ;;
  ;; Speedbar without the separate frame.
  :defer t
  :custom
  (sr-speedbar-right-side nil "Keep it on the left."))


(use-package sudo-edit
  ;; https://github.com/nflath/sudo-edit
  ;; Edit a file as another user easily.
  :defer t
  )

(use-package tex-site
  :ensure auctex
  :after
  (auctex-latexmk reftex company-auctex)
  :functions
  (TeX-revert-document-buffer)
  :hook
  ((TeX-mode . TeX-source-correlate-mode)
   (LaTeX-mode . LaTeX-math-mode))
  :bind
  (:map TeX-mode-map
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
  (use-package preview
    :ensure nil ;; included with auctex
    :custom
    (setq preview-auto-cache-preamble t))
  (setq-default TeX-master nil)
  (setq LaTeX-math-menu-unicode t
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
  :config
  (defun conleym:setup-tide-mode ()
    (interactive)
    (tide-setup)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    ;; slight difference from readme version because
    ;; company, flycheck, and eldoc are all globally enabled.
    (tide-hl-identifier-mode +1))
  :hook
  ((typescript-mode . conleym:setup-tide-mode)
   (before-save . tide-format-before-save)))


(use-package unicode-troll-stopper
  ;; https://github.com/camsaul/emacs-unicode-troll-stopper
  ;; Highlight homoglpyhs.
  ;; TODO turn on global mode when available. See issue #2.
  )


(use-package web-mode
  ;; http://web-mode.org
  ;; Major mode for various web template languages.
  :after
  (flycheck tide)
  :functions
  (flycheck-add-mode)
  :mode
  (("\\.hbs\\'" . web-mode) ;; handlebars.js templates
   ("\\.tsx\\'" . web-mode)) ;; typescript react/jsx.
  :init
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (conleym:setup-tide-mode))))
  :custom
  (web-mode-enable-current-element-highlight t)
  :config
  (flycheck-add-mode 'typescript-tslint 'web-mode))


(use-package which-key
  ;; https://github.com/justbur/emacs-which-key
  ;; Pop up a list of possible completions for the sequence of keys typed after a short delay.
  :delight
  :config
  (which-key-mode))


(use-package xkcd
  ;; https://github.com/vibhavp/emacs-xkcd
  ;; Read XKCD in Emacs.
  :commands ;; why doesn't :functions silence warnings here?
  (xkcd-next xkcd-prev)
  ;; I want to be able to resize the images. Note that xkcd-mode must be
  ;; added to eimp-ignore-readonly-modes for that to work (it's
  ;; customized).
  :hook
  (xkcd-mode . eimp-mode)
  ;; Rebind keys, since eimp uses the arrows.
  :bind
  (:map xkcd-mode-map
        ("n" . #'xkcd-next)
        ("p" . #'xkcd-prev))
  :custom
  (xkcd-cache-dir
   (conleym:persistence-dir-file "xkcd/")  "Keep xkcd files in the persistence directory.")
  (xkcd-cache-latest
   (concat xkcd-cache-dir "latest")  "Keep xkcd files in the persistence directory.")
  :config
  ;; Avoid stupid mkdir-RET-RET message, plus failure to load, when directory doesn't exist.
  (conleym:maybe-mkdir xkcd-cache-dir))


(use-package yaml-mode
  ;; https://github.com/yoshiki/yaml-mode
  ;; Major mode for editing YAML.
  :mode "\\.ya?ml\\'"
  :custom
  (yaml-indent-offset tab-width))


(use-package zone-nyan
  ;; https://depp.brause.cc/zone-nyan/
  ;; nyan screensaver ("zone program")
  :after (zone)
  :defer t
  :defines zone-programs
  :config
  (add-to-list 'zone-programs #'zone-nyan)
  ;;  :custom
  ;; Not ideal, with the GUI elements
  ;; (zone-nyan-bg-music-program "VLC"  "Use VLC to play the music")
  ;; (zone-nyan-bg-music-args
  ;; `("-L", (expand-file-name "~/Downloads/original.mp3"))  "Set the music"))
)

(provide 'conleym-elpa-packages)
