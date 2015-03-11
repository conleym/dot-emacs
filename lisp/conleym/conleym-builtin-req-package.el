(require 'req-package)
(require 'conleym-init-utils)


(use-package abbrev
  ;; Expands abbreviations from a dictionary.
  :diminish ""
  :init (progn
          ;; Global abbrev mode. Curiously not customizable.
          (setq-default abbrev-mode t))
  :config (progn
            (setq abbrev-file-name
                  (conleym:persistence-dir-file "abbrev_defs"))
            ;; Load the file if it exists, don't bother me if it doesn't.
            (if (file-exists-p abbrev-file-name)
                (quietly-read-abbrev-file))))

(use-package autorevert
  :init (progn
          (global-auto-revert-mode))
  :config (progn
            ;; Default (5 seconds) is too long to wait.
            (setq auto-revert-interval 1)))

(use-package bookmark
  :config (progn
            (setq bookmark-default-file
                  (conleym:persistence-dir-file "bookmarks"))))

(req-package desktop
  :init (progn
          (setq desktop-save t
                desktop-save-mode t
                desktop-load-locked-desktop t
                desktop-not-loaded-hook #'desktop-save-mode-off
                desktop-restore-eager t
                desktop-dirname (conleym:persistence-dir-file "desktop/")
                desktop-path (list desktop-dirname))
          ;; Avoid error if dir doesn't yet exist.
          (conleym:maybe-mkdir desktop-dirname)))

(use-package dired
  :config (progn
            (setq dired-auto-revert-buffer t)))

(use-package eldoc
  ;; Shows lisp docstrings in the minibuffer.
  :diminish "")

(use-package files
  :init (progn
          ;; Number of versions to keep. Just picked a relatively large
          ;; number. Default is 2.
          (let ((kept-versions 20))
            (setq version-control t
                  kept-old-versions kept-versions
                  kept-new-versions kept-versions))
          ;; autosave, backup, etc. files go in the persistence dir.
          (let ((auto-save-list-dir (conleym:persistence-dir-file "auto-save-list/"))
                (auto-save-dir (conleym:persistence-dir-file "auto-saves/"))
                (backup-dir (conleym:persistence-dir-file "backups/")))
            (conleym:maybe-mkdir auto-save-list-dir)
            (conleym:maybe-mkdir auto-save-dir)
            (conleym:maybe-mkdir backup-dir)
            (setq auto-save-list-file-prefix (concat auto-save-list-dir ".saves-"))
            (setq auto-save-file-name-transforms
                  `((".*" ,auto-save-dir)))
            (setq backup-directory-alist
                  `(( ".*" . ,backup-dir))))))

(req-package ido
  :init (progn
          ;; Search recently opened files, not just currently open ones
          ;; and use flex matching.
          (setq ido-use-virtual-buffers t
                ido-enable-flex-matching t)
          (ido-everywhere t)
          (ido-mode t))
  :config (progn
            (setq ido-save-directory-list-file
                  (conleym:persistence-dir-file "ido.last"))
            (setq ido-max-prospects 10)))

(req-package imenu
  ;; Show definitions from current file in a menu.
  :init (progn
          (defun conleym:safe-imenu()
            "Try to add imenu index to the menubar, ignoring errors if imenu
isn't supported in this major mode."
            (interactive)
            (require 'imenu)
            (ignore-errors
              (progn
                (imenu-add-menubar-index))))
          (conleym:add-function-to-hooks 'conleym:safe-imenu
                                         #'after-change-major-mode-hook))
  :config (progn
            (setq imenu-auto-rescan t
                  imenu-sort-function #'imenu--sort-by-name)))

(req-package ispell
  :config (progn
            ;; Prefer hunspell if available.
            (when (executable-find "hunspell")
              (setq-default ispell-program-name "hunspell")
              (setq-default ispell-dictionary "en_US")
              (add-to-list 'ispell-local-dictionary-alist
                           '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "'" t ("-d en_US") "~tex" utf-8)))))
  ;; ispell uses {a,i,hun}spell. Need $PATH.
  :require (exec-path-from-shell))

(use-package js-mode
  :mode ".jsx$")

(req-package lisp-mode
  ;; Turn eldoc-mode on in all lisp modes.
  :init (progn
          (conleym:add-function-to-hooks 'turn-on-eldoc-mode
                                         #'emacs-lisp-mode-hook
                                         #'lisp-interaction-mode-hook
                                         #'ielm-mode-hook)))

(use-package nxml-mode
  ;; Major mode for editing XML.
  :init (progn
          (push '("<\\?xml" . nxml-mode) magic-mode-alist)
          (push '("<![dD][oO][cC][tT][yY][pP][eE]" . nxml-mode) magic-mode-alist))
  :config (progn
            (setq nxml-attribute-indent tab-width
                  nxml-slash-auto-complete-flag t
                  nxml-child-indent tab-width)))

(use-package rcirc
  :config (progn
            (rcirc-track-minor-mode)
            (setq rcirc-log-flag t
                  rcirc-default-full-name user-full-name
                  rcirc-log-directory (conleym:persistence-dir-file "rcirc-logs/")
                  rcirc-server-alist
                  '(("irc.freenode.net" :port 6697 :encryption tls)))))

(req-package recentf
  :bind ("C-x C-r" . conleym:recentf-ido-find-file)
  :config (progn
            (setq recentf-save-file (conleym:persistence-dir-file "recentf")
                  recentf-max-menu-items 25
                  recentf-max-saved-items 400))
  :init (progn
          (recentf-mode) ;; always on.
          (defun conleym:recentf-ido-find-file ()
            "Find a recent file using ido."
            (interactive)
            (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
              (when file
                (find-file file))))
          ;; Up arrow will go to previously opened files in this session if you
          ;; hit up in the minibuffer.
          (add-hook 'ido-setup-hook
                    #'(lambda ()
                        (define-key ido-completion-map [up] 'previous-history-element)))))

(use-package ruby-mode
  :mode ("Vagrantfile\\'" . ruby-mode))

(use-package saveplace
  :init (progn
          (setq-default save-place t))
  :config (progn
            (setq save-place-version-control t)
            (setq save-place-file
                  (conleym:persistence-dir-file "saved-places"))))

(req-package scheme
  ;; Enable eldoc-mode for scheme.
  :require (eldoc)
  :init (progn
          (add-hook 'scheme-mode-hook
                    #'turn-on-eldoc-mode)))

(req-package semantic
  :config (progn
            (setq semanticdb-default-save-directory
                  (conleym:persistence-dir-file "semanticdb/"))))

(req-package sh-script
  ;; Configure sh-mode for better zsh support.
  :init (progn
          (defun conleym:zsh-mode()
            (sh-mode)
            (sh-set-shell "zsh")))
  :mode ("\\.zsh$" . conleym:zsh-mode)
        ("^\\.zshenv$" . conleym:zsh-mode))

(req-package shell
  :require (exec-path-from-shell)
  :init (progn
          ;; Use zsh if available.
          (let ((zsh (executable-find "zsh")))
            (when zsh
              (setq explicit-shell-file-name "zsh")
              (setenv "SHELL" zsh)))))

(use-package speedbar
  :init (progn
          ;; Turn line numbers off in the speedbar buffer. Counteracts
          ;; global-linum-mode for the speedbar.
          (add-hook 'speedbar-mode-hook
                    #'(lambda()
                        (linum-mode -1))))
  :config (progn
            (setq speedbar-default-position 'left)
            (setq speedbar-show-unknown-files t)))

(use-package subword
  :init (progn
          (global-subword-mode t)))

(use-package vc-hooks
  :config (progn
            ;; Just because it's in version control doesn't mean I want no
            ;; local backups...
            (setq vc-make-backup-files t)))

(use-package which-func
  :init (progn
          (which-function-mode t)))

(provide 'conleym-builtin-req-package)
