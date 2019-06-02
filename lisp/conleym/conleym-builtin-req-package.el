(require 'req-package)
(require 'conleym-init-utils)


(use-package abbrev
  ;; Expands abbreviations from a dictionary.
  :ensure nil
  :diminish ""
  :init
  ;; Global abbrev mode. Curiously not customizable.
  (setq-default abbrev-mode t)
  :config
  (setq save-abbrevs 'silently)
  (setq abbrev-file-name
        (conleym:persistence-dir-file "abbrev_defs"))
  ;; Load the file if it exists, don't bother me if it doesn't.
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file)))


(use-package autorevert
  :diminish auto-revert-mode
  :config
  ;; Default (5 seconds) is too long to wait.
  (setq auto-revert-interval 1)
  (add-hook 'dired-mode-hook #'auto-revert-mode)
  (global-auto-revert-mode))


(use-package bookmark
  :config
  (setq bookmark-default-file (conleym:persistence-dir-file "bookmarks")
        ;; Save every time the bookmarks are changed.
        bookmark-save-flag 1))


(use-package browse-url
  :config
  (if (conleym:is-darwin)
      (setq browse-url-browser-function #'browse-url-default-macosx-browser)))


(use-package desktop
  :config
  (setq desktop-save t
        desktop-save-mode t
        desktop-load-locked-desktop t
        desktop-not-loaded-hook 'desktop-save-mode-off
        desktop-restore-eager t
        desktop-dirname (conleym:persistence-dir-file "desktop/")
        desktop-path (list desktop-dirname))
  ;; Avoid error if dir doesn't yet exist.
  (conleym:maybe-mkdir desktop-dirname))


(req-package dired
  :ensure nil
  :config
  (setq dired-auto-revert-buffer t))


(use-package ede/base
  :defer t
  :ensure nil
  :config
  (setq ede-project-placeholder-cache-file (conleym:persistence-dir-file "ede-projects.el")))


(use-package eldoc
  ;; Shows lisp docstrings in the minibuffer.
  :diminish ""
  :init
  (global-eldoc-mode))


(use-package elide-head
  :defer t
  :init
  (add-hook 'prog-mode-hook #'elide-head))


(req-package files
  :require (exec-path-from-shell)
  :ensure nil
  :config
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
      `(( ".*" . ,backup-dir))))
  (if (conleym:is-darwin)
    (progn
      ;; Delete using Mac trash rather than freedesktop.org trash.
      (setq trash-directory "~/.Trash")
      ;; OS X ls doesn't suport --dired. Try to use GNU ls instead, if
      ;; available.
      (when-let ((gls (executable-find "gls")))
        (setq insert-directory-program gls)))))


(use-package ido
  ;; Search recently opened files, not just currently open ones
  ;; and use flex matching.
  :config
  (setq ido-enable-flex-matching t
        ido-max-prospects 10
        ido-save-directory-list-file (conleym:persistence-dir-file "ido.last")
        ido-use-virtual-buffers t)
  (ido-mode t)
  (ido-everywhere t))


(use-package ielm
  :defer t)


(use-package image-dired
  :defer t
  :config
  (setq image-dired-dir (conleym:persistence-dir-file "image-dired")))


(use-package imenu
  ;; Show definitions from current file in a menu.
  :config
  (defun conleym:safe-imenu()
    "Try to add imenu index to the menubar, ignoring errors if imenu
isn't supported in this major mode."
    (interactive)
    (require 'imenu)
    (ignore-errors
      (progn
        (imenu-add-menubar-index))))
  (add-hook 'after-change-major-mode-hook #'conleym:safe-imenu)
  (setq imenu-auto-rescan t
        imenu-sort-function #'imenu--sort-by-name))


(req-package ispell
  :require (exec-path-from-shell) ;; ispell uses {a,i,hun}spell. Need $PATH.
  :config
  ;; Prefer hunspell if available.
  (when (executable-find "hunspell")
    (setq-default ispell-program-name "hunspell")
    (setq-default ispell-dictionary "en_US")
    (add-to-list 'ispell-local-dictionary-alist
                 '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "'" t ("-d en_US") "~tex" utf-8)))))


(req-package js-mode
  :require (key-chord)
  :ensure nil
  :mode ".jsx$"
  :config
  (key-chord-define js-mode-map ";;" "\C-e;"))


(use-package nxml-mode
  ;; Major mode for editing XML.
  :defer t
  :ensure nil
  :init
  (push '("<\\?xml" . nxml-mode) magic-mode-alist)
  (push '("<![dD][oO][cC][tT][yY][pP][eE]" . nxml-mode) magic-mode-alist)
  :config
  (setq nxml-attribute-indent tab-width
        nxml-child-indent tab-width
        nxml-slash-auto-complete-flag t))


(use-package nsm
  :defer t
  :config
  (setq nsm-settings-file (conleym:persistence-dir-file "network-security.data")))


(use-package rcirc
  :defer t
  :config
  (rcirc-track-minor-mode)
  (setq rcirc-log-flag t
        rcirc-default-full-name user-full-name
        rcirc-log-directory (conleym:persistence-dir-file "rcirc-logs/")
        rcirc-time-format "%Y-%m-%d %H:%M ")
  (defun-rcirc-command j (arg)
    "Shorthand for /join."
    (interactive "M")
    (rcirc-cmd-join arg))
  (defun-rcirc-command p (arg)
    "Shorthand for /part."
    (interactive "i")
    (rcirc-cmd-part arg))
  ;; From the rcirc manual. Defines /reconnect.
  (defun-rcirc-command reconnect (arg)
    "Reconnect the server process."
    (interactive "i")
    (unless process
      (error "There's no process for this target"))
    (let* ((server (car (process-contact process)))
           (port (process-contact process :service))
           (nick (rcirc-nick process))
           channels query-buffers)
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (when (eq process (rcirc-buffer-process))
            (remove-hook 'change-major-mode-hook
                         'rcirc-change-major-mode-hook)
            (if (rcirc-channel-p rcirc-target)
                (setq channels (cons rcirc-target channels))
              (setq query-buffers (cons buf query-buffers))))))
      (delete-process process)
      (rcirc-connect server port nick
                     rcirc-default-user-name
                     rcirc-default-full-name
                     channels))))


(req-package recentf
  :require (ido)
  :bind ("C-x C-r" . conleym:recentf-ido-find-file)
  :config
  (setq recentf-save-file (conleym:persistence-dir-file "recentf")
        recentf-max-menu-items 25
        recentf-max-saved-items 400)
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
                (define-key ido-completion-map [up] 'previous-history-element)))
  :init
  ;; always on.
  (recentf-mode))


(req-package reftex
  :config
  (add-hook 'LaTeX-mode-hook #'turn-on-reftex)
  (setq reftex-plug-into-AUCTeX t))


(use-package ruby-mode
  :mode ("Vagrantfile\\'" . ruby-mode))


(use-package saveplace
  :config
  (setq-default save-place t)
  (setq save-place-version-control t
        save-place-file (conleym:persistence-dir-file "saved-places")))


(use-package semantic
  :defer t
  :defines (semanticdb-default-save-directory)
  :config
  (setq semanticdb-default-save-directory
        (conleym:persistence-dir-file "semanticdb/")))


(req-package sh-script
  ;; Configure sh-mode for better zsh support.
  :config
  (defun conleym:zsh-mode()
    (sh-mode)
    (sh-set-shell "zsh"))
  :mode ("\\.zsh$" . conleym:zsh-mode)
        ("^\\.zshenv$" . conleym:zsh-mode))


(req-package shell
  :require (exec-path-from-shell)
  :config
  ;; Use zsh if available.
  (let ((zsh (executable-find "zsh")))
    (when zsh
      (setq explicit-shell-file-name "zsh")
      (setenv "SHELL" zsh))))


(use-package speedbar
  :config
  (setq speedbar-default-position 'left
        speedbar-show-unknown-files t)
  ;; Turn line numbers off in the speedbar buffer. Counteracts
  ;; global-linum-mode for the speedbar.
  (conleym:add-functions-to-hook
   'speedbar-mode-hook
   #'conleym:disable-line-numbers))


(use-package sql
  :defer t
  :config
  (setq sql-postgres-login-params
        '((user :default "postgres")
          (port :default 5432)
          (server :default "localhost")
          (database :default "postgres")
          (password :default "postgres"))))


(use-package subword
  :diminish ""
  :config
  (global-subword-mode t))


(use-package vc-hooks
  :ensure nil
  :config
  ;; Just because it's in version control doesn't mean I want no
  ;; local backups...
  (setq vc-make-backup-files t))


(use-package wdired
  :config
  (setq wdired-allow-to-change-permissions t))


(use-package which-func
  :config
  (which-function-mode t))



(provide 'conleym-builtin-req-package)
