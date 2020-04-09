;; Builtin package configuration.  -*- lexical-binding: t -*-
;;
;; All packages here come with emacs.
(require 'conleym-init-utils)



;; Expands abbreviations from a dictionary.
(use-package abbrev
  :delight
  :init
  ;; Global abbrev mode. Curiously not customizable.
  (setq-default abbrev-mode t)
  :custom
  (save-abbrevs 'silently "Never ask me about saving. Just save.")
  (abbrev-file-name (conleym:persistence-dir-file "abbrev_defs") "Keep abbrev file in persistence dir.")
  :config
  ;; Load the file if it exists, don't bother me if it doesn't.
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file)))


(use-package autorevert
  :delight auto-revert-mode
  :after (dired)
  :hook (dired-mode . auto-revert-mode)
  :custom
  (auto-revert-interval 1 "Default (5 seconds) is too long to wait.")
  :config
  (global-auto-revert-mode))


(use-package bookmark
  :custom
  (bookmark-default-file (conleym:persistence-dir-file "bookmarks") "Keep bookmarks in persistence dir.")
  (bookmark-save-flag 1 "Save every time the bookmarks are changed."))


(use-package browse-url
  :config
  (if (conleym:is-darwin)
      (setq browse-url-browser-function #'browse-url-default-macosx-browser)))


(use-package cperl-mode
  ;; Replace perl-mode with cperl-mode.
  :config
  (dolist (x auto-mode-alist)
    (if (eq 'perl-mode (cdr x))
        (setf (cdr x) 'cperl-mode)))
  (dolist (x interpreter-mode-alist)
    (if (eq 'perl-mode (cdr x))
        (setf (cdr x) 'cperl-mode)))
  :mode "\\.latexmkrc\\'")


(use-package desktop
  :preface
  ;; keep desktop files in a persistence subdir. Curiously not customizable.
  (setq
   desktop-dirname (conleym:persistence-dir-file "desktop/"))
  :custom
  (desktop-save t "Always save. Never ask.")
  (desktop-save-mode t "Enable desktop saving.")
  (desktop-load-locked-desktop t "Just do it. Don't ask me.")
  (desktop-not-loaded-hook 'desktop-save-mode-off)
  (desktop-restore-eager t "Restore all the buffers immediately.")
  (desktop-path (list desktop-dirname) "Keep desktop files in the persistence dir.")
  :config
  ;; Avoid error if dir doesn't yet exist.
  (conleym:maybe-mkdir desktop-dirname))


(use-package dired
  :custom
  (dired-listing-switches "-alh" "Make file sizes human-readable.")
  (dired-auto-revert-buffer t "Always revert dired buffers."))


(use-package display-line-numbers
  :config
  (global-display-line-numbers-mode))


(use-package ede/base
  :defer t
  :custom
  (ede-project-placeholder-cache-file (conleym:persistence-dir-file "ede-projects.el") "Keep ede cache in persistence dir."))


;; Shows lisp docstrings in the minibuffer.
(use-package eldoc
  :delight
  :init
  (global-eldoc-mode))


(use-package elide-head
  :hook prog-mode)


(use-package files
  ;; Remove trailing whitespace (always) and convert tabs to spaces (usually) before saving.
  :hook (before-save . (delete-trailing-whitespace conleym:maybe-untabify-buffer))
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


(use-package gamegrid
  :defer t
  :custom
  (gamegrid-user-score-file-directory (conleym:persistence-dir-file "games/"))
  (conleym:maybe-mkdir gamegrid-user-score-file-directory))


(use-package hl-line
  :config
  (set-face-background hl-line-face "gray13")
  (global-hl-line-mode))


(use-package ido
  ;; Search recently opened files, not just currently open ones
  ;; and use flex matching.
  :custom
  (ido-enable-flex-matching t)
  (ido-max-prospects 10)
  (ido-save-directory-list-file (conleym:persistence-dir-file "ido.last"))
  (ido-use-virtual-buffers t)
  (ido-mode t)
  (ido-everywhere t))


(use-package image-dired
  :custom
  (image-dired-dir (conleym:persistence-dir-file "image-dired") "Keep thumbnails in the persistence directory."))


(use-package imenu
  :hook (after-change-major-mode . conleym:safe-imenu)
  :config
  (defun conleym:safe-imenu()
    "Try to add imenu index to the menubar, ignoring errors if imenu isn't supported in this major mode."
    (interactive)
    (require 'imenu)
    (unless (active-minibuffer-window)
      (condition-case err
          (imenu-add-menubar-index)
        (imenu-unavailable
         ;; Don't show it in the echo area. Just plop it into *Messages*.
         (let ((inhibit-message t))
           (message "%s" (error-message-string err)))))))
  ;; enable auto rescan, and sort by name.
  (setq imenu-auto-rescan t
        imenu-sort-function #'imenu--sort-by-name))


(use-package ispell
  :config
  ;; Prefer hunspell if available.
  (when (executable-find "hunspell")
    (setq-default ispell-program-name "hunspell")
    (setq-default ispell-dictionary "en_US")
    (add-to-list 'ispell-local-dictionary-alist
                 '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "'" t ("-d en_US") "~tex" utf-8)))))


(use-package js-mode
  :mode "\\.js[mx]?\\'"
  :defines js-mode-map
  :config
  ;; Can't use chord, because it doesn't yet support local keymaps.
  ;; See https://github.com/jwiegley/use-package/pull/778/files
  (key-chord-define js-mode-map ";;" "\C-e;"))


(use-package mb-depth
  :config
  (minibuffer-depth-indicate-mode 1))


(use-package nxml-mode
  ;; Major mode for editing XML.
  :defer t
  :magic  (("<\\?xml" . nxml-mode)
           ("<![dD][oO][cC][tT][yY][pP][eE]" . nxml-mode))
  :custom
  (nxml-attribute-indent tab-width)
  (nxml-child-indent tab-width)
  (nxml-slash-auto-complete-flag t))


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


(use-package recentf
  :bind ("C-x C-r" . conleym:recentf-ido-find-file)
  :custom
  (recentf-save-file (conleym:persistence-dir-file "recentf"))
  (recentf-max-menu-items 25)
  (recentf-max-saved-items 400)
  ;; Up arrow will go to previously opened files in this session if you
  ;; hit up in the minibuffer.
  :hook (ido-setup . conleym:setup-ido-with-recentf)
  :init
  ;; always on.
  (recentf-mode))


(use-package reftex
  :hook (LaTeX-mode . turn-on-reftex)
  :custom
  (reftex-plug-into-AUCTeX t))


(use-package ruby-mode
  :mode ("Vagrantfile\\'" . ruby-mode))


(use-package savehist
  :custom
  ;; I don't use this, but in case it ever gets turned on, save data in the proper place.
  (savehist-file (conleym:persistence-dir-file "savehist")))


(use-package saveplace
  :custom
  (save-place-version-control t)
  (save-place-file (conleym:persistence-dir-file "saved-places"))
  :init
  (save-place-mode +1))


(use-package semantic
  :defer t
  :custom
  (semanticdb-default-save-directory (conleym:persistence-dir-file "semanticdb/")))


(use-package server
  :config 
  (unless (or (daemonp) (server-running-p))
    (server-start)))


(use-package speedbar
  :custom
  (speedbar-default-position 'left)
  (speedbar-show-unknown-files t)
  ;; Turn line numbers off in the speedbar buffer.
  :hook (speedbar-mode . conleym:disable-display-line-numbers-mode))


(use-package sql
  :after (abbrev)
  :config
  ;; I find these abbrevs unhelpful.
  (clear-abbrev-table sql-mode-abbrev-table))


(use-package subword
  :delight
  :config
  (global-subword-mode t))


(use-package tramp
  :custom
  (tramp-default-method "ssh" "Faster than scp."))


(use-package tramp-cache
  :custom
  (tramp-persistency-file-name (conleym:persistence-dir-file "tramp")))


(use-package vc-hooks
  :custom
  (vc-make-backup-files t " Just because it's in version control doesn't mean I don't want local backups."))


(use-package wdired
  :config
  (setq wdired-allow-to-change-permissions t))


(use-package which-func
  :config
  (which-function-mode t))


(provide 'conleym-builtin-packages)
