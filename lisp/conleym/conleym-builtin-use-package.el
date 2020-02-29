;; Builtin package configuration.
;;
;; All packages here come with emacs.
(require 'conleym-init-utils)



;; Expands abbreviations from a dictionary.
(use-package abbrev
  :delight
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
  :delight auto-revert-mode
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


(use-package dired
  :config
  (setq dired-auto-revert-buffer t))


(use-package display-line-numbers
  :config
  (global-display-line-numbers-mode))


(use-package ede/base
  :defer t
  :config
  (setq ede-project-placeholder-cache-file (conleym:persistence-dir-file "ede-projects.el")))


;; Shows lisp docstrings in the minibuffer.
(use-package eldoc
  :delight
  :init
  (global-eldoc-mode))


(use-package elide-head
  :hook prog-mode)


(use-package files
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
  :config
  (setq gamegrid-user-score-file-directory
        (conleym:persistence-dir-file "games/"))
  (conleym:maybe-mkdir gamegrid-user-score-file-directory))


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


(use-package image-dired
  :config
  (setq image-dired-dir (conleym:persistence-dir-file "image-dired")))


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
  :chords ((";;" . "\C-e;"))
)



(provide 'conleym-builtin-use-package)
