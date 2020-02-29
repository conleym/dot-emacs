;; Builtin package configuration.
;;
;; All packages here come with emacs. Use :ensure nil to avoid installing over
;; them.

(require 'conleym-init-utils)


;; Expands abbreviations from a dictionary.
(use-package abbrev
  :ensure nil
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
  :ensure nil
  :delight auto-revert-mode
  :config
  ;; Default (5 seconds) is too long to wait.
  (setq auto-revert-interval 1)
  (add-hook 'dired-mode-hook #'auto-revert-mode)
  (global-auto-revert-mode))


(use-package bookmark
  :ensure nil
  :config
  (setq bookmark-default-file (conleym:persistence-dir-file "bookmarks")
        ;; Save every time the bookmarks are changed.
        bookmark-save-flag 1))


(use-package browse-url
  :ensure nil
  :config
  (if (conleym:is-darwin)
      (setq browse-url-browser-function #'browse-url-default-macosx-browser)))


(use-package desktop
  :ensure nil
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
  :ensure nil
  :config
  (setq dired-auto-revert-buffer t))


(use-package display-line-numbers
  :ensure nil
  :config
  (global-display-line-numbers-mode))


(use-package ede/base
  :defer t
  :ensure nil
  :config
  (setq ede-project-placeholder-cache-file (conleym:persistence-dir-file "ede-projects.el")))


;; Shows lisp docstrings in the minibuffer.
(use-package eldoc
  :ensure nil
  :delight
  :init
  (global-eldoc-mode))


(use-package elide-head
  :ensure nil
  :hook prog-mode)


(use-package files
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


(provide 'conleym-builtin-use-package)
