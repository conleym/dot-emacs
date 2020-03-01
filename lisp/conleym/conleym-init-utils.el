;;; conleym-init-utils.el --- Helpful functions used in init.

(defun conleym:dot-dir-file (file)
  "Return the path to FILE in the `user-emacs-directory'."
  (expand-file-name (concat user-emacs-directory
                            (convert-standard-filename file))))

(defun conleym:add-lisp-dir (dir)
  "Add `user-emacs-directory'/DIR and its subdirectories to `load-path'."
  (let ((default-directory (conleym:dot-dir-file dir)))
    (normal-top-level-add-to-load-path '("."))
    (normal-top-level-add-subdirs-to-load-path)))

(defun conleym:persistence-dir-file (file)
  "Return the path to FILE in the directory where Emacs's persistent data should be stored."
  (concat (conleym:dot-dir-file "persistence/") file))

(defun conleym:maybe-mkdir (dir)
  "Create DIR unless it already exists."
  (unless (file-exists-p dir)
    (mkdir dir t)))

(defun conleym:is-darwin ()
  "Is this emacs running on a darwin-based system?"
  (eq system-type 'darwin))

(defun conleym:is-mac-app ()
  "Is this emacs a macOS app?"
  (memq window-system '(ns)))

(defun conleym:disable-linum-mode ()
  "Disable linum-mode."
  (interactive)
  (linum-mode -1))

(defun conleym:disable-display-line-numbers-mode ()
  "Disable display-line-numbers-mode."
  (interactive)
  (display-line-numbers-mode -1))

(defun conleym:disable-line-numbers ()
  "Disable both linum-mode and display-line-numbers-mode."
  (interactive)
  (conleym:disable-linum-mode)
  (conleym:disable-display-line-numbers-mode))

(defun conleym:recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(defun conleym:setup-ido-with-recentf ()
  "Bind up and down arrows to history navigation in ido."
  (define-key ido-completion-map [up] 'previous-history-element)
  (define-key ido-completion-map [down] 'next-history-element))

(defun conleym:zsh-mode()
  "Shell mode with zsh syntax."
  (interactive)
  (sh-mode)
  (sh-set-shell "zsh"))

(defun conleym:untabify-buffer ()
  "Unconditionally convert tab to space in the current buffer."
  (interactive)
  (untabify (point-min) (point-max)))

(defun conleym:maybe-untabify-buffer ()
  "Convert tabs to spaces in the current buffer unless `indent-tabs-mode' is active."
  (interactive)
  (unless indent-tabs-mode
    (conleym:untabify-buffer)))

(defun conleym:safe-imenu()
    "Try to add imenu index to the menubar, ignoring errors if imenu
isn't supported in this major mode."
    (interactive)
    (require 'imenu)
    (ignore-errors
      (progn
        (imenu-add-menubar-index))))


(defun conleym:setup-tide-mode ()
  (interactive)
  (tide-setup)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  ;; slight difference from readme version because
  ;; company, flycheck, and eldoc are all globally enabled.
  (tide-hl-identifier-mode +1))

(defun conleym:fci-80-mode ()
  "Draw the fill column indicator in column 80."
  (interactive)
  (setq fci-rule-column 80) ;; becomes local when set.
  (fci-mode 1))


(provide 'conleym-init-utils)
