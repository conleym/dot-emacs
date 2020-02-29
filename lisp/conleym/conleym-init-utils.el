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

(defun conleym:add-functions-to-hook (hook &rest functions)
  "Add multiple FUNCTIONS to a single HOOK."
  (mapcar (lambda (x) (add-hook hook x)) functions))

(defun conleym:add-function-to-hooks (function &rest hooks)
  "Add a single FUNCTION to multiple HOOKS."
  (mapcar (lambda (x) (add-hook x function)) hooks))

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
  ""
  (define-key ido-completion-map [up] 'previous-history-element)
  (define-key ido-completion-map [down] 'next-history-element))




(provide 'conleym-init-utils)
