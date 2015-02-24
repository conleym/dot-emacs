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
  (unless (file-exists-p dir)
    (mkdir dir t)))

(defun conleym:is-darwin ()
  (eq system-type 'darwin))

(defun conleym:is-mac-app ()
  (memq window-system '(mac ns)))

(provide 'conleym-init-utils)
