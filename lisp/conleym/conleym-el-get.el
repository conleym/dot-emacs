(require 'conleym-init-utils)

; Define vars to shut the byte compiler up. Most of these can be customized,
; but we want them to be in place when el-get is installed.
(defvar el-get-dir)
(defvar el-get-recipe-path)
(defvar el-get-git-shallow-clone)
(defvar el-get-user-package-directory)

;; Grab only the most recent version when cloning, not the whole repository.
(setq el-get-git-shallow-clone t)

(let* ((base-dir (conleym:dot-dir-file "lisp/el-get/"))
       (load-dir (concat base-dir "el-get/"))
       (user-dir (concat base-dir "el-get-user/")))
  (setq el-get-dir base-dir)
  (add-to-list 'load-path load-dir)
  (unless (require 'el-get nil 'noerror)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (add-to-list 'el-get-recipe-path (concat user-dir "recipies/"))
  (setq el-get-user-package-directory (concat user-dir "init-files/")))

(el-get 'sync)

(provide 'conleym-el-get)
