;; Emacs will add a call to package-initialize unless we have this here.
;; (package-initialize)

(setq load-prefer-newer t)
(prefer-coding-system 'utf-8)

;; Bootstrap.
(load "~/.emacs.d/lisp/conleym/conleym-init-utils")
(require 'conleym-init-utils)

(require 'server)
(unless (server-running-p)
  (server-start))

;; Tell Emacs to look in the directory where I keep most of my stuff.
(conleym:add-lisp-dir "lisp/conleym")

;; Recompile anything that isn't current.
;; (byte-recompile-directory user-emacs-directory 0)


;; Keep themes in one place, not just cluttering ~/.emacs.d
(setq custom-theme-directory
      (conleym:dot-dir-file "themes/"))

;; Grabbed this from github, since the elpa package wants to install color-theme.
(add-to-list 'custom-theme-load-path
             (conleym:dot-dir-file "themes/emacs-color-theme-solarized/"))

;; customized settings go in a separate file.
(setq custom-file (conleym:dot-dir-file "custom-settings.el"))
(load custom-file)

;; Set dark background in terminal to match (customized) frame setting.
(set-terminal-parameter nil 'background-mode 'dark)

;; Don't ask to kill buffers with processes.
(setq kill-buffer-query-functions
      (remq #'process-kill-buffer-query-function
            kill-buffer-query-functions))

;; Make mouse work in the terminal.
;;
;; Continue to use deprecated window-system-as-boolean, since display-mouse-p
;; returns false in the terminal unless this is already enabled.
(if (not window-system)
    ;; Xterm setup
    (xterm-mouse-mode t))

;; Use y/n instead of yes/no. defalias also works here.
(fset #'yes-or-no-p #'y-or-n-p)

;; Turn the fucking bell off.
(setq ring-bell-function #'ignore)

;; Easily change text size with control + mouse wheel.
(global-set-key (kbd "<C-wheel-up>") #'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") #'text-scale-decrease)

;; Ordinarily bound to right click only, but I do this accidentally far too
;; often on the ol' trackpad.
(global-set-key (kbd "<M-mouse-3>") #'mouse-buffer-menu)

;; This fixes fn+delete when running under X11. Without this it's backspace,
;; same as plain delete.
(global-set-key [delete] #'delete-char)

;; c-backspace is backwards-kill-line
;; https://emacsredux.com/blog/2013/04/08/kill-line-backward/
(global-set-key (kbd "C-<backspace>") (lambda ()
                                        (interactive)
                                        (kill-line 0)
                                        (indent-according-to-mode)))

(defun conleym:untabify-buffer ()
  "Unconditionally convert tab to space in the current buffer."
  (interactive)
  (untabify (point-min) (point-max)))

(defun conleym:maybe-untabify-buffer ()
  "Convert tabs to spaces in the current buffer unless `indent-tabs-mode' is active."
  (interactive)
  (unless indent-tabs-mode
    (conleym:untabify-buffer)))

;; Remove trailing whitespace (always) and convert tabs to spaces (usually) before saving.
(conleym:add-functions-to-hook 'before-save-hook
                               #'delete-trailing-whitespace
                               #'conleym:maybe-untabify-buffer)


;; A variation on this using nadvice
;; https://lists.gnu.org/archive/html/emacs-devel/2010-07/msg01410.html
;; (defun conleym:advise-debug-on-error (old-function &rest arguments)
;;   (condition-case err
;;       (apply old-function arguments)
;;     ;; Let the debugger run
;;     ((debug error) (signal (car err) (cdr err)))))

;; (defun conleym:advise-debug-on-error-nosignal (old-function &rest arguments)
;;   (condition-case nil
;;     (apply old-function arguments)
;;     ((debug error) nil)))


(require 'conleym-packages)
(require 'conleym-secrets)
