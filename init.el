(prefer-coding-system 'utf-8)

;; Bootstrap.
(load "~/.emacs.d/lisp/conleym/conleym-init-utils")
(require 'conleym-init-utils)

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
;; Note: display-mouse-p returns false in the terminal unless this is already
;; enabled, so cannot be used here.
(unless (display-graphic-p)
  ;; Xterm setup
  (xterm-mouse-mode 1)
  (global-set-key (kbd "<mouse-4>") #'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") #'scroll-up-line))

;; Use y/n instead of yes/no. defalias also works here.
(fset #'yes-or-no-p #'y-or-n-p)

;; Turn the fucking bell off.
(setq ring-bell-function #'ignore)

;; Easily change text size with control + mouse wheel.
(global-set-key (kbd "<C-wheel-up>") #'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") #'text-scale-decrease)

;; Ordinarily bound to right click only, but I do this accidentally far too
;; often on the ol' trackpad.
(global-set-key (kbd "<C-M-mouse-3>") #'mouse-buffer-menu)

;; This fixes fn+delete when running under X11. Without this it's backspace,
;; same as plain delete.
(global-set-key [delete] #'delete-char)

(global-set-key (kbd "C-x C-b") #'ibuffer)

;; c-backspace is backwards-kill-line
;; https://emacsredux.com/blog/2013/04/08/kill-line-backward/
(defun backward-kill-line ()
  (interactive)
  (kill-line 0)
  (indent-according-to-mode))

(global-set-key (kbd "C-<backspace>") #'backward-kill-line)

;; enable emojis.
;; https://www.reddit.com/r/emacs/comments/ggd90c/color_emoji_in_emacs_27/fq0nvxc
(set-fontset-font "fontset-default" 'symbol "Apple Color Emoji")
(set-fontset-font "fontset-default" 'symbol "Noto Color Emoji" nil 'append)
(set-fontset-font "fontset-default" 'symbol "Segoe UI Emoji" nil 'append)
(set-fontset-font "fontset-default" 'symbol "Symbola" nil 'append)

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
