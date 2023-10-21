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

;; Use y/n instead of yes/no. defalias also works here.
(fset #'yes-or-no-p #'y-or-n-p)

;; Don't ask to kill buffers with processes.
(setq kill-buffer-query-functions
      (remq #'process-kill-buffer-query-function
            kill-buffer-query-functions))

;; Turn the fucking bell off.
(setq ring-bell-function #'ignore)

;; enable emojis.
;; https://www.reddit.com/r/emacs/comments/ggd90c/color_emoji_in_emacs_27/fq0nvxc
(set-fontset-font "fontset-default" 'symbol "Apple Color Emoji")
(set-fontset-font "fontset-default" 'symbol "Noto Color Emoji" nil 'append)
(set-fontset-font "fontset-default" 'symbol "Segoe UI Emoji" nil 'append)
(set-fontset-font "fontset-default" 'symbol "Symbola" nil 'append)


(require 'conleym-packages)
(require 'conleym-secrets)
(require 'conleym-keys)
