(setq load-prefer-newer t)

;; Bootstrap.
(require 'package)
(package-initialize)
(load "~/.emacs.d/lisp/conleym/conleym-init-utils")
(require 'conleym-init-utils)

;; Tell Emacs to look in the directory where I keep most of my stuff.
(conleym:add-lisp-dir "lisp/conleym")

;; Recompile anything that isn't current.
(byte-recompile-directory user-emacs-directory 0)

;; Make mouse work in the terminal.
(if (not window-system)
    ;; Xterm setup
    (xterm-mouse-mode 1))

(if (conleym:is-darwin)
    (progn 
      ;; Delete using Mac trash rather than freedesktop.org trash.
      (setq trash-directory "~/.Trash")
      ;; OS X ls doesn't suport --dired
      (let ((ls "/opt/local/bin/gls")) ;; macports GNU ls
        (if (file-executable-p ls)
            (setq insert-directory-program ls)))))

;; Use y/n instead of yes/no. defalias also works here.
(fset 'yes-or-no-p 'y-or-n-p)

;; Turn the fucking bell off.
(setq ring-bell-function 'ignore)

;; Get rid of "For information about..." minibuffer message on startup.
;;
;; No, guys, your startup message is *NOT* important enough to warrant your
;; extreme measures. Whatever machine I'm on, whatever my username is, I don't
;; need to see it.
(put 'inhibit-startup-echo-area-message 'saved-value t)
(setq inhibit-startup-echo-area-message (user-login-name))
;; Remove the symbol table crap from inhibit-startup-echo-area-message.
;; Customize will write invalid data if it's a plist, so we *need* to
;; do this. emacs-startup-hook runs AFTER the message would be displayed.
(add-hook 'emacs-startup-hook
          #'(lambda()
              (put 'inhibit-startup-echo-area-message 'saved-value nil)))

;; customized settings go in a separate file.
(setq custom-file (conleym:dot-dir-file "custom.el"))
(load custom-file)

(require 'conleym-packages)
(require 'conleym-secrets)
