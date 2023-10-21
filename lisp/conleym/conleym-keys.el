;; Key bindings and supporting functions. -*- lexical-binding: t -*-

(require 'mac-opt-chars-mode)

;; c-backspace is backwards-kill-line
;; https://emacsredux.com/blog/2013/04/08/kill-line-backward/
(defun backward-kill-line ()
  (interactive)
  (kill-line 0)
  (indent-according-to-mode))

(global-set-key (kbd "C-<backspace>") #'backward-kill-line)


;; This fixes fn+delete when running under X11. Without this it's backspace,
;; same as plain delete.
(global-set-key [delete] #'delete-char)


;; Make mouse work in the terminal.
;;
;; Note: display-mouse-p returns false in the terminal unless this is already
;; enabled, so cannot be used here.
(unless (display-graphic-p)
  ;; Xterm setup
  (xterm-mouse-mode 1)
  (global-set-key (kbd "<mouse-4>") #'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") #'scroll-up-line))


;; Easily change text size with control + mouse wheel.
(global-set-key (kbd "<C-wheel-up>") #'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") #'text-scale-decrease)


;; Ordinarily bound to right click only, but I do this accidentally far too
;; often on the ol' trackpad.
(global-set-key (kbd "<C-M-mouse-3>") #'mouse-buffer-menu)



(provide 'conleym-keys)
