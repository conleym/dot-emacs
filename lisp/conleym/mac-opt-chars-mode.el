;; mac-opt-chars-mode  -*- lexical-binding: t -*-
;;
;; Minor mode based on
;; https://www.reddit.com/r/emacs/comments/mpbgx7/typing_accented_characters_in_emacs_on_macos/gu9opv1/


(defvar mac-opt-alternate-alt-x)
(defvar mac-opt-keymap)
(defvar mac-opt-chars-mode)

;; equivalent to C-M-x with mac-opt-chars-mode on.
(setq mac-opt-alternate-alt-x (kbd "C-≈"))

(setq mac-opt-keymap (make-sparse-keymap))

(defun mac-toggle-ns-alt-modifier ()
  "Toggle mac-opt-chars-mode."
  (if (not mac-opt-chars-mode)
      (setq ns-alternate-modifier 'meta)
    (progn
      ;; Whenever the mode is activated, ensure C-M-x is an alias for whatever
      ;; command is invoked by M-x.
      (define-key mac-opt-keymap mac-opt-alternate-alt-x (key-binding (kbd "M-x")))
      (setq ns-alternate-modifier nil))))

(define-minor-mode mac-opt-chars-mode
  "Type characters with option as in other Mac applications."
  :global t
  :lighter " mac-opt-chars"
  :keymap mac-opt-keymap
  (mac-toggle-ns-alt-modifier))


(provide 'mac-opt-chars-mode)
