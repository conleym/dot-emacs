;; Read eshell aliases from the shell.  -*- lexical-binding: t -*-


;; Variation on https://www.emacswiki.org/emacs/EshellAlias#toc11, with bits of exec-path-from-shell.
(defun conleym:eshell-read-aliases ()
  "Read aliases from the shell itself."
  (let ((shell (or shell-file-name (getenv "SHELL"))))
    (with-temp-buffer
      (let ((exit-code (call-process shell nil t nil '"-l" "-i" "-c" "alias"))
            (result (buffer-string)))
        (unless (zerop exit-code)
          (error "Nonzero exit code (%s) from shell %s: output was %s"
                 exit-code shell result))
        result))))


(defun conleym:eshell-aliases-from-shell ()
  "Read aliases from the shell and return a list of eshell definitions."
  (let* ((shell-output (conleym:eshell-read-aliases))
         (lines (split-string shell-output "\n" t)))
    (mapcar #'(lambda (line)
                (let* ((split (split-string line "=" t))
                       (name (car split))
                       (value (concat
                               ;; Remove any surrounding single quotes
                               (replace-regexp-in-string "^'\\(.*\\)'$" "\\1" (string-join (cdr split) "="))
                               ;; Append $* so eshell aliases respect args, if any.
                               " $*")))
                  (list name value)))
            lines)))


(defun conleym:copy-shell-aliases-to-eshell ()
  "Read shell aliases and insert them into the list of eshell aliases."
  (eval-when-compile
    (require 'cl-lib)
    (require 'em-alias))
  (setq eshell-command-aliases-list (cl-union eshell-command-aliases-list (conleym:eshell-aliases-from-shell))))



(provide 'conleym-aliases-from-shell)
