;; -*- lexical-binding: t; -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backup-by-copying t)
 '(blink-cursor-blinks 0 nil nil "Blink forever")
 '(column-number-mode t)
 '(compilation-ask-about-save nil nil nil "Prompts are annoying. Of course I want to save the file before compiling it.")
 '(cua-mode t nil (cua-base))
 '(custom-enabled-themes '(wheatgrass))
 '(custom-safe-themes
   '("8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26"
     default))
 '(delete-by-moving-to-trash t)
 '(delete-old-versions t nil nil "We're keeping a lot of backups. No need to ask about getting rid of older ones.")
 '(eimp-ignore-read-only-modes
   '(gnus-article-mode puzzle-mode tumme-display-image-mode
                       tumme-thumbnail-mode w3m-mode xkcd-mode))
 '(enable-recursive-minibuffers t)
 '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)
 '(frame-background-mode 'dark)
 '(gc-cons-threshold 20000000 nil nil "Suggestion from flx-ido author on improving performance: GC every 20MB instead of every .76MB.")
 '(global-linum-mode nil)
 '(gnutls-min-prime-bits 1024)
 '(history-length t)
 '(indent-tabs-mode nil nil nil "Tabs are for suckers, and I'm no sucker.")
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(large-file-warning-threshold 200000000 nil nil "Default (10 MB) is way too small. Let's try 200 MB instead.")
 '(mac-pseudo-daemon-mode t)
 '(midnight-mode t nil (midnight))
 '(minibuffer-prompt-properties
   '(read-only t point-entered minibuffer-avoid-prompt face
               minibuffer-prompt) nil nil "Make minbuffer prompt read only (default) and don't allow the cursor into it (not default).")
 '(mouse-drag-and-drop-region-cross-program t nil nil "Allow dragging text to other programs.")
 '(mouse-drag-copy-region 'non-empty nil nil "Only save mouse-selected text to kill ring if region is nonempty.")
 '(mouse-drag-mode-line-buffer t nil nil "Allows dragging of the filename from the modeline to other programs.")
 '(mouse-yank-at-point t)
 '(ns-pop-up-frames nil nil nil "Documents opened via Mac's open command show up in the current frame, not a new one.")
 '(package-enable-at-startup nil)
 '(package-selected-packages nil)
 '(require-final-newline t)
 '(save-interprogram-paste-before-kill t)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(show-paren-style 'expression)
 '(size-indication-mode t)
 '(tab-width 4 nil nil "Who the hell uses a tab width of 8 (the default)?")
 '(tool-bar-mode nil)
 '(tramp-syntax 'default nil (tramp))
 '(use-dialog-box nil nil nil "The dialog box is annoying and, in some situations, works incorrectly on Mac.")
 '(use-package-enable-imenu-support t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
