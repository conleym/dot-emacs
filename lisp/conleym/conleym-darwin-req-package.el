(require 'req-package)
(require 'conleym-init-utils)

(if (conleym:is-darwin)
   (progn
      ;; Must have Dash.app
      (req-package dash-at-point)
      (req-package reveal-in-finder)
      ;; Seems vaguely interesting. must have 'syn' installed, which I do.
      (req-package wordsmith-mode)))

(provide 'conleym-darwin-req-package)
