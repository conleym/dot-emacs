(require 'req-package)
(require 'conleym-init-utils)

(if (conleym:is-darwin)
   (progn
      ;; Must have Dash.app
      (req-package dash-at-point)
      (req-package reveal-in-osx-finder)))

(provide 'conleym-darwin-req-package)
