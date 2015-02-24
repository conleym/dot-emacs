(require 'conleym-elpa)
(require 'conleym-el-get)

;; Ensure req-package and its dependencies are installed.
(unless (require 'req-package "req-package" t)
  (progn
    (require 'package)
    (package-refresh-contents)
    (package-install 'req-package)))

(require 'req-package)
(require 'conleym-builtin-req-package)
(require 'conleym-req-package)
(require 'conleym-darwin-req-package)

(req-package-finish)

(provide 'conleym-packages)
