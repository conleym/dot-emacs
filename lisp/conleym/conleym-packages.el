(require 'conleym-elpa)
(require 'conleym-el-get)
(require 'package)

;; Ensure package archive contents are current to avoid 404s from
;; repositories due to rapidly-changing packages.
(package-refresh-contents)

;; Ensure req-package and its dependencies are installed.
(unless (require 'req-package "req-package" t)
  (progn
    (package-install 'req-package)))

(require 'req-package)
(require 'conleym-builtin-req-package)
(require 'conleym-req-package)
(require 'conleym-darwin-req-package)

;; Load packages, installing any that are missing.
(req-package-finish)

(provide 'conleym-packages)
