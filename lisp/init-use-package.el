;;; init-use-package.el --- Bootstrap use-package system.
;;; Commentary:
;;; Code:

;; From https://github.com/jwiegley/use-package#package-installation:
(eval-when-compile
  (require 'use-package))

;; Using https was causing permanent hangs on macOS although M-x eww RET
;; https://wikipedia.org RET works, so might be actually a package
;; signing/verification problem. https on Windows is also no-bueno.
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

;; use-package
;; :init keyword runs *before* package loaded, always
;; :config keyword runs *after* package loaded, whether now or lazily
(setq use-package-always-ensure t)

(provide 'init-use-package)
;;; init-use-package.el ends here
