;;; init-yaml.el --- YAML file setup.
;;; Commentary:
;;; Code:

(use-package yaml-mode)

;; yaml-mode inherits text-mode but we don't want flyspell-mode on
;; there, just for comment strings/docs.
(add-hook 'yaml-mode-hook 'flyspell-prog-mode)

(provide 'init-yaml)
;;; init-yaml.el ends here
