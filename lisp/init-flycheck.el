;;; init-flycheck.el --- General linting config for Flycheck

;;; Commentary:
;; More specific configurations, e.g., for yaml mode, should go into
;; init-yaml.el.

;;; Code:

(setq flycheck-indication-mode nil) ; turn off fringe stuff
(add-hook 'after-init-hook #'global-flycheck-mode)
(global-unset-key (kbd "<f2>"))
(global-set-key (kbd "<f2>") 'flycheck-list-errors)
(flycheck-add-mode 'vale-craigmac 'gfm-mode)

(provide 'init-flycheck)
;;; init-flycheck.el ends here
