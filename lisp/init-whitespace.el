;;; init-whitespace.el --- Config for whitespace display and modes
;;; Commentary:
;;; Code:

(require 'whitespace)
(add-hook 'before-save-hook #'whitespace-cleanup)

;; yaml inherits from text-mode but we want whitespace on there
(add-hook 'yaml-mode-hook #'whitespace-mode)
(add-hook 'prog-mode-hook #'whitespace-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; this is used when whitespace-style contains 'tab-mark',
;; 'space-mark', or 'newline-mark'
(setq whitespace-display-mappings
      '(
        (space-mark   ?\     [?·]     [?.])     ; space
        (space-mark   ?\xA0  [?·]     [?.])     ; hard space
        (newline-mark ?\n    [?↵ ?\n] [?$ ?\n]) ; eol
        (tab-mark     ?\t    [?» ?\t] [?\\ ?\t]) ; tab
        ))

(provide 'init-whitespace)
;;; init-whitespace.el ends here

