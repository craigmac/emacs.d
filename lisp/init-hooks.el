;;; init-hooks.el --- setup hooks for all packages
;;; Commentary:

;;; Code:

(add-hook 'text-mode-hook 'abbrev-mode)

(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; .zsh file is shell script too
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))

(add-hook 'before-save-hook #'whitespace-cleanup)

(add-hook 'text-mode-hook #'flyspell-mode)
(add-hook 'prog-mode-hook #'flyspell-prog-mode)

;; yaml inherits from text-mode but we want whitespace on there
(add-hook 'yaml-mode-hook #'whitespace-mode)
(add-hook 'prog-mode-hook #'whitespace-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(provide 'init-hooks)
;;; init-hooks.el ends here
