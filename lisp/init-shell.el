;;; init-shell.el --- Shell related stuff, eshell, etc.
;;; Commentary:
;;; Code:

;; make a shell script executable automatically on save
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

(use-package eshell-toggle
  :custom
  (eshell-toggle-size-fraction 3)
  (eshell-toggle-use-projectile-root t)
  (eshell-toggle-run-command nil)
  (eshell-toggle-init-function #'eshell-toggle-init-eshell)
  :bind
  ("C-`" . eshell-toggle))

;; company was interfering by trying to offer word completions when typing,
;; for example, typing `ls -la` would start completion on 'la' and stop you
;; from hitting enter. I can't find a way to add
;; `pcomplete-completions-at-point' to company backend/ends and have
;; it work. I'm so sick of this shit!
(add-hook 'eshell-mode-hook '(lambda () (company-mode -1)))

(provide 'init-shell)
;;; init-shell.el ends here
