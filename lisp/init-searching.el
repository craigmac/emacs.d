;;; init-searching.el --- Grep, moving around, finding, etc.
;;; Commentary:
;;; Code:

(use-package anzu
  ;; UI in modeline to show in real time regex matches
  :config
  (global-anzu-mode)
  :bind
  ("M-%" . anzu-query-replace)
  ("C-M-%" . anzu-query-replace-regexp))

(use-package avy
  :config
  (setq avy-background t)
  (setq avy-style 'at-full)
  :bind ("C-j" . avy-goto-char))

(use-package rg
  :config
  (rg-enable-default-bindings))

(provide 'init-searching)
;;; init-searching.el ends here
