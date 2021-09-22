;;; init-ui.el --- UI related stuff (fonts, frames, etc.)
;;
;;; Commentary:
;;
;;; Code:

(unless (eq window-system 'n)
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-splash-screen t)

;; Remove borders from frames
(let ((no-border '(internal-border-width . 0)))
  (add-to-list 'default-frame-alist no-border)
  (add-to-list 'initial-frame-alist no-border))

;; Much better than old `linum-mode'
(setq-default display-line-numbers-width 3)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Vertical lines in the buffer, like set cc=80 in Vim
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)
(setq display-fill-column-indicator-character ?\u2502)
(setq-default fill-column 80)

(use-package all-the-icons)

(use-package diminish)

(use-package doom-modeline
  :init (doom-modeline-mode)
  :custom ((doom-modeline-height 14)))

(use-package doom-themes)

(use-package hl-todo
  :config
  (global-hl-todo-mode))

(use-package modus-themes
  :config
  (load-theme 'modus-operandi t))

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '("" invocation-name " " (:eval (if (buffer-file-name)
            (abbreviate-file-name (buffer-file-name))
          "%b"))))

(blink-cursor-mode -1)

(set-fringe-mode 10)

;; Each major mode defines their own, but for instance this turns
;; lambda string into the lambda symbol => λ when in emacs-lisp mode
(global-prettify-symbols-mode)

(provide 'init-ui)
;;; init-ui.el ends here
