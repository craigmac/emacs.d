(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(require 'modus-themes)
(load-theme 'modus-operandi t)

;; INTERNAL VARIABLES
(defconst private-dir (expand-file-name "private" user-emacs-directory))
(setq user-full-name "C.D. MacEachern")
(setq user-mail-address "craigmaceachern@fastmail.com")
(setq confirm-kill-emacs 'y-or-n-p)
(setq save-interprogram-paste-before-kill t)
(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)
(setq history-length 1000)
(setq visible-bell nil)
(setq create-lockfiles nil)
(setq fill-column 80)
(setq sentence-end-double-space nil)
(setq confirm-kill-emacs 'y-or-n-p)
(setq ring-bell-function 'ignore)
(setq use-file-dialog nil)
(setq load-prefer-newer t)
(put 'narrow-to-region 'disabled nil)

;; GLOBAL KEYBINDINGS
(global-set-key (kbd "s-.") (lambda () (interactive)(find-file "~/.emacs.d/init.el")))

;; SHIPPED MODES
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-visual-line-mode)
(show-paren-mode)
(transient-mark-mode)

(semantic-mode)
(global-linum-mode)
(add-hook 'prog-mode-hook 'linum-mode)
(save-place-mode t)

;; OS-SPECIFIC
(when (eq system-type 'darwin)
  ; sets Option key on mac to be Super (defaults to Command key)
  (setq mac-option-modifier 'super)
  ; sets Command key to be Meta/Alt instead, much easier to use on mac keyboards
  (setq mac-command-modifier 'meta)
  (setq mac-control-modifier 'control))

;; HOOKS
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Organized using Top Level Nodes from C-h i d m Emacs<RET>
;; 1 The Organization of the Screen

(size-indication-mode)
(line-number-mode)
(column-number-mode)
(display-time-mode)
(setq display-time-day-and-date t)
(display-battery-mode)

;; MAIL
;; 'Mail' appears next to load level if mail setup and there is unread mail
;(setq display-time-use-mail-icon nil) ; use this to set graphical mail icon beside load

(global-unset-key (kbd "M-`")) ; turn off showing menus in echo area
(global-unset-key (kbd "<f10>")) ; usually pops up menu for key nav

;; 2 Kinds of User Input

;; macOS blocks at the system level the following:
;; M-<Tab>: do it with <Esc><Tab>, or the unblocked C-M-i (completion-at-point)
;; M-<Space>: do it with <Esc><Space>, just runs command just-one-space, so not needed
;; C-M-d: use C-M-down instead, and use C-M-up to go up



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("ddff22007104a1317014e48ff3d4911a83771a4ccf57185ccebf7f91339dbfb8" default))
 '(package-selected-packages '(modus-themes)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "White" :foreground "Black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 160 :width normal :foundry "nil" :family "JetBrains Mono NL")))))
