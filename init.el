(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(require 'modus-themes)
(require 'ido)
(require 'flymake)
(require 'compile)

(load-theme 'modus-operandi t)

;; INTERNAL VARIABLES
(setq debug-on-error nil)
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
(global-set-key (kbd "s-.") (lambda () (interactive)(find-file
						     "~/.emacs.d/init.el")))

;; Flymake ships with emacs, but flycheck might be better--investigate!
(flymake-mode)
(define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)

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
(auto-fill-mode t)
(blink-cursor-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode)
(global-eldoc-mode)
;; remap here means whatever is mapped to call dabbrev-expand should just
;; call hippie-expand instead (it's better).
(global-set-key [remap dabbrev-expand] 'hippie-expand)
;; Load image files as actual images (e.g., when find-file foo.jpeg see image)
(auto-image-file-mode)

;; (I)nteractively (Do) Mode: better find-file, buffer-find, etc.
;; TODO: add to the list of ignores for ido, like elpa/ semanticdb/ eshell/
;; auto-save-list/ .git/ node_modules/ .cache/ .vscode/ etc.

(ido-mode t)
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point 'guess)
(setq ido-create-new-buffer 'always)
(setq ido-cannot-complete-command 'ido-prev-match) ; default is a help popup
;; when building buffer list to show, but summary type buffers like *Help*
;; at the end to kind of hide them.
(add-hook 'ido-make-buffer-list-hook 'ido-summary-buffers-to-end)

;; COMPILATION / GREP
;; * control env passed to compilation command with variable
;; 'compilation-environment'
;; * usually we want 'grep-find' not just 'grep'; former reads subdirs
;; recursively, so whole tree of files from current location. 'grep' defaults to
;; current directory.
(global-set-key (kbd "<f5>") 'compile)
(global-set-key (kbd "C-<f5>") 'kill-compilation)
(global-set-key (kbd "<f6>") 'recompile)

(setq compilation-scroll-output t)
(setq compilation-always-kill t)
(setq compilation-auto-jump-to-first-error t)

;; set next/previous result/error keys (M-g M-n/p) to look for buffers with
;; error/matches in windows of current frame only, and then for a buffer
;; used previously by 'next-error' or 'previous-error'
(setq next-error-find-buffer-function 'next-error-buffer-on-selected-frame)

;; 1 means skip anything less important then a warning,
;; 2 means skip anything less important than an error, and 0 dont' skip any.
(setq compilation-skip-threshold 0)

;; in only modes stemming from prog-mode
(add-hook 'prog-mode-hook (flymake-mode))
;; flymake configuration, log-level value must be lower or equal to
;; warning-minimum-level because things not logged will not be displayed
;; in buffer either.
(setq warning-minimum-level :error)	  ; popup shows only on 'error' level
(setq warning-minimum-log-level :warning) ; log these and up to *Flymake Log*

;; TODO: buffer only so I need to toggle this only in compilation/grep buffers
;;(next-error-follow-minor-mode)

;; 'grep-find-ignored-directories' and others pulls default from here so
;; we add here to best share it around
(add-to-list 'vc-directory-exclusion-list "elpa")


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
