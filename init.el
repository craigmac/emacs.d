(require 'package)
;; Using https was causing permanent hangs on macOS
;; although M-x eww RET https://wikipedia.org RET works,
;; so might be actually a package signing/verification problem?
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;; use-package notes:
;; :init keyword => execute code before pkg loaded; accepts 1+ forms. Should
;; only put in here what would succeed in running even if pkg not on system.
;; :config keyword => execute code after pkg load; deferred lazy load.
(eval-when-compile
  (require 'use-package))

;; DEBUG
;; C-g to debug hangs, like when calling e.g. (while t (print 'foo'))
;; or when calling synchronously an external program that never finishes
;; (toggle-debug-on-quit)

;; Debug a command causing a hang where C-g doesn't work, try again using:
;; M-x debug-on-entry RET call-process RET

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package modus-themes
  :init
  (load-theme 'modus-operandi t))

;; Ripgrep support
(use-package rg
  :config
  (rg-enable-default-bindings))

  
(use-package magit)

;; github.com/purcell/exec-path-from-shell
;; This is really important on macOS where env inheritance is wonky,
;; otherwise some env things are out of sync with terminal env, like not
;; finding 'rg' binary even when /usr/local/bin is in the exec-path.
(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))

;; maybe rebind find-file to call projectile-find-file always
(use-package projectile
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :custom
  (projectile-project-search-path '("~/git")))

(use-package diminish)

(use-package which-key
  :diminish
  :config
  (which-key-mode))

(use-package eldoc
  :diminish)

(use-package company
  :config
  (global-company-mode)
  :custom
  (company-minimum-prefix-length 2))

(use-package ace-jump-mode
  ;; creates autoload for ace-jump-mode and defers until use, then binds
  ;; a key to that command, all in one.
  ;; Literal way to do the same thing would be:
  ;; :commands ace-jump-mode
  ;; :init
  ;; (bind-key "C-." 'ace-jump-mode)
  :bind ("C-." . ace-jump-mode))

;; better M-x prompt, especially with IDO stuff enabled
(use-package smex)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; in case we want default M-x experience
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Flymake ships with emacs, but flycheck might be better--investigate!
(use-package flymake
  :config
  (flymake-mode))

;; vertical display of IDO things
(use-package ido-vertical-mode
  :config
  (ido-vertical-mode nil))

;; TODO: get this into the above use-package somehow
(define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)

(use-package compile)

;; Don't write 'M-x customize RET' set vars to this file, use this one instead
(setq custom-file "~/.emacs.d/custom-file.el")
(load-file custom-file)

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


;; SHIPPED MODES
(tool-bar-mode -1)
(scroll-bar-mode -1)
(global-visual-line-mode)
(show-paren-mode)
(transient-mark-mode)
(semantic-mode)
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
(global-hl-line-mode t)

;; (I)nteractively (Do) Mode: better find-file, buffer-find, etc.
;; TODO: add to the list of ignores for ido, like elpa/ semanticdb/ eshell/
;; auto-save-list/ .git/ node_modules/ .cache/ .vscode/ etc.
(require 'ido)
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


;; macOS
(when (eq system-type 'darwin)
  (add-to-list 'exec-path "/usr/local/bin")
  ; sets Option key on mac to be Super (defaults to Command key)
  (setq mac-option-modifier 'super)
  ; sets Command key to be Meta/Alt instead, much easier to use on mac keyboards
  (setq mac-command-modifier 'meta)
  (setq mac-control-modifier 'control)
  ;; ls on mac is not GNU so we use this to emulate, according
  ;; to help of 'dired-use-ls-dired'
  (setq ls-lisp-use-insert-directory-program nil)
  (require 'ls-lisp))

;; HOOKS
;;(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Organized using Top Level Nodes from C-h i d m Emacs<RET>
;; 1 The Organization of the Screen

(size-indication-mode)
(line-number-mode)
(column-number-mode)

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
