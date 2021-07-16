(eval-when-compile
   (require 'use-package))

;; Using https was causing permanent hangs on macOS
;; although M-x eww RET https://wikipedia.org RET works,
;; so might be actually a package signing/verification problem?
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

(setq use-package-always-ensure t)

(use-package modus-themes
  :config
  (load-theme 'modus-operandi t))

(use-package magit
  :ensure t)

(use-package yaml-mode)

(use-package rg
  :config
  (rg-enable-default-bindings))

(use-package exec-path-from-shell
  :config
  ;; macOS GUI env inheritance is odd: this fixes env being out of sync
  ;; with terminal env, like not finding binaries that it should
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package diminish)

(use-package which-key
  :config
  (which-key-mode))

(use-package eldoc
  :config
  (global-eldoc-mode))

(use-package projectile
  :config
  (setq projectile-project-search-path '("~/git"))
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
	      ("s-p" . projectile-find-file)
	      ("C-c p" . projectile-command-map)))

(use-package ace-jump-mode
  :config
  :bind ("C-." . ace-jump-mode))

(use-package markdown-mode)

;; Completion/Selection framework stack:

;; orderless: a completion style, which extends or replaces the built-in list of pattern
;;            matching ’completion-styles’ native to Emacs
;; embark:    visualizes the list of completion candidates, as well as provides actions
;;            we can perform on them, on a per-item or per-set basis. Like
;;            a right-click functionality to work with candidates.
;; selectrum: front-end to display candidates vertically; replaces minibuffer default
;; prescient: filters mru completions, etc. to top of list
;; consult:   enhances several minibuffer-centric commands and provides new ones
;; marginalia: provides meta-information to various completion lists

(use-package orderless
  :config
  ;; Uses Emacs’ completion-styles
  (setq completion-styles '(orderless)))

(use-package embark
  :bind
  (("M-RET" . embark-act))
  :init
  ;; optionally replace the key help with a completion-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; hide mode line of embark live/completion buffers
  (add-to-list 'display-buffer-alist
	       '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
		 nil
		 (window-parameters (mode-line-format . none)))))

;; this not working until
;; https://github.com/oantolin/embark/commit/22875aa5bda21b588487b719982cbaf8410830da
;; merges.
;; use which-key popup instead of C-h to show available actions
;; after running embark-act
;; (setq embark-action-indicator
;;       (lambda (map _target)
;;         (which-key--show-keymap "Embark" map nil nil 'no-paging)
;;         #'which-key--hide-popup-ignore-command)
;;       embark-become-indicator embark-action-indicator)

;; used with consult
(use-package embark-consult
  :after (embark consult)
  :demand t ;; necessary with hook below
  ;; gives consult previews as you move around auto-updating collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package marginalia
  :config
  (marginalia-mode))

(use-package selectrum
  :config
  (selectrum-mode +1))

;; better filtering and sorting according to selectrum maintainer
(use-package selectrum-prescient
  :config
  (selectrum-prescient-mode +1)
  ;; save cmd history to disk, better sorting over time
  (prescient-persist-mode +1))

(use-package consult
  :config
  (global-set-key (kbd "<f7>") 'consult-outline)
  (global-set-key [C-tab] 'consult-buffer)
  (global-set-key (kbd "C-x C-r") 'consult-recent-file))

(use-package company
  :init
  (setq company-minimum-prefix-length 2)
  :config
  (global-company-mode))

(use-package flycheck
   :init
   ;; https://github.com/DavidAnson/markdownlint-cli2
   (setq flycheck-markdown-markdownlint-cli-executable "markdownlint-cli2")
   :config
   (global-set-key (kbd "<f9>") 'flycheck-mode)
   (define-key flycheck-mode-map (kbd "M-n") 'flycheck-next-error)
   (define-key flycheck-mode-map (kbd "M-p") 'flycheck-previous-error))

;; ------------------ EMACS SETTINGS ----------------------------
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
(setq make-backup-files nil)
(put 'narrow-to-region 'disabled nil)
(setq gc-cons-threshold 20000000) 	; allow 20MB of memory before garbage collection
(setq sentence-end-double-space nil)	; sentence begins with single space!
(fset 'yes-or-no-p 'y-or-n-p)		; easier yes/no with y/n instead

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
;; call hippie-expand instead (it's better). Rare cases I use this.
(global-set-key [remap dabbrev-expand] 'hippie-expand)
;; Load image files as actual images (e.g., when find-file foo.jpeg see image)
(auto-image-file-mode)
(size-indication-mode)
(line-number-mode)
(column-number-mode)


;; COMPILATION / GREP
;; * control env passed to compilation command with variable
;; 'compilation-environment'
;; * usually we want 'grep-find' not just 'grep'; former reads subdirs
;; recursively, so whole tree of files from current location. 'grep' defaults to
;; current directory.
(global-set-key (kbd "<f5>") 'compile)
(global-set-key (kbd "C-<f5>") 'kill-compilation)
(global-set-key (kbd "<f6>") 'recompile)

(require 'compile)
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

;; 'grep-find-ignored-directories' and others pulls default from here so
;; we add here to best share it around
(add-to-list 'vc-directory-exclusion-list "elpa")

;; macOS
;;(when (eq system-type 'darwin)
;;  (add-to-list 'exec-path "/usr/local/bin"))

(global-unset-key (kbd "M-`")) ; turn off showing menus in echo area
(global-set-key (kbd "M-`") 'shell)
(global-unset-key (kbd "<f10>")) ; usually pops up menu for key nav


;; macOS blocks at the system level the following:
;; M-<Tab>: do it with <Esc><Tab>, or the unblocked C-M-i (completion-at-point)
;; M-<Space>: do it with <Esc><Space>, just runs command just-one-space, so not needed
;; C-M-d: use C-M-down instead, and use C-M-up to go up

(defun open-line-above ()
  "Opens a line above the current line."
  (interactive)
  (forward-line -1)
  (open-line 1)
  (forward-line 2))


;; mimic VS Code open line below/above keys
(global-set-key (kbd "<s-return>") '()) ; Code this opens line below and puts
(global-set-key (kbd "<S-s-return>") 'open-line-above)



;; Replace ’typewriter’ style straight quotes of various kinds with the
;; appropriate and nicer “curved” variants.
;; Replace single quote with ’ and double straight quote with “curlies”,
;; and replace single backtick ` with ‘, and two `` with “.
;; Only occurs in comments, strings, and text paragraphs.
;; For Markdown trying to put in three ``` becomes “‘ so either prefix each
;; with C-q, or turn off replacing that one in Markdown documents.
;; (electric-quote-mode)
;; (add-hook 'markdown-mode-hook
;; 	  default is ’(8216 8217 8220 8221): meaning
;; 	  backtick ` becomes ‘
;; 	  straight single quote ' becomes ’
;; 	  left double " becomes “
;;           right double " becomes ”
;;           what I want is to leave ` chars alone to make code fences
;; 	  (setq electric-quote-char TODO))

;; from protesilaos:
(setq cmac-simple-insert-pair-alist
      '(("' Single quote"        . (39 39))     ; ' '
        ("\" Double quotes"      . (34 34))     ; " "
          ("` Elisp quote"         . (96 39))     ; ` '
          ("‘ Single apostrophe"   . (8216 8217)) ; ‘ ’
          ("“ Double apostrophes"  . (8220 8221)) ; “ ”
          ("( Parentheses"         . (40 41))     ; ( )
          ("{ Curly brackets"      . (123 125))   ; { }
          ("[ Square brackets"     . (91 93))     ; [ ]
          ("< Angled brackets"     . (60 62))     ; < >
          ("= Equals signs"        . (61 61))     ; = =
          ("~ Tilde"               . (126 126))   ; ~ ~
          ("* Asterisks"           . (42 42))     ; * *
          ("_ underscores"         . (95 95))))   ; _ _

;; TODO:
;; flyspell mode binding to <f10> and install ispell or something
