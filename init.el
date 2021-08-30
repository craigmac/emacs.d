;;; init.el --- craigmac Emacs configuration
;;
;;; Commentary:
;;
;; This is my personal Emacs configuration.  Nothing more, nothing less.
;;
;;; License:
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

;; From https://github.com/jwiegley/use-package#package-installation:
(eval-when-compile
  (require 'use-package))

;; Using https was causing permanent hangs on macOS although M-x eww RET
;; https://wikipedia.org RET works, so might be actually a package
;; signing/verification problem. https on Windows is also no-bueno.
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)

;; use-package
;; :init keyword runs *before* package loaded, always
;; :config keyword runs *after* package loaded, whether now or lazily
(setq use-package-always-ensure t)

(use-package all-the-icons)

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

(use-package company
  :init
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-limit 10)
  ;;  (setq company-tooltip-align-annotations t)
  ;; when near bottom of window, flip to fix it
  ;;  (setq company-tooltip-flip-when-above t)

  :config
  (global-company-mode))

(use-package consult
  ;; enhances several minibuffer-centric commands and provides new ones
  :config
  (global-set-key (kbd "<f7>") 'consult-outline)
  (global-set-key [C-tab] 'consult-buffer)
  (global-set-key (kbd "C-x C-r") 'consult-recent-file))

;; Useful functions that are included in Emacs Prelude kit.
(use-package crux)

(use-package doom-modeline
  :init (doom-modeline-mode)
  :custom ((doom-modeline-height 14)))

(use-package doom-themes)

(use-package diminish)

(use-package editorconfig
  :config
  (editorconfig-mode))

(use-package eldoc
  :config
  (global-eldoc-mode))

(use-package embark
  ;; visualizes the list of completion candidates, as well as provides actions
  ;; we can perform on them, on a per-item or per-set basis. Like
  ;; a right-click functionality to work with candidates.
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

(use-package embark-consult
  ;; used with consult
  :after (embark consult)
  :demand t ;; necessary with hook below
  ;; gives consult previews as you move around auto-updating collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package exec-path-from-shell
  :config
  ;; macOS GUI env inheritance is odd: this fixes env being out of sync
  ;; with terminal env, like not finding binaries that it should
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package diff-hl
  :after (magit)
  :config
  (diff-hl-margin-mode)
  (global-diff-hl-mode)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package flycheck
  :init
  (with-no-warnings
    ;; https://github.com/DavidAnson/markdownlint-cli2
    (setq flycheck-markdown-markdownlint-cli-executable "markdownlint-cli2"))
  :config
  ;; (setq-default flycheck-indication-mode 'right-fringe)
  (global-set-key (kbd "<f9>") 'flycheck-mode)
  (define-key flycheck-mode-map (kbd "M-n") 'flycheck-next-error)
  (define-key flycheck-mode-map (kbd "M-p") 'flycheck-previous-error))

(use-package git-timemachine)

(use-package helpful
  :config
  ;; Note that the built-in 'describe-function' includes both functions
  ;; and macros. 'helpful-function' is functions only, so we provide
  ;; 'helpful-callable' as a drop-in replacement.
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  ;; Lookup the current symbol at point. C-c C-d is a common keybinding
  ;; for this in lisp modes.
  (global-set-key (kbd "C-c C-d") #'helpful-at-point)
  ;; Look up *F*unctions (excludes macros).
  ;;
  ;; By default, C-h F is bound to `Info-goto-emacs-command-node'. Helpful
  ;; already links to the manual, if a function is referenced there.
  (global-set-key (kbd "C-h F") #'helpful-function)
  ;; Look up *C*ommands.
  ;;
  ;; By default, C-h C is bound to describe `describe-coding-system'. I
  ;; don't find this very useful, but it's frequently useful to only
  ;; look at interactive functions.
  (global-set-key (kbd "C-h C") #'helpful-command))

(use-package hl-todo
  :config
  (global-hl-todo-mode))

(use-package magit)

(use-package marginalia
  ;; provides meta-information to various completion lists, like doc strings
  ;; to the mini-buffer list when using M-x
  :config
  (marginalia-mode))

(use-package markdown-mode)

(use-package modus-themes
  :config
  (load-theme 'modus-operandi t))

(use-package orderless
  ;; orderless: a completion style, which extends or replaces the built-in list of pattern
  ;;            matching ’completion-styles’ native to Emacs
  :config
  ;; Uses Emacs’ completion-styles
  (setq completion-styles '(orderless)))

(use-package projectile
  :config
  (setq projectile-project-search-path '("~/git"))
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
	      ("s-p" . projectile-find-file)
	      ("C-c p" . projectile-command-map)))

(use-package selectrum
  ;; selectrum: front-end to display candidates vertically; replaces minibuffer default
  :config
  (selectrum-mode +1))

(use-package selectrum-prescient
  ;; better filtering and sorting according to selectrum maintainer.
  ;; filters MRU completions, etc. to top of selection list
  :config
  (selectrum-prescient-mode +1)
  ;; save cmd history to disk, better sorting over time
  (prescient-persist-mode +1))

(use-package smartparens
  :config
  (require 'smartparens-config)
  (setq sp-base-key-bindings 'paredit)
  (setq sp-autoskip-closing-pair 'always)
  (setq sp-hybrid-kill-entire-symbol nil)
  (sp-use-paredit-bindings)
  (show-smartparens-global-mode +1)
  (setq blink-matching-paren nil))

(use-package rg
  :config
  (rg-enable-default-bindings))

(use-package undo-tree
  ;; visual undo for easy jumping back and forth between history
  :config
  ;; autosave the undo-tree history
  (setq undo-tree-history-directory-alist
	`((".*" . ,temporary-file-directory)))
  (setq undo-tree-auto-save-history t)
  (global-undo-tree-mode))

(use-package which-key
  :config
  (which-key-mode))

(use-package yaml-mode)

;; SETTINGS
(setq apropos-do-all t)
(setq mouse-yank-at-point t)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; Used througout config, was 'prelude-savefile-dir' originally.
(setq cdm-savefile-dir (expand-file-name "savefile" user-emacs-directory))
(setq custom-file "~/.emacs.d/custom.el") ; 'M-x customize' vars go here instead
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
(setq gc-cons-threshold 20000000) 	; allow 20MB of memory before garbage collection
(setq sentence-end-double-space nil)	; sentence begins with single space!
(fset 'yes-or-no-p 'y-or-n-p)		; easier yes/no with y/n instead
;; enable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
;; dired - reuse current buffer by pressing 'a'
(put 'dired-find-alternate-file 'disabled nil)

;; enabled change region case commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; enable erase-buffer command
(put 'erase-buffer 'disabled nil)

;; Death to the tabs!  However, tabs historically indent to the next
;; 8-character offset; specifying anything else will cause *mass*
;; confusion, as it will change the appearance of every existing file.
;; In some cases (python), even worse -- it will change the semantics
;; (meaning) of the program.
;;
;; Emacs modes typically provide a standard means to change the
;; indentation width -- eg. c-basic-offset: use that to adjust your
;; personal indentation width, while maintaining the style (and
;; meaning) of any files you load.
(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq-default tab-width 8)            ;; but maintain correct appearance

;; Newline at end of file
(setq require-final-newline t)

;; hippie expand is dabbrev expand on steroids
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

;; indent line if not indented already, otherwise complete thing at point
(setq tab-always-indent 'complete)

;; nicer scrolling
(setq scroll-conservatively 100000
      scroll-preserve-screen-position t) ; keep point in place, e.g., C/M-v

;; Built-in packages (not GNU/MELPA)
(unless (eq window-system 'n)
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

(global-visual-line-mode)
(show-paren-mode)
(transient-mark-mode)
(semantic-mode)
(save-place-mode t)
(auto-fill-mode t)
(blink-cursor-mode -1)
(global-auto-revert-mode)
(global-eldoc-mode)
;; remap here means whatever is mapped to call dabbrev-expand should just
;; call hippie-expand instead (it's better). Rare cases I use this.
(global-set-key [remap dabbrev-expand] 'hippie-expand)
;; Load image files as actual images (e.g., when find-file foo.jpeg see image)
(auto-image-file-mode)			; open .png as image not text buffer
(size-indication-mode)			; e.g., 60:8 49% in modeline
(column-number-mode)
;;(set-fringe-mode 10)
(delete-selection-mode)
;; enable winner-mode to manage window configurations.
;; C-c Left/Right to undo/redo last window change
(winner-mode)
(fset 'yes-or-no-p 'y-or-n-p)

;; meaningful names for buffers with the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers



(defun craigmac-jekyll-markdown-mode ()
    "Minor mode setup for Jekyll Liquid Markdown buffers."
    (flyspell-mode)
    (whitespace-mode)
    (abbrev-mode)
    (setq-local tab-width 3))
(add-hook 'markdown-mode-hook 'craigmac-jekyll-markdown-mode)

;; M-x sudo to invoke TRAMP to edit current file as root hack
(defun sudo()
  "Use TRAMP to `sudo' the current buffer"
  (interactive)
  (when buffer-file-name
    (find-alternate-file
     (concat "/sudo:root@localhost:"
	     buffer-file-name))))
;; .zsh file is shell script too
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))


;; saveplace remembers your location in a file when saving files
(setq save-place-file (expand-file-name "saveplace" cdm-savefile-dir))
;; activate it for all buffers
(save-place-mode)

;; savehist keeps track of some history
(require 'savehist)
(setq savehist-additional-variables
      ;; search entries
      '(search-ring regexp-search-ring)
      ;; save every minute
      savehist-autosave-interval 60
      ;; keep the home clean
      savehist-file (expand-file-name "savehist" cdm-savefile-dir))
(savehist-mode)

;; save recent files
(require 'recentf)
(setq recentf-save-file (expand-file-name "recentf" cdm-savefile-dir)
      recentf-max-saved-items 500
      recentf-max-menu-items 15
      ;; disable recentf-cleanup on Emacs start, because it can cause
      ;; problems with remote files
      recentf-auto-cleanup 'never)

(defun cdm-recentf-exclude-p (file)
  "A predicate to decide whether to exclude FILE from recentf."
  (let ((file-dir (file-truename (file-name-directory file))))
    (cl-some (lambda (dir)
               (string-prefix-p dir file-dir))
             (mapcar 'file-truename (list cdm-savefile-dir package-user-dir)))))

(add-to-list 'recentf-exclude 'cdm-recentf-exclude-p)

(recentf-mode +1)

;; use shift + arrow keys to switch between visible buffers
(require 'windmove)
(windmove-default-keybindings)

;; tramp, for sudo access
(require 'tramp)
;; keep in mind known issues with zsh - see emacs wiki
(setq tramp-default-method "ssh")

(set-default 'imenu-auto-rescan t)


;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(require 'compile)
(setq compilation-scroll-output t	; scroll compile buffer as it grows
      compilation-ask-about-save nil	; just save and compile don't ask
      compilation-always-kill t		; kill old compile proc before next one
      compilation-auto-jump-to-first-error t) ; see 'compilation-skip-threshold'
(global-set-key (kbd "<f5>") 'compile)
(global-set-key (kbd "C-<f5>") 'kill-compilation)
(global-set-key (kbd "<f6>") 'recompile)

;; set next/previous result/error keys (M-g M-n/p) to look for buffers with
;; error/matches in windows of current frame only, and then for a buffer
;; used previously by 'next-error' or 'previous-error'
(setq next-error-find-buffer-function 'next-error-buffer-on-selected-frame)

;; 1 means skip anything less important then a warning,
;; 2 means skip anything less important than an error, and 0 dont' skip any.
(setq compilation-skip-threshold 2)

;; 'grep-find-ignored-directories' and others pulls default from here so
;; we add here to best share it around
(add-to-list 'vc-directory-exclusion-list "elpa")

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '("" invocation-name " " (:eval (if (buffer-file-name)
					  (abbreviate-file-name (buffer-file-name))
					"%b"))))

(require 'whitespace)
(setq whitespace-line-column 80
      whitespace-style '(face tabs empty trailing lines-tail))
(add-hook 'before-save-hook #'whitespace-cleanup)

(setq dired-recursive-deletes 'always
      dired-recursive-copies 'always)
;; if window beside is dired, it is target window for moves, etc.
(setq dired-dwim-target t)
;; some extensions that are not enable by default
(require 'dired-x)

(flyspell-mode)
(with-no-warnings
  (setq ispell-program-name "aspell"
	ispell-extra-args '("--sug-mode=ultra")))

(add-hook 'text-mode-hook #'flyspell-mode)
(add-hook 'text-mode-hook 'abbrev-mode)
(add-hook 'emacs-lisp-mode-hook 'abbrev-mode)
(add-hook 'prog-mode-hook #'flyspell-prog-mode)

;; yaml inherits from text-mode but we want whitespace on there
(add-hook 'yaml-mode-hook #'whitespace-mode)
(add-hook 'prog-mode-hook #'whitespace-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'text-mode-hook 'abbrev-mode)
;; make a shell script executable automatically on save
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)
(add-hook 'prog-mode-hook 'linum-mode)

;; macOS - for using Microsoft Ergonomic keyboard instead of Apple Magic one
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'super)     ; use left-option as <s-..> aka 'super
  ;; From Prelude: enable emoji, and stop the UI from freezing when trying to
  ;; display them.
  (when (fboundp 'set-fontset-font)
    (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))
    (with-no-warnings
    ;; ’ls’ on macOS is not GNU ls by default so we emulate standard
    ;; GNU ls using the following (from docs on ’dired-use-ls-dired’):
    (setq ls-lisp-use-insert-directory-program nil)
    (require 'ls-lisp)))

(defun cdm-swap-meta-and-super ()
  "Swap the mapping of Meta and Super.
Very useful for people using their Mac with a
Windows external keyboard from time to time."
  (interactive)
  (if (eq mac-command-modifier 'super)
      (progn
        (setq mac-command-modifier 'meta)
        (setq mac-option-modifier 'super)
        (message "Command is now bound to META and Option is bound to SUPER."))
    (setq mac-command-modifier 'super)
    (setq mac-option-modifier 'meta)
    (message "Command is now bound to SUPER and Option is bound to META.")))

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
;;(setq cmac-simple-insert-pair-alist
;;      '(("' Single quote"        . (39 39))     ; ' '
;;        ("\" Double quotes"      . (34 34))     ; " "
;;          ("` Elisp quote"         . (96 39))     ; ` '
;;          ("‘ Single apostrophe"   . (8216 8217)) ; ‘ ’
;;          ("“ Double apostrophes"  . (8220 8221)) ; “ ”
;;          ("( Parentheses"         . (40 41))     ; ( )
;;          ("{ Curly brackets"      . (123 125))   ; { }
;;          ("[ Square brackets"     . (91 93))     ; [ ]
;;          ("< Angled brackets"     . (60 62))     ; < >
;;          ("= Equals signs"        . (61 61))     ; = =
;;          ("~ Tilde"               . (126 126))   ; ~ ~
;;          ("* Asterisks"           . (42 42))     ; * *
;;          ("_ underscores"         . (95 95))))   ; _ _

;; GLOBAL KEYBINDINGS
(global-set-key (kbd "s-.") (lambda ()
                              (interactive)
                              (find-file
			       "~/.emacs.d/init.el")))
;; text-scale-increase is bound by default to s-= (Super key)
(global-set-key (kbd "s--") 'text-scale-decrease)
;; continuous -/+ after this adjusts until any other key pressed and 0 resets
(global-set-key (kbd "s-0") 'text-scale-adjust)
(global-set-key [(shift return)] 'crux-smart-open-line)
(global-set-key [(control shift return)] 'crux-smart-open-line-above)
(global-set-key [remap move-beginning-of-line] #'crux-move-beginning-of-line)
(global-set-key [remap kill-whole-line] #'crux-kill-whole-line)
(global-set-key (kbd "C-<backspace>") #'crux-kill-line-backwards)
(global-set-key (kbd "M-`") #'crux-visit-shell-buffer)
(global-set-key (kbd "s-j") #'crux-top-join-line)
(global-unset-key (kbd "<f10>")) ; usually pops up menu for key nav
(global-set-key (kbd "M-i") 'consult-imenu)
(global-set-key (kbd "M-o") 'other-window)

;;; init.el ends here
