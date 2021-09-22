;;; init-preferences.el --- Built-in Emacs functionality tweaks
;;; Commentary:

;;; Code:

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
(setq gc-cons-threshold 20000000)   ; allow 20MB of memory before garbage collection
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

(global-visual-line-mode)
(show-paren-mode)
(transient-mark-mode)
(semantic-mode)
(save-place-mode t)
(auto-fill-mode t)
(blink-cursor-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode)
(global-eldoc-mode)
;; Load image files as actual images (e.g., when find-file foo.jpeg see image)
(auto-image-file-mode) ; open .png as image not text buffer
(size-indication-mode) ; e.g., 60:8 49% in modeline
(column-number-mode)
(global-display-line-numbers-mode) ; replaces 'nlinum-mode'
;;(set-fringe-mode 10)
(delete-selection-mode)
;; enable winner-mode to manage window configurations.
;; C-c Left/Right to undo/redo last window change
(winner-mode)


;; meaningful names for buffers with the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

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

(require 'compile)
(setq compilation-scroll-output t	; scroll compile buffer as it grows
      compilation-ask-about-save nil	; just save and compile don't ask
      compilation-always-kill t		; kill old compile proc before next one
      compilation-auto-jump-to-first-error t) ; see 'compilation-skip-threshold'

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
      '("" invocation-name " "
        (:eval
         (if (buffer-file-name)
             (abbreviate-file-name (buffer-file-name))
           "%b"))))

(require 'whitespace)
(setq whitespace-line-column 80
      whitespace-style '(face tabs empty trailing lines-tail))


(flyspell-mode)
(with-no-warnings
  (setq ispell-program-name "aspell"
  ispell-extra-args '("--sug-mode=ultra")))

;; Replace ’typewriter’ style straight quotes of various kinds with the
;; appropriate and nicer “curved” variants.
;; Replace single quote with ’ and double straight quote with “curlies”,
;; and replace single backtick ` with ‘, and two `` with “.
;; Only occurs in comments, strings, and text paragraphs.
;; For Markdown trying to put in three ``` becomes “‘ so either prefix each
;; with C-q, or turn off replacing that one in Markdown documents.
;; (electric-quote-mode)
;; (add-hook 'markdown-mode-hook
;;      default is ’(8216 8217 8220 8221): meaning
;;      backtick ` becomes ‘
;;      straight single quote ' becomes ’
;;      left double " becomes “
;;           right double " becomes ”
;;           what I want is to leave ` chars alone to make code fences
;;      (setq electric-quote-char TODO))

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




(provide 'init-preferences)
;;; init-preferences.el ends here
