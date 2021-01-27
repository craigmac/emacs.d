;;; init.el --- Emacs 27.1+ init file.
;;; Commentary:
;; Where this init.el file should lie:
;; Windows GUI => %APPDATA%/.emacs.d/init.el
;; WSL => $HOME/.emacs.d/init.el
;; macOS => $HOME/.emacs.d/init.el
;; Linux => $HOME/.emacs.d/init.el

;;; Code:
;;; PACKAGES
(require 'package)
(setq package-selected-packages
      '(ace-window
	avy
	company
	eglot
	modus-operandi-theme
	project
	which-key))
(package-install-selected-packages)

(global-set-key (kbd "M-o") 'ace-window)
(global-set-key (kbd "C-;") 'avy-goto-char)
(define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)

(load-theme 'modus-operandi t)

(global-company-mode t)
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 2)

(which-key-mode)

;; Ido
;; ==============================
;; M-n/p cycles through previous directories from history of searches
;; M-k to remove current directory from the history
;; C-f to escape anything ido and enter normal 'find-file'
;; C-t toggle regexp search within ido prompt
;; C-b|C-t to switch between find file/buffer.
;; The head of buffer or file list can be killed with C-k, C-S-b buries
;; the buffer at the end of the list.
;;
;; TODO: add more ignored dirs: elpa/ semanticdb/ eshell/ auto-save-list/ .git/
;; move all files matching "Summary" or "output\*$" to the end of the list
;; TODO: add to the list of ignores!
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)
(add-hook 'ido-make-buffer-list-hook 'ido-summary-buffers-to-end)
(setq ido-use-filename-at-point 'guess)	; best guess if file or URL
(setq ido-ignore-extensions t)
(setq ido-cannot-complete-command 'ido-prev-match) ; don't popup help buffer

;; INTERNAL VARIABLES
(defconst private-dir (expand-file-name "private" user-emacs-directory))
(setq user-full-name "C.D. MacEachern"
      user-mail-address "craigmaceachern@fastmail.com"
      confirm-kill-emacs 'y-or-n-p
      save-interprogram-paste-before-kill t
      inhibit-startup-message t
      inhibit-splash-screen t
      initial-scratch-message nil
      history-length 1000
      visible-bell nil
      create-lockfiles nil
      fill-column 80
      sentence-end-double-space nil
      confirm-kill-emacs 'y-or-n-p
      ring-bell-function 'ignore
      use-file-dialog nil
      load-prefer-newer t)
(setq-default c-default-style '((java-mode . "java")
			(awk-mode . "awk")
			(cc-mode . "stroustrup")))
(put 'narrow-to-region 'disabled nil)

;; OS-SPECIFIC
(when (equal system-type 'windows-nt)
  (setq-default w32-pipe-read-delay 0)
  (setenv "PATH"
    (concat "C:\\gnuwin32\\bin" path-separator (getenv "PATH"))))

(when (eq system-type 'darwin)
  (setenv "PATH"
	  (concat "/usr/local/bin:" (getenv "PATH")))
  ;; for GNU grep/find/locate and few others. Requires
  ;; brew install coreutils findutils grep
  (add-to-list 'exec-path "/usr/local/opt/grep/libexec/gnubin")
  (add-to-list 'exec-path "/usr/local/bin")
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'control)
  (setq mac-control-modifier 'control)
  (setq mac-right-option-modifier 'none))

;; KEYBINDINGS
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-z") 'zap-up-to-char)
(global-set-key "\C-o" 'occur)

;; COMPILATION / GREP
;; TIP: control env passed to compilation command with
;; variable 'compilation-environment'
;; TIP: usually we want 'grep-find' not just 'grep'; former reads subdirs
;; recursively, so whole tree of files from current location. 'grep' is
;; defaults to current directory.
(global-set-key (kbd "<f5>") 'compile)
(global-set-key (kbd "C-<f5>") 'kill-compilation)
(global-set-key (kbd "<f6>") 'recompile)

;; TODO: get these into an eval-after-load or something?
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
;; turn on auto display of corresponding source locations of error/matches
;; when moving through the list of them in a grep/compilation output buffer

;; TODO: add elpa/ and other directories to be ignored when doing 'grep-find'
;; or 'rgrep' (more interactive version of grep-find)

;; TODO: buffer only so I need to toggle this only in compilation/grep buffers
;;(next-error-follow-minor-mode)

;; NOTE: needs GNU grep, see how this is done in 'darwin'
;; os detection below, might have to configure 'grep-apply-setting' directly
;; instead of letting grep-compute-defaults try to figure it out, but that
;; should be last resort where emacs cannot find our GNU grep for some reason.
;; (setq grep-highlight-matches t)
(require 'grep)
(setq grep-save-buffers t)		; Don't ask: save all buffers then run

;; TODO: 'grep-find-ignored-directories' pulls its default from
;; 'vc-directory-exclusion-list' so let's add our junk to that list to benefit
;; both:
(add-to-list 'vc-directory-exclusion-list "elpa")

(global-set-key "%" 'match-paren)
(global-set-key [remap dabbrev-expand] 'hippie-expand)
(global-set-key (kbd "M-i") 'imenu)

(global-unset-key (kbd "M-SPC"))
(global-set-key (kbd "M-SPC w") 'save-buffer)
(global-set-key (kbd "M-SPC b") 'ibuffer)
(global-set-key (kbd "M-SPC r") 'grep-find)

;; TODO: bind 'find-grep-dired' as well

;; TODO: make deletion hungrier globally, I like it.

;; FUNCTIONS
(defun compile-and-run ()
  "Compile C++ buffer with C++17 standard and then run the binary."
  (interactive)
  (let* ((src (file-name-nondirectory (buffer-file-name)))
	(exe (file-name-sans-extension src)))
    (compile (concat "g++ -std=c++17 -Wall -Wextra " src " -o " exe " && ./" exe))))

(defun my-c++-mode-hook ()
  "My personal C++ mode setup."
  (interactive)
  (c-toggle-auto-hungry-state 1))

(defun my-python-mode ()
  "Additional setup for python files."
  (interactive))

(defun match-paren (arg)
  "Go to the matching paren ARG if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

;; HOOKS
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'php-mode-hook 'my-php-mode)
(add-hook 'python-mode 'my-python-mode)
(setq-default major-mode 'text-mode)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'c++-mode-hook 'my-c++-mode-hook)
;; with dired-x loaded, M-o toggles omitting each dired buffer.
(add-hook 'dired-load-hook (lambda () (require 'dired-x)))
;; make omitting the default view
(add-hook 'dired-mode-hook 'dired-omit-toggle)
(add-hook 'prog-mode 'linum-mode)

;; UI
(custom-set-faces
 '(default ((t (:family "Triplicate T4c" :foundry "outline" :slant normal :weight normal :height 180 :width normal)))))
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; BUILT-IN PACKAGES/SETTINGS
(column-number-mode t)
(show-paren-mode)
(auto-fill-mode t)
(if (fboundp 'blink-cursor-mode)
    (blink-cursor-mode -1))
(fset 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode)
(global-eldoc-mode)
(global-visual-line-mode)
(show-paren-mode)
(transient-mark-mode)
(auto-image-file-mode)
(semantic-mode)
;; in only modes stemming from prog-mode
(add-hook 'prog-mode-hook (flymake-mode))
;; flymake configuration, log-level value must be lower or equal to
;; warning-minimum-level because things not logged will not be displayed
;; in buffer either.
(setq warning-minimum-level :error)	  ; popup shows only on 'error' level
(setq warning-minimum-log-level :warning) ; log these to *Flymake Log*

;;; init.el ends here
