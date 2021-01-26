;;; init.el --- Emacs 27.1+ init file.
;;; Commentary:
;; Where this init.el file should lie:
;; Windows GUI => %APPDATA%/.emacs.d/init.el
;; WSL => $HOME/.emacs.d/init.el
;; macOS => $HOME/.emacs.d/init.el
;; Linux => $HOME/.emacs.d/init.el
					;
;;; Code:
(require 'package)
(package-initialize)

;;; ELPA
(setq package-selected-packages
      '(modus-themes))
(package-install-selected-packages)

;; INTERNAL VARIABLES
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))
(defconst private-dir (expand-file-name "private" user-emacs-directory))
(setq user-full-name "C.D. MacEachern"
      user-mail-address "craigmaceachern@fastmail.com"
      confirm-kill-emacs 'y-or-n-p
      save-interprogram-paste-before-kill t
      inhibit-startup-message t
      inhibit-splash-screen t
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
    (concat "C:\\gnuwin32\\bin" path-separator (getenv "PATH")))
  ;; github.com/magit/with-editor/issues/41 solution for Windows
  (defadvice server-ensure-safe-dir
      (around my-around-server-ensure-safe-dir activate)
    "Ignores any errors raised from server-ensure-safe-dir"
    (ignore-errors ad-do-it)))
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'control)
  (setq mac-control-modifier 'control)
  (setq mac-right-option-modifier 'none))
(when (equal system-type 'gnu/linux))

;; KEYBINDINGS
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-z") 'zap-up-to-char)
(global-set-key "\C-o" 'occur)
(global-set-key (kbd "<f6>") 'compile)
(global-set-key "%" 'match-paren)
(global-set-key [remap dabbrev-expand] 'hippie-expand)
(global-set-key (kbd "M-i") 'imenu)

(global-unset-key (kbd "M-SPC"))
(global-set-key (kbd "M-SPC w") 'save-buffer)
(global-set-key (kbd "M-SPC b") 'ibuffer)

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
 '(default ((t (:family "Triplicate T4c"
			:foundry "outline"
			:slant normal
			:weight normal
			:height 180
			:width normal)))))
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(require 'modus-themes)
(load-theme 'modus-operandi t)

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
(windmove-default-keybindings)

(require 'ido)
(ido-mode 1)
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)
;;; init.el ends here
