;;; init.el
;
;;; Commentary:
; Where this init.el file should lie:
; Windows GUI => %APPDATA%/.emacs.d/init.el
; WSL => $HOME/.emacs.d/init.el
; macOS => $HOME/.emacs.d/init.el
; Linux => $HOME/.emacs.d/init.el
;
;;; Code:
(setq debug-on-error nil)

(require 'package)
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

; TODO: void-variable on first try, fix this
(when (not package-archive-contents)
  package-refresh-contents)

; Built-in variable settings
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

; Built-in Modes
(column-number-mode t)
(show-paren-mode)
(auto-fill-mode t)
(blink-cursor-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode)
(global-eldoc-mode)
(global-visual-line-mode)
(show-paren-mode)
(transient-mark-mode)

(semantic-mode)

; Keybindings
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-z") 'zap-up-to-char)

; Hooks
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'php-mode-hook 'my-php-mode)
(add-hook 'python-mode 'my-python-mode)

(defun my-php-mode ()
  "Additional PHP programming setup."
  (interactive)
  (local-set-key (kbd "<f1>") 'my-php-function-lookup))

(defun my-php-function-lookup ()
  "Lookup symbol under point using web manual."
  (interactive)
  (let ((symbol (symbol-at-point)))
    (if (not symbol)
        (message "No symbol at point to lookup.")
      (browse-url (concat "http://php.net/manual-lookup.php?pattern="
                          (symbol-name symbol))))))

(defun my-python-mode ()
  "Additional setup for python files."
  (interactive))

; UI
(custom-set-faces
 '(default ((t (:family "Triplicate T4c"
			:foundry "outline"
			:slant normal
			:weight normal
			:height 180
			:width normal)))))
; Disable GUI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)



(add-hook 'prog-mode-hook 'linum-mode)

; OS-Specific
(when (equal system-type 'windows-nt)
  (setq w32-pipe-read-delay 0)

  ;; github.com/magit/with-editor/issues/41 solution for Windows
  (defadvice server-ensure-safe-dir
      (around my-around-server-ensure-safe-dir activate)
    "Ignores any errors raised from server-ensure-safe-dir"
    (ignore-errors ad-do-it)))

(when (equal system-type 'darwin))
(when (equal system-type 'gnu/linux))

;; Working area / Playground

;; TODO: Alt-` bound to show terminal along bottom, switching
;; to it if buffer exists otherwise creating it.
;; hints: 'get-buffer-create'

(put 'narrow-to-region 'disabled nil)

(setq package-selected-packages
      '(modus-themes))
(package-install-selected-packages)
(require 'modus-themes)
(load-theme 'modus-operandi t)

;;; init.el ends here
