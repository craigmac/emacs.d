(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

(setq use-package-always-ensure t) ; useful on new computer, to auto install

(scroll-bar-mode -1)        ; disable scrollbars
(tool-bar-mode -1)          ; disable visual toolbar and icons
(set-fringe-mode 10)        ; Give some breathing room

(setq visible-bell nil)

(set-face-attribute 'default nil :font "Hack" :height 160)


;; easier than the C-g business.
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; on new machine run M-x all-the-icons-install-fonts after install
(use-package all-the-icons)

;; turns off listing in modeline on demand
(use-package diminish)

;; ivy for completing-read-function (default emacs function)
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-l" . ivy-alt-done)
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line)
	 :map ivy-switch-buffer-map
	 ("C-k" . ivy-previous-line)
	 ("C-l" . ivy-done)
	 ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
	 ("C-k" . ivy-previous-line)
	 ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode t))

;; details of commands, e.g., when doing M-x
(use-package ivy-rich
  :config
  (ivy-rich-mode))

;; ivy installs this but we can use use-package to config
;; any package that is on the system
(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history)))

;; remap keeps the keymap already set up but changes what fn it calls.
;; helpful provides a much better help-type buffer over basic help view.
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; nicer looking modeline down below, and clean
(use-package doom-modeline
  :init (doom-modeline-mode)
  :custom ((doom-modeline-height 18)))

;; for some options, load-them doom-...
(use-package doom-themes)

;; Built-in package settings
(column-number-mode)
(global-display-line-numbers-mode t)
(show-paren-mode)
(auto-fill-mode t)
(if (fboundp 'blink-cursor-mode)
    (blink-cursor-mode -1))
(fset 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode)
(global-eldoc-mode)
(global-visual-line-mode)
(transient-mark-mode)
(auto-image-file-mode)
(semantic-mode)
(add-hook 'prog-mode-hook (flymake-mode))
;; flymake configuration, log-level value must be lower or equal to
;; warning-minimum-level because things not logged will not be displayed
;; in buffer either.
(setq warning-minimum-level :error)	  ; popup shows only on 'error' level
(setq warning-minimum-log-level :warning) ; log these to *Flymake Log*
(define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)
(setq-default major-mode 'text-mode)

;; disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package modus-themes
  :config
  (load-theme 'modus-operandi t))

(use-package which-key
  :diminish
  :init (which-key-mode))

;; TODO: read about how to use use-package properly
;; TODO: read about macOS keybindings and how to change them, and
;; anything set by default on macOS detection

;; jump to init.el with Command-.
(global-set-key (kbd "s-.") (lambda () (interactive)(find-file "~/.emacs.d/init.el")))

(use-package magit)

(use-package company
    :config
    (global-company-mode t)
    :custom
    (company-idle-delay 0)
    (company-minimum-prefix-length 2))

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

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Part 3: https://www.youtube.com/watch?v=xaZMwNELaJY

