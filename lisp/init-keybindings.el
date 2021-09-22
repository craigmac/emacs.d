;;; init-keybindings.el --- Configure global keybindings.
;;; Commentary:
;;  Keys for macOS runs in init-macos first if we are on macOS.
;;  This file must be evaluated AFTER loading of packages.
;;  This file is for global keybindings
;;; Code:

;; remap here means whatever is mapped to call dabbrev-expand should just
;; call hippie-expand instead (it's better). Rare cases I use this.
(global-set-key [remap dabbrev-expand] 'hippie-expand)

(global-set-key (kbd "<f5>") 'compile)
(global-set-key (kbd "C-<f5>") 'kill-compilation)
(global-set-key (kbd "<f6>") 'recompile)

(global-set-key (kbd "s-.") (lambda () (interactive)(find-file
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

;; Turn off flyspell mode map bindings completely when first loaded
(eval-after-load "flyspell"
  '(define-key flyspell-mode-map (kbd "C-.") nil))

;;(global-set-key (kbd "C-M-i") 'consult-project-imenu)

(global-set-key (kbd "<f7>") 'consult-outline)
(global-set-key [C-tab] 'consult-buffer)
(global-set-key (kbd "C-x C-r") 'consult-recent-file)

(global-set-key (kbd "<f9>") 'flycheck-mode)
(define-key flycheck-mode-map (kbd "M-n") 'flycheck-next-error)
(define-key flycheck-mode-map (kbd "M-p") 'flycheck-previous-error)

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
(global-set-key (kbd "C-h C") #'helpful-command)

(global-set-key (kbd "M-s-f") #'anzu-query-replace)
(global-set-key (kbd "M-s-F") #'anzu-query-replace-regexp)

(provide 'init-keybindings)
;;; init-keybindings.el ends here
