;;; init-global-keybindings.el --- Global keybinds
;;; Commentary:
;;; Code:

;; TODO: bind below crux functions in this use-package block
(use-package crux
  :bind
  ("C-a" . crux-move-beginning-of-line)
  ("C-k" . crux-smart-kill-line)
  ("C-c n" . crux-cleanup-buffer-or-region)
  ("C-c f" . crux-recentf-find-file)
  ("s-j" . crux-top-join-line)
  ("C-<backspace>" . crux-kill-line-backwards))

;; open init.el quick
(global-set-key (kbd "s-.") (lambda ()
                              (interactive)
                              (find-file
                               "~/.emacs.d/init.el")))

;; text-scale-increase is bound by default to s-= (Super key)
;; continuous -/+ after this adjusts until any other key pressed and 0 resets
(global-set-key (kbd "s--") 'text-scale-decrease)
(global-set-key (kbd "s-0") 'text-scale-adjust)

;; use crux versions
(global-set-key [(shift return)] 'crux-smart-open-line)
(global-set-key [(control shift return)] 'crux-smart-open-line-above)
(global-set-key [remap move-beginning-of-line] #'crux-move-beginning-of-line)
(global-set-key [remap kill-whole-line] #'crux-kill-whole-line)

;; usually pops up menu for key nav
(global-unset-key (kbd "<f10>"))

(global-set-key (kbd "M-i") 'consult-imenu)
(global-set-key (kbd "M-o") 'other-window)

;; use `ibuffer' instead of default `list-buffers'
;; projectile is setup with this too to group by project buffers
(global-set-key [remap list-buffers] 'ibuffer)

(provide 'init-global-keybindings)
;;; init-global-keybindings.el ends here
