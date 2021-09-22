;;; init-macos.el --- when in Rome..

;;; Commentary:
;; 1. I'm using the magic keyboard on macOS:
;; C-M-Space (mark-sexp) is gobbled up by stupid Character Viewer (Emojis)
;; on macOS. Old solutions to work around this stopped working around High
;; Sierra. Now we've cannot unbind C-M-Space. Either we do instead C-M-s-2 (@)
;; or we do Esc C-Space (easier on magic keyboard to press)

;; 2. macOS GUI env inheritance is odd: exec-path-from-shell fixes env being out of sync
;; with terminal env, like not finding binaries that it should.

;;; Code:

(defun craigmac/maybe-suspend-frame ()
  "Only suspend when using terminal Emacs on macOS."
  (interactive)
  (unless (and *is-a-mac* window-system)
    (suspend-frame)))

(global-set-key (kbd "C-z") 'craigmac/maybe-suspend-frame)

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; ⌘ is Meta ("M")
(setq mac-command-modifier 'meta)
;; make ⌥ act as super key (Windows/Apple key)
(setq mac-option-modifier 'super)

(defun craigmac/swap-meta-and-super ()
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

(provide 'init-macos)
;;; init-macos.el ends here
