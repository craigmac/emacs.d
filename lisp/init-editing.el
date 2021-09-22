;;; init-editing.el --- Config related to editing, like expand-region, parens

;;; Commentary:
;; Stuff related to how you edit in a buffer, like selecting, undoing.

;;; Code:

(use-package crux)

(use-package editorconfig
  :config
  (editorconfig-mode))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package smartparens
  :config
  (require 'smartparens-config)
  (setq sp-base-key-bindings 'paredit)
  (setq sp-autoskip-closing-pair 'always)
  (setq sp-hybrid-kill-entire-symbol nil)
  (sp-use-paredit-bindings)
  (show-smartparens-global-mode +1)
  (setq blink-matching-paren nil))

;; (use-package undo-tree
;;   ;; visual undo for easy jumping back and forth between history
;;   :config
;;   ;; autosave the undo-tree history
;;   (setq undo-tree-history-directory-alist
;;   `((".*" . ,temporary-file-directory)))
;;   (setq undo-tree-auto-save-history t)
;;   (global-undo-tree-mode))

(global-visual-line-mode)
(show-paren-mode)
(transient-mark-mode)
(semantic-mode)
(save-place-mode t)
(auto-fill-mode t)
(global-auto-revert-mode)
;; remap here means whatever is mapped to call dabbrev-expand should just
;; call hippie-expand instead (it's better). Rare cases I use this.
(global-set-key [remap dabbrev-expand] 'hippie-expand)
;; Load image files as actual images (e.g., when find-file foo.jpeg see image)
(auto-image-file-mode)      ; open .png as image not text buffer
(size-indication-mode)      ; e.g., 60:8 49% in modeline
(column-number-mode)
(delete-selection-mode)

;; First try to indent the current line, and if the line was already indented,
;; then try `completion-at-point'
(setq tab-always-indent 'complete)

(provide 'init-editing)
;;; init-editing.el ends here
