;;; init-dired.el --- Summary
;;; Commentary:
;;; Code:

(setq dired-recursive-deletes 'always
      dired-recursive-copies 'always)
;; if window beside is dired, it is target window for moves, etc.
(setq dired-dwim-target t)
;; some extensions that are not enable by default
(require 'dired-x)
;; delete them without asking
(setq dired-clean-confirm-killing-deleted-buffers nil)

(provide 'init-dired)
;;; init-dired.el ends here
