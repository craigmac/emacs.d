;;; init-compile.el --- Compilation buffers/modes
;;; Commentary:
;;; Code:

(require 'compile)
(setq compilation-scroll-output t   ; scroll compile buffer as it grows
      compilation-ask-about-save nil  ; just save and compile don't ask
      compilation-always-kill t     ; kill old compile proc before next one
      compilation-auto-jump-to-first-error t) ; see 'compilation-skip-threshold'

(global-set-key (kbd "<f5>") 'compile)
(global-set-key (kbd "C-<f5>") 'kill-compilation)
(global-set-key (kbd "<f6>") 'recompile)

;; set next/previous result/error keys (M-g M-n/p) to look for buffers with
;; error/matches in windows of current frame only, and then for a buffer
;; used previously by 'next-error' or 'previous-error'
(setq next-error-find-buffer-function 'next-error-buffer-on-selected-frame)

;; 1 means skip anything less important then a warning,
;; 2 means skip anything less important than an error, and 0 dont' skip any.
(setq compilation-skip-threshold 2)

(provide 'init-compile)
;;; init-compile.el ends here
