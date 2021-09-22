;;; init-typography.el --- Related to symbols you see when typing.
;;; Commentary:
;;; Code:


;; Replace ’typewriter’ style straight quotes of various kinds with the
;; appropriate and nicer “curved” variants.
;; Replace single quote with ’ and double straight quote with “curlies”,
;; and replace single backtick ` with ‘, and two `` with “.
;; Only occurs in comments, strings, and text paragraphs.
;; For Markdown trying to put in three ``` becomes “‘ so either prefix each
;; with C-q, or turn off replacing that one in Markdown documents.
;; (electric-quote-mode)
;; (add-hook 'markdown-mode-hook
;;      default is ’(8216 8217 8220 8221): meaning
;;      backtick ` becomes ‘
;;      straight single quote ' becomes ’
;;      left double " becomes “
;;           right double " becomes ”
;;           what I want is to leave ` chars alone to make code fences
;;      (setq electric-quote-char TODO))

(provide 'init-typography)
;;; init-typography.el ends here

