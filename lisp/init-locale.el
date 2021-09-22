;;; init-locale.el --- UTF-8 please!
;;; Commentary:
;;; Code:

;; sets coding system priority, default input method, etc.
(set-language-environment "UTF-8")
;; set default value of various coding systems like new buffer,
;; subprocess I/O.
(set-default-coding-systems 'utf-8)

(provide 'init-locale)
;;; init-locale.el ends here
