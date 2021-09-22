;;; init-jekyll.el --- Config for working with Jekyll specifically
;;; Commentary:
;;; Code:

(defun craigmac-jekyll-markdown-mode ()
    "Minor mode setup for Jekyll Liquid Markdown buffers."
    (flyspell-mode)
    ;; 'face is required. 'tabs and 'spaces are visualized via faces.
    (setq whitespace-style '(face tabs spaces space-mark))
    (whitespace-mode)
    (abbrev-mode)
    (setq-local tab-width 3)
    (display-line-numbers-mode)
    ;; gem install kramdown kramdown-parser-gfm
    (setq-local markdown-command "kramdown -x parser-gfm"
                markdown-asymmetric-header t
                markdown-list-indent-width 2
                markdown-hide-markup nil
                ;; three backticks do no trigger auto insert menu of languages
                ;; because our parser supports more than listed here so I don't
                ;; use it.
                markdown-gfm-use-electric-backquote nil
                ;; quicker for tab completion with keyboard, instead of menus
                markdown-nested-imenu-heading-index nil)
    ;; setup flycheck to run markdownlint-cli2
    (setq flycheck-checker 'markdown-vs-code-markdownlint))
(add-hook 'gfm-mode-hook 'craigmac-jekyll-markdown-mode)
;; I just want these two disabled everywhere by default
(setq-default flycheck-disabled-checkers '(markdown-mdl proselint vale))
;; Run my vale syntax checker after markdownlint-cli2
(flycheck-add-next-checker 'markdown-vs-code-markdownlint 'vale-craigmac)
;; M-x sudo to invoke TRAMP to edit current file as root hack
(defun sudo()
  "Use TRAMP to `sudo' the current buffer."
  (interactive)
  (when buffer-file-name
    (find-alternate-file
     (concat "/sudo:root@localhost:"
       buffer-file-name))))
;; .zsh file is shell script too
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))

(provide 'init-jekyll)
;;; init-jekyll.el ends here
