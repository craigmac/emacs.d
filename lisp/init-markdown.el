;;; init-markdown.el --- Markdown related.
;;; Commentary:
;;; Code:

;; requirement for markdown-mode to enable edit code block with C-c '
(use-package edit-indirect)

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
  ;; gem install kramdown kramdown-parser-gfm
  :init (setq markdown-command "kramdown -x parser-gfm"))

(provide 'init-markdown)
;;; init-markdown.el ends here
