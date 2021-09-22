(flycheck-define-checker markdown-vs-code-markdownlint
  "A Markdown linter using the same linter the VS Code
'Markdownlint' extension does."
  :command ("markdownlint-cli2" source)
  :error-patterns
  ;; example:
  ;; youi-tv-run.md:102:302 MD033/no-inline-html Inline HTML [Element: li]
  ((error line-start (file-name) ":" line ":" column " " (message) line-end))
  :modes (gfm-mode markdown-mode))


;; TODO: I've registered this using custom to the list of flycheck-checkers,
;; but it should be done programmatically for distribution and/or the custom.el
;; file was altered and/or removed somehow.

;; TODO: turn off the other default enabled syntax checkers proselint, markdown-mdl
;; for Markdown/GFM modes.

;; TODO: turn on automatically this checker and then vale for Markdown and GFM modes.
