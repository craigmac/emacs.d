;;; init-spelling.el --- flyspell, dictionaries, abbrevs, etc.
;;; Commentary:
;;; Code:

(use-package flycheck
  ;; I use the same CLI tool that VS Code's Markdownlint extension uses,
  ;; it works differently that original CLI markdown tools, and supports
  ;; a config inheritance like, for example, Git's.
  ;; https://github.com/DavidAnson/markdownlint-cli2
  :config
  ;; TODO: currently doesn't work, cli2 too different than original mdl tool
  ;; will need to either try flycheck-mmark package or make my own
  ;; (setq flycheck-markdown-mdl-executable "markdownlint-cli2"))
  ;; (setq-default flycheck-indication-mode 'right-fringe)
  (global-set-key (kbd "<f9>") 'flycheck-mode)
  (define-key flycheck-mode-map (kbd "M-n") 'flycheck-next-error)
  (define-key flycheck-mode-map (kbd "M-p") 'flycheck-previous-error))

;; TODO: which one these am I keeping?
(flycheck-define-checker markdown-vs-code-markdownlint
  "A Markdown linter using the same linter the VS Code
'Markdownlint' extension does."
  :command ("markdownlint-cli2" source)
  :error-patterns
  ;; example:
  ;; youi-tv-run.md:102:302 MD033/no-inline-html Inline HTML [Element: li]
  ((error line-start (file-name) ":" line ":" column " " (message) line-end))
  :modes (gfm-mode markdown-mode))

(flycheck-define-checker vale-craigmac
  "Run vale."
  :command ("vale"
            "--output"
            "line"
            source)
  :error-patterns
  ;; example:
  ;; 6.12-migration.md:81:107:write-good.Passive:'is described' may be passive
  ((error line-start (file-name) ":" line ":" column ":" (message) line-end))
  :modes (gfm-mode markdown-mode org-mode text-mode rst-mode))

(use-package flycheck-vale
  :after flycheck)

(use-package flycheck-posframe
  ;; use popup mini frames to display error inline when hovered over
  :after flycheck)

(flyspell-mode)
(with-no-warnings
  (setq ispell-program-name "aspell"
  ispell-extra-args '("--sug-mode=ultra")))

(add-hook 'text-mode-hook 'abbrev-mode)
(add-hook 'emacs-lisp-mode-hook 'abbrev-mode)
(add-hook 'prog-mode-hook #'flyspell-prog-mode)

(provide 'init-spelling)
;;; init-spelling.el ends here
