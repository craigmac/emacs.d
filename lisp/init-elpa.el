;;; init-elpa.el --- Setting and helpers for package.el
;;; Commentary: Many thanks to github.com/purcell for the inspiration.
;;; Code:

;; package.el setup
(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa-stable" . "http://stable.melpa.org/packages/")))
(package-initialize)

(defvar cdm-package-list
  '(all-the-icons
    anzu
    avy
    company
    consult
    crux
    doom-modeline
    editorconfig
    eldoc
    embark
    embark-consult
    exec-path-from-shell
    expand-region
    diff-hl
    flycheck
    git-timemachine
    helpful
    hl-todo
    magit
    marginalia
    markdown-mode
    modus-themes
    orderless
    projectile
    selectrum
    selectrum-prescient
    smartparens
    rg
    undo-tree
    which-key
    yaml-mode))


(provide 'init-elpa)

;;; init-elpa.el ends here
