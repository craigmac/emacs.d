;;; init-git.el --- Git related things.
;;; Commentary:
;;; Code:

(use-package diff-hl
  :after (magit)
  :config
  (global-diff-hl-mode)
  ;; mouse click in fringe over git marker shows the diff
  (global-diff-hl-show-hunk-mouse-mode)
  ;; highlight changes on the fly
  (diff-hl-flydiff-mode)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package forge
  ;; magit companion for working with Git forges (GitLab/Hub)
  :after magit
  :init
  ;; github API key stored using this one specifically, instead
  ;; of looking at all three possible files
  (setq auth-sources '("~/.authinfo")))

(use-package git-timemachine)

(use-package magit
  :bind
  ("C-c m" . 'magit-status))

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(add-hook 'git-commit-mode-hook 'flyspell-mode)

;; macos Catalina can take 20x longer than Linux unless we specify git
;; absolute path, according to magit manual under "MacOS Performance"
;; section. Turn this off though if we want to use Magit on remote
;; machines using Tramp.

(when *is-a-mac*
  (setq magit-git-executable "/usr/local/bin/git"))

;; Magit manual recommends you bind ‘C-c g’ instead of ‘C-c M-g’ to
;; ‘magit-file-dispatch’. The former is a much better binding but the
;; ‘C-c <letter>’ namespace is strictly reserved for users; preventing
;; Magit from using it by default.
(global-set-key (kbd "C-c g") 'magit-file-dispatch)

;; 'grep-find-ignored-directories' and others pulls default from here so
;; we add here to best share it around
(add-to-list 'vc-directory-exclusion-list "elpa")

;; Allow access from emacsclient
(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))

(provide 'init-git)
;;; init-git.el ends here
