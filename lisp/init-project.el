;;; init-project.el --- Project-centric stuff
;;; Commentary:
;;; Code:

(use-package projectile
  :config
  (setq projectile-project-search-path '("~/git" "~"))
  (when (executable-find "rg")
    (setq-default projectile-generic-command "rg --files --hidden"))
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-find-file)
              ("C-c p" . projectile-command-map)))

;; group ibuffer list by projectile root, so we can see which
;; buffers belong to which project
(use-package ibuffer-projectile
  :after (projectile)
  :config
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-projectile-set-filter-groups)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))

(provide 'init-project)
;;; init-project.el ends here
