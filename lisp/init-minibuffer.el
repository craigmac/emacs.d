;;; init-minibuffer.el --- Minibuffer stuff: completion system, etc.
;;; Commentary:
;;; Code:

(use-package company
  :init
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-limit 10)
  ;;  (setq company-tooltip-align-annotations t)
  ;; when near bottom of window, flip to fix it
  ;;  (setq company-tooltip-flip-when-above t)
  :config
  (global-company-mode))

;; TODO: tweak colours on this to not be so jarring with modus theme
(use-package company-box
  ;; https://github.com/sebastiencs/company-box
  ;; better UI for company with help strings, icons, support for whitespace-mode
  :after (company)
  :hook (company-mode . company-box-mode))

(use-package consult
  ;; Enhances several minibuffer-centric commands and provides new
  ;; ones, replacing Emacs' default (completing-read) function.
  :config
  (global-set-key (kbd "<f7>") 'consult-outline)
  (global-set-key [C-tab] 'consult-buffer)
  (global-set-key (kbd "C-x C-r") 'consult-recent-file))

(use-package embark
  ;; Visualizes the list of completion candidates, as well as provides actions
  ;; we can perform on them, on a per-item or per-set basis. Like
  ;; a right-click functionality to work with candidates.
  :bind
  (("M-RET" . embark-act))
  :init
  ;; optionally replace the key help with a completion-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; hide mode line of embark live/completion buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  ;; used with consult
  :after (embark consult)
  :demand t ;; necessary with hook below
  ;; gives consult previews as you move around auto-updating collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package marginalia
  ;; provides meta-information to various completion lists, like doc strings
  ;; to the mini-buffer list when using M-x
  :config
  (marginalia-mode))

(use-package orderless
  ;; A completion style, which extends or replaces the built-in list
  ;; of pattern matching ’completion-styles’ native to Emacs. Used in
  ;; conjunction with Selectrum (which chooses a filtering algorithm from
  ;; Emacs' 'completion-styles).
  :config
  ;; Add to Emacs’ built-in 'completion-styles
  (setq completion-styles '(orderless)))

(use-package selectrum
  ;; https://github.com/raxod502/selectrum
  ;;
  ;; A better completion UI using standard Emacs APIs - an interface
  ;; for selecting items from a list for all Emacs completion
  ;; commands. For filtering by default is defers to Emacs'
  ;; completion-styles, but to use other more powerful ones you need
  ;; to install other packages that provide these. Two of such are:
  ;; Prescient and Orderless.
  :config
  (selectrum-mode +1)
  ;; to work with flyspell interface
  (setq flyspell-correct-interface #'flyspell-correct-dummy))

(use-package selectrum-prescient
  ;; better filtering and sorting according to selectrum maintainer.
  ;; filters MRU completions, etc. to top of selection list
  :config
  (selectrum-prescient-mode +1)
  ;; save cmd history to disk, better sorting over time
  (prescient-persist-mode +1))

(provide 'init-minibuffer)
;;; init-minibuffer.el ends here
