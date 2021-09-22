;;; init.el --- craigmac Emacs configuration
;;
;;; Commentary:
;;
;; This is my personal Emacs configuration.  Nothing more, nothing less.
;; Inspired in part by other great configurations like Steve Purcell's,
;; Prelude, and many more.
;;
;;; Code:
(defconst *is-a-mac* (eq system-type 'darwin))
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(setq gc-cons-threshold 50000000)

(require 'init-locale)
(require 'init-use-package)
(require 'init-settings)
(require 'init-ui)
(require 'init-minibuffer)
(require 'init-searching)
(require 'init-help)
(require 'init-project)
(require 'init-markdown)
(require 'init-spelling)
(require 'init-editing)
(when *is-a-mac*
  (require 'init-macos))
(require 'init-yaml)
(require 'init-whitespace)
(require 'init-jekyll)
(require 'init-dired)
(require 'init-typography)
(require 'init-flycheck)
(require 'init-global-keybindings)
(require 'init-git)
(require 'init-shell)
(require 'init-compile)

;; Variables configured via `customize' interface
(when (file-exists-p custom-file)
  (load custom-file))

;; TODO:
;; * figure out spelling settings
;; * how to add words to ok words list for work words
;; * company settings:
;;   * stupid autofilling out!
;;   * enter NOT filling candidate in.
;;   * not filling in automatically if it's the only candidate.
;;   * something weird about suggestions with numbers or numbers plus decimal
;;     like it completes any other number found in the buffer which is annoying
;; * auto-revert buffers when magit switches branches? how do i?
;; * M-$ blocked on macOS, can't use for flyspell. Bind something else.


;; TODO: consider these
(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

(defun rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))

(defun browse-current-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p)
             (tramp-tramp-file-p file-name))
        (error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))

;;; init.el ends here
