;;; init-exec-path.el --- Set up exec-path properly -*- lexical-binding: t -*-
;;; Commentary: Thanks to github.com/purcell/emacs.d for this one.
;;; Code:

(require-package 'exec-path-from-shell)

;; Put any variables you want Emacs to pick up that weren't in here
;; (with-eval-after-load 'exec-path-from-shell
;;   (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "NIX_SSL_CERT_FILE" "NIX_PATH"))
;;     (add-to-list 'exec-path-from-shell-variables var)))


;; Call exec-path-from-shell-initialize only when we are running GUI,
;; or we are not a daemon process
(when (or (memq window-system '(mac ns x))
          (unless (memq system-type '(ms-dos windows-nt))
            (daemonp)))
  (exec-path-from-shell-initialize))

(provide 'init-exec-path)
;;; init-exec-path.el ends here
