;; Always highlight parentheses
(show-paren-mode t)

;; Don't need to display anything on startup
(setq inhibit-startup-message t)

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                   (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c g") 'rgrep)

;; Org-mode indents with headings
(add-hook 'org-mode-hook 'org-indent-mode)

;; Show line/column numbers in tool bar
(line-number-mode t)
(column-number-mode t)

;; Disable blinking cursor
(blink-cursor-mode 0)

;; Disable audible alarm
(setq ring-bell-function 'ignore)

;; Fill column
(add-hook 'c-mode-hook (lambda () (set-fill-column 80)))
(add-hook 'c-mode-hook 'auto-fill-mode)
(add-hook 'c++-mode-hook (lambda () (set-fill-column 80)))
(add-hook 'c++-mode-hook 'auto-fill-mode)

;; About having backups file all over the places
(setq make-backup-files nil)

(provide 'config-misc)


