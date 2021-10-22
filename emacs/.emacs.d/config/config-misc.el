;; Always highlight parentheses
(show-paren-mode t)

;; Don't need to display anything on startup
(setq inhibit-startup-message t)

;; Replacement for other-window (C-x o) that splits the window if it doesn't exist yet
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p) (split-window-horizontally))
  (other-window 1))

(defun switch-to-eshell ()
  (interactive)
  (let ((name (eshell-curr-name)))
    (unless (get-buffer name)
      (make-shell name))
    (switch-to-buffer name)))

(global-set-key (kbd "C-x o") 'other-window-or-split)
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c g") 'rgrep)
(global-set-key (kbd "C-x m") 'switch-to-eshell)

;; Org-mode indents with headings
(add-hook 'org-mode-hook 'org-indent-mode)

;; Show line/column numbers in tool bar
(line-number-mode t)
(column-number-mode t)

;; Disable blinking cursor
(blink-cursor-mode 0)

;; Disable audible alarm
(setq ring-bell-function 'ignore)

;; text mode
(setq set-fill-column 80)

(provide 'config-misc)


