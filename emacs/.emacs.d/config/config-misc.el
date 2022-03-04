;; Always highlight parentheses
(show-paren-mode t)

(display-time)

(winner-mode 1)

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
(setq org-directory "~/org")

(defun org-file-path (filename)
  "Return the absolute address of an org file, given its relative name."
  (concat (file-name-as-directory org-directory) filename))

(setq org-default-notes-file "~/org/tasks.org")

(setq org-index-file (org-file-path "tasks.org"))
(setq org-agenda-files (list org-index-file
			     (org-file-path "work.org")
			     (org-file-path "research.org")
			     (org-file-path "home.org")
			     (org-file-path "events.org")))

(global-set-key (kbd "C-c a") 'org-agenda)
;;  C-c C-x C-s will mark a task as done, move it to an appropriate
;;  place in the archive, and save all the Org buffers.
(defun hrs/mark-done-and-archive ()
  "Mark the state of an org-mode item as DONE, archive it, and
save the Org buffers."
  (interactive)
  (org-todo 'done)
  (org-archive-subtree)
  (org-save-all-org-buffers))

(define-key org-mode-map (kbd "C-c C-x C-s") 'hrs/mark-done-and-archive)

(setq org-enforce-todo-dependencies t)
(setq org-enforce-todo-checkbox-dependencies t)

(defun hrs/dashboard ()
  (interactive)
  (call-process-shell-command "daily-checklist")
  (delete-other-windows)
  (find-file org-index-file)
  (org-agenda nil "p"))

(global-set-key (kbd "C-c d") 'hrs/dashboard)

(defun hrs/open-work-file ()
  "Open the work TODO list."
  (interactive)
  (find-file (org-file-path "work.org"))
  (flycheck-mode -1)
  (end-of-buffer))

(global-set-key (kbd "C-c w") 'hrs/open-work-file)

(defun hrs/open-research-file ()
  "Open the work TODO list."
  (interactive)
  (find-file (org-file-path "research.org"))
  (flycheck-mode -1)
  (end-of-buffer))

(global-set-key (kbd "C-c r") 'hrs/open-research-file)

(defun hrs/open-index-file ()
  "Open the master org TODO list."
  (interactive)
  (find-file org-index-file)
  (flycheck-mode -1)
  (end-of-buffer))

(global-set-key (kbd "C-c i") 'hrs/open-index-file)

(defun org-capture-todo ()
  (interactive)
  (org-capture :keys "t"))

(global-set-key (kbd "C-c t") 'org-capture-todo)

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

(defun win-resize-top-or-bot ()
  "Figure out if the current window is on top, bottom or in the
middle"
  (let* ((win-edges (window-edges))
	 (this-window-y-min (nth 1 win-edges))
	 (this-window-y-max (nth 3 win-edges))
	 (fr-height (frame-height)))
    (cond
     ((eq 0 this-window-y-min) "top")
     ((eq (- fr-height 1) this-window-y-max) "bot")
     (t "mid"))))

(defun win-resize-left-or-right ()
  "Figure out if the current window is to the left, right or in the
middle"
  (let* ((win-edges (window-edges))
	 (this-window-x-min (nth 0 win-edges))
	 (this-window-x-max (nth 2 win-edges))
	 (fr-width (frame-width)))
    (cond
     ((eq 0 this-window-x-min) "left")
     ((eq (+ fr-width 4) this-window-x-max) "right")
     (t "mid"))))

(defun win-resize-enlarge-horiz ()
  (interactive)
  (cond
   ((equal "top" (win-resize-top-or-bot)) (enlarge-window -5))
   ((equal "bot" (win-resize-top-or-bot)) (enlarge-window 5))
   ((equal "mid" (win-resize-top-or-bot)) (enlarge-window -5))
   (t (message "nil"))))

(defun win-resize-minimize-horiz ()
  (interactive)
  (cond
   ((equal "top" (win-resize-top-or-bot)) (enlarge-window 5))
   ((equal "bot" (win-resize-top-or-bot)) (enlarge-window -5))
   ((equal "mid" (win-resize-top-or-bot)) (enlarge-window 5))
   (t (message "nil"))))

(defun win-resize-enlarge-vert ()
  (interactive)
  (cond
   ((equal "left" (win-resize-left-or-right)) (enlarge-window-horizontally -5))
   ((equal "right" (win-resize-left-or-right)) (enlarge-window-horizontally 5))
   ((equal "mid" (win-resize-left-or-right)) (enlarge-window-horizontally -5))))

(defun win-resize-minimize-vert ()
  (interactive)
  (cond
   ((equal "left" (win-resize-left-or-right)) (enlarge-window-horizontally 5))
   ((equal "right" (win-resize-left-or-right)) (enlarge-window-horizontally -5))
   ((equal "mid" (win-resize-left-or-right)) (enlarge-window-horizontally 5))))

(global-set-key [C-M-down] 'win-resize-minimize-vert)
(global-set-key [C-M-up] 'win-resize-enlarge-vert)
(global-set-key [C-M-left] 'win-resize-minimize-horiz)
(global-set-key [C-M-right] 'win-resize-enlarge-horiz)
(global-set-key [C-M-up] 'win-resize-enlarge-horiz)
(global-set-key [C-M-down] 'win-resize-minimize-horiz)
(global-set-key [C-M-left] 'win-resize-enlarge-vert)
(global-set-key [C-M-right] 'win-resize-minimize-vert)

;; (global-hl-line-mode)

;; (set-face-background 'hl-line "#1f7fbe")
;; (set-face-foreground 'hl-line "#f3ee2a")

(provide 'config-misc)


