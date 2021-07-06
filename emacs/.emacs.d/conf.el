(require 'package)

(setq package-list '(evil lsp-mode rust-mode magit evil xclip linum-relative lsp-ui
                          ace-window use-package org helm evil-leader elpy fzf
                          projectile rust-mode racer company ccls ivy swiper
                          yasnippet yasnippet-snippets projectile
                          helm-projectile helm-ag helm-rg smart-mode-line rg projectile-ripgrep
                          evil-nerd-commenter afternoon-theme buffer-move))

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
	(package-install package)))


(use-package lsp-mode
  :commands lsp
  :custom
  ((lsp-enable-indentation nil)
   (lsp-enable-on-type-formatting nil)
))

(setq lsp-file-watch-threshold nil)
(add-hook 'c++-mode-hook 'lsp)
(add-hook 'c-mode-hook 'lsp)

;; lsp-ui
(require 'lsp-ui)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)
(add-hook 'after-init-hook 'global-company-mode)
(setq lsp-diagnostic-package :none)


;; ccls
(use-package ccls
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda () (require 'ccls) (lsp)))
  :config
  (dolist (dir '(".ccls-cache" "build"))
    (add-to-list 'lsp-file-watch-ignored dir))
  (setq ccls-executable "~/.local/bin/ccls")
  ;;(setq ccls-args '("--init={\"cache\": {\"directory\": \"/home/alvaro/.cache/ccls-cache\"}}"))
)


(defface company-box-candidate
  '((((background light)) :foreground "black")
    (t :foreground "white"))
  "Face used to color candidates."
  :group 'company-box)

(package-initialize)

;; buffers
(setq indo-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

(defalias 'list-buffers 'ibuffer)

;; windows
(use-package ace-window
  :ensure t
  :init
  (progn
    (global-set-key [remap other-window] 'ace-window)
    ))

;; magit config
(global-set-key (kbd "C-x g") 'magit-status)

;; evil config
(setq evil-toggle-key "C-q")
(require 'evil)
(evil-mode 1)

(eval-after-load 'evil-maps
  '(define-key evil-normal-state-map (kbd "M-.") nil))

(global-evil-leader-mode)
(setq evil-leader/in-all-states 1)
(evil-leader/set-leader ",")
(evil-leader/set-key
  "." 'switch-to-previous-buffer
  "b" 'helm-buffers-list
  "f" 'helm-find-files
  "g" 'magit-status
  "x" 'helm-M-x)

(define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
(define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)

(define-key evil-normal-state-map (kbd "C-]") 'xref-find-definitions)
(define-key evil-normal-state-map (kbd "C-r") 'xref-find-references)
(define-key evil-normal-state-map (kbd "C-\\") 'find-tag)
(define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop)

;; fuzzy file find

(global-set-key (kbd "C-c p p") 'fzf-find-file)
(global-set-key (kbd "C-c p f") 'helm-rg)

; Get rid of the startup message
(setq inhibit-startup-message t)
; Show file full path in title bar
(setq-default frame-title-format
	      (list '((buffer-file-name " %f"
					(dired-directory
					 dired-directory
					 (revert-buffer-function " %b"
								 ("%b - Dir:  " default-directory)))))))
; Shows parenthesis
(show-paren-mode 1)
; Shows column number
(column-number-mode 1)

(add-hook 'c-mode-common-hook #'hs-minor-mode)

;; hide menu bar
(menu-bar-mode -1)

;; mode-line bar
(set-face-attribute 'mode-line nil
		    :foreground "White"
		    :background "Black"
		    :box nil)

(add-hook 'dired-mode-hook (lambda () (evil-emacs-state)))

;; clipboard
(require 'xclip)
(xclip-mode 1)

;; backup files
(setq backup-directory-alist `(("." . "~/.saves")))

(setq-default indent-tabs-mode nil)

;; text mode
(setq set-fill-column 80)

;; c-h with backspace issues

(global-set-key [?\C-h] 'delete-backward-char)
(global-set-key [?\C-x ?h] 'help-command)

;; whitespace 80 column
(require 'whitespace)
(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face lines-tail))

(add-hook 'prog-mode-hook 'whitespace-mode)

;; elpy
(elpy-enable)

(add-hook 'elpy-mode-hook (lambda () (highlight-indentation-mode -1)))
(setq elpy-rpc-python-command "python3")

;; rust
(use-package rust-mode
  :ensure t)

(use-package lsp-mode
  :ensure t
  :hook ((rust-mode . lsp)
         (python-mode . lsp)))

(setq lsp-prefer-capf t)
(setq lsp-completion-provider :capf)
(setq lsp-completion-enable t)

(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)

(global-set-key (kbd "C-c d") "M-.")

(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)

(when (fboundp 'electric-indent-mode) (electric-indent-mode -1))

(defun indent-or-complete ()
        (interactive)
        (if (looking-at "\\_>")
                (company-complete-common)
                (indent-according-to-mode)))

;; linum-relative
(require 'linum-relative)

;; enable mouse
(require 'mouse)
(xterm-mouse-mode t)
(mouse-wheel-mode t)

(global-set-key [(C-f5)] 'compile)

(add-hook 'c-mode-common-hook (lambda ()
      (local-set-key (kbd "RET") 'newline-and-indent)))

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

(icomplete-mode 1)

(require 'yasnippet)
(yas-global-mode 1)

(require 'projectile)
(setq projectile-use-native-indexing t)
(setq projectile-globally-ignored-directories
      '(".git"
        ".ccls-cache"
        ".svn"
        ".stack-work"
        ".cquery_cached_index"
        "out"
        "third_party"))

(setq projectile-globally-ignored-files
      '("tags"
        "cscope.files"
        "cscope.out"
        "cscope.in.out"))

(projectile-mode +1)

(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c f") 'projectile-command-map)

(defun my-project-try-cargo-toml (_)
  (let (dir)
    (ignore-errors
      (let* ((output (shell-command-to-string "cargo metadata --no-deps --format-version 1"))
             (js (json-read-from-string output)))
        (setq dir (cdr (assq 'workspace_root js)))))
    (and dir (cons 'transient  dir))))

;; Try rust projects before version-control (vc) projects
(add-hook 'project-find-functions 'my-project-try-cargo-toml nil nil)

(custom-set-variables
 '(helm-ag-base-command "rg --no-heading")
 `(helm-ag-success-exit-status '(0 2)))

;;(set-face-attribute 'mode-line-buffer-id nil :foreground "white")

;;(set-face-foreground 'mode-line "#2424F1")
;;(set-face-background 'mode-line "#2424F1")
;;(set-face-foreground 'mode-line-inactive "#2424F1")
;;(set-face-background 'mode-line-inactive "#2424F1")

(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

(setq org-agenda-files (list "~/org/work.org"
                             "~/org/home.org"))
(require 'org-mouse)

(add-hook 'python-mode-hook
      (lambda ()
        (setq indent-tabs-mode nil)
        (setq python-indent 4)
        (setq tab-width 4))
      (untabify (point-min) (point-max)))

(require 'rg)
(rg-enable-default-bindings)

;; Vim key bindings
(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-key
  "ci" 'evilnc-comment-or-uncomment-lines
  "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
  "ll" 'evilnc-quick-comment-or-uncomment-to-the-line
  "cc" 'evilnc-copy-and-comment-lines
  "cp" 'evilnc-comment-or-uncomment-paragraphs
  "cr" 'comment-or-uncomment-region
  "cv" 'evilnc-toggle-invert-comment-line-by-line
  "."  'evilnc-copy-and-comment-operator
  "\\" 'evilnc-comment-operator ; if you prefer backslash key
)

(setq sml/no-confirm-load-theme t)
(setq sml/theme 'dark)
(sml/setup)
;;(load-theme 'afternoon t)


(require 'buffer-move)
(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)
