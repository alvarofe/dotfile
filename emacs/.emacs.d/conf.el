
(require 'package)

(setq package-list '(evil rust-mode magit fzf evil xclip linum-relative lsp-ui
                          ace-window use-package org helm evil-leader elpy
                          projectile rust-mode racer company ccls ivy swiper
                          counsel))

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
	(package-install package)))


(add-to-list 'load-path "~/.emacs.d/lsp-mode")
(require 'lsp-mode)
(setq lsp-file-watch-threshold nil)
(add-hook 'c++-mode-hook 'lsp)
(add-hook 'c-mode-hook 'lsp)

;; lsp-ui
(require 'lsp-ui)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)
(add-hook 'after-init-hook 'global-company-mode)
(setq lsp-diagnostic-package :none)

;; ccls
(setq ccls-executable "/usr/bin/ccls")
(use-package ccls
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda () (require 'ccls) (lsp))))

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

(define-key evil-normal-state-map (kbd "C-]") 'xref-find-definitions)
(define-key evil-normal-state-map (kbd "C-r") 'xref-find-references)
(define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop)

;; fuzzy file find

(global-set-key (kbd "C-c p p") 'counsel-fzf)

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
(setq-default c-basic-offset 2
          tab-width 2)

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
(require 'rust-mode)

(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)

(global-set-key (kbd "C-c d") "M-.")

(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)

(setq-default tab-width 4) 
(when (fboundp 'electric-indent-mode) (electric-indent-mode -1))

(defun indent-or-complete ()
	(interactive)
	(if (looking-at "\\_>")
		(company-complete-common)
		(indent-according-to-mode)))

;; linum-relative
(require 'linum-relative)

;; Custom themes
(add-to-list 'custom-theme-load-path "themes")
;; (load-theme 'vscode-dark-plus t)

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

(use-package counsel
  :ensure t
  :bind
  (("M-y" . counsel-yank-pop)
   :map ivy-minibuffer-map
   ("M-y" . ivy-next-line)))

(use-package ivy
  :ensure t
  :diminish (ivy-mode)
  :bind (("C-x b" . ivy-switch-buffer))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "%d/%d ")
  (setq ivy-display-style 'fancy))


(use-package swiper
  :ensure t
  :bind (("C-c C-s" . swiper-isearch)
         ("C-c C-r" . ivy-resume)
         ("M-x" . counsel-M-x))
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-display-style 'fancy)
    ))

(icomplete-mode 1)

(add-hook 'eshell-mode-hook
          (lambda ()
            ;; (setq pcomplete-ignore-case t)
            ;; (setq pcomplete-cycle-completions nil)

            ;; These have to be done in a hook because
            ;; eshell-command-map is a buffer local variable.  It's
            ;; apparently not supposed to be and appears to to be a
            ;; bug in the way eshell is implemented.
            ;; eshell-mode-map:    <key>
            ;; ehsell-command-map: C-c <key>
            (define-key eshell-command-map (kbd "C-t") 'eshell-truncate-buffer-all)
            (define-key eshell-command-map (kbd "M-o") 'eshell-truncate-buffer-all)
            (define-key eshell-mode-map (kbd "M-r") 'counsel-esh-history)
            (define-key eshell-mode-map (kbd "<tab>")
              (lambda () (interactive) (pcomplete-std-complete)))
            )
          )
