(require 'package)

(setq package-list '(evil rust-mode magit fzf evil xclip
			  linum-relative lsp-ui clang-format
			  ace-window use-package xcscope evil-tabs org
			  helm evil-leader elpy projectile rust-mode racer
			  company  typescript-mode ccls fold-this))

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

;; ccls

;; (require 'ccls)
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

(global-evil-tabs-mode t)

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
(define-key evil-normal-state-map (kbd "C-[") 'find-tag)
(define-key evil-normal-state-map (kbd "C-r") 'xref-find-references)
;; (define-key evil-normal-state-map (kbd "C-f") 'ggtags-find-definition)
(define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop)

;; fuzzy file find

(global-set-key (kbd "C-c p p") 'fzf-git)


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

;; cscope
(require 'xcscope)
(cscope-setup)

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
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook
  '(lambda() (set-fill-column 80)))

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

;; clang-format
;; (require 'clang-format)
;; (global-set-key (kbd "C-c i") 'clang-format-region)
;; (global-set-key (kbd "C-c u") 'clang-format-buffer)

(setq clang-format-style-option "LLVM")

;; rust
(require 'rust-mode)

(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)

(add-hook 'racer-mode-hook #'company-mode)

(global-set-key (kbd "C-c d") "M-.")

(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)

; (setq indent-tabs-mode nil)
; (setq c-default-style
      ; '((c++-mode . "k&r") (awk-mode . "awk") (other . "gnu")))
(setq-default tab-width 2) ; set tab width to 4 for all buffers
(when (fboundp 'electric-indent-mode) (electric-indent-mode -1))

(defun indent-or-complete ()
	(interactive)
	(if (looking-at "\\_>")
		(company-complete-common)
		(indent-according-to-mode)))

;; linum-relative
(require 'linum-relative)

;; fold-this
(global-set-key (kbd "C-c C-f") 'fold-this)
(global-set-key (kbd "C-c C-u") 'fold-this-unfold-all)

;; Custom themes
(add-to-list 'custom-theme-load-path "themes")
(load-theme 'alect-dark t)


;; enable mouse
;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 5))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

(xterm-mouse-mode 1)

(global-set-key [mouse-4] 'scroll-down-line)
(global-set-key [mouse-5] 'scroll-up-line)
