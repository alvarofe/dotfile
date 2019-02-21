(require 'package)

(setq package-list '(evil ggtags rust-mode magit fzf evil xclip yasnippet linum-relative lsp-ui company-lsp clang-format ace-window use-package xcscope cquery evil-tabs))

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
	(package-install package)))


(add-to-list 'load-path "~/.emacs.d/lsp-mode")
(require 'lsp-mode)

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

;; ggtags

(eval-after-load 'ggtags
  '(progn
     (evil-make-overriding-map ggtags-mode-map 'normal)
     ;; force update evil keymaps after ggtags-mode loaded
          (add-hook 'ggtags-mode-hook #'evil-normalize-keymaps)))

;; magit config
(global-set-key (kbd "C-x g") 'magit-status)


;; evil config

(setq evil-toggle-key "C-q")
(require 'evil)
(evil-mode 1)

(global-evil-tabs-mode t)

(eval-after-load 'evil-maps
				 '(define-key evil-normal-state-map (kbd "M-.") nil))

;;(define-key evil-normal-state-map (kbd "C-]") 'ggtags-find-tag-dwim)
(define-key evil-normal-state-map (kbd "C-]") 'xref-find-definitions)
;;(define-key evil-normal-state-map (kbd "C-r") 'ggtags-find-reference)
(define-key evil-normal-state-map (kbd "C-r") 'xref-find-references)
(define-key evil-normal-state-map (kbd "C-f") 'ggtags-find-definition)
(define-key evil-normal-state-map (kbd "C-u") 'xref-find-references)
(define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop)

;; ggtags (gnu global)
(eval-after-load 'ggtags
  (setq ggtags-enable-navigation-keys nil))
(add-hook 'c-mode-common-hook
	  (lambda ()
	    (when (derived-mode-p 'c-mode 'c++-mode)
	      (ggtags-mode 1))))

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
; Change default colors
;; (set-background-color "grey14")
;; (set-foreground-color "white")
;; (set-cursor-color "white")

(add-hook 'c-mode-common-hook #'hs-minor-mode)

;; enable mouse

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 5))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

(xterm-mouse-mode 1)

(global-set-key [mouse-4] 'scroll-down-line)
(global-set-key [mouse-5] 'scroll-up-line)

;; company-lsp / company-mode

(add-hook 'after-init-hook 'global-company-mode)
(require 'company-lsp)
(push 'company-lsp company-backends)

;; lsp-ui
(require 'lsp-ui)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)

;; cquery 

(setq cquery-executable "/usr/local/bin/cquery")
(setq cquery-extra-init-params '(:index (:comments 2) :cacheFormat "msgpack" :completion (:detailedLabel t)))

(defun cquery//enable ()
  (condition-case nil
      (lsp)
    (user-error nil)))

(use-package cquery
  :commands lsp
  :init 
  (add-hook 'c-mode-hook #'cquery//enable)
  (add-hook 'c++-mode-hook #'cquery//enable))

(require 'cquery)

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

;; yasnippet

(require 'yasnippet)
(yas-global-mode 1)

;; backup files
(setq backup-directory-alist `(("." . "~/.saves")))

;; coding style linux

(setq c-default-style "linux")

(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
	 (column (c-langelem-2nd-pos c-syntactic-element))
	 (offset (- (1+ column) anchor))
	 (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

(add-hook 'c-mode-common-hook
	  (lambda ()
	    ;; Add kernel style
	    (c-add-style
	     "linux-tabs-only"
	     '("linux" (c-offsets-alist
			(arglist-cont-nonempty
			 c-lineup-gcc-asm-reg
			 c-lineup-arglist-tabs-only))))))

(add-hook 'c-mode-hook
	  (lambda ()
	    (setq indent-tabs-mode t)
	    (setq show-trailing-whitespace t)
	    (c-set-style "linux-tabs-only")))
;; text mode
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook
  '(lambda() (set-fill-column 80)))

;; c-h with backspace issues

(global-set-key [?\C-h] 'delete-backward-char)
(global-set-key [?\C-x ?h] 'help-command)
