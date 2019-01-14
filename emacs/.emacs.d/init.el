(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#3c3836" "#fb4933" "#b8bb26" "#fabd2f" "#83a598" "#d3869b" "#8ec07c" "#ebdbb2"])
 '(custom-safe-themes
   (quote
    ("84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "2a9039b093df61e4517302f40ebaf2d3e95215cb2f9684c8c1a446659ee226b9" "e2fd81495089dc09d14a88f29dfdff7645f213e2c03650ac2dd275de52a513de" "a622aaf6377fe1cd14e4298497b7b2cae2efc9e0ce362dade3a58c16c89e089c" "d8dc153c58354d612b2576fea87fe676a3a5d43bcc71170c62ddde4a1ad9e1fb" default)))
 '(package-selected-packages
   (quote
    (rust-mode yasnippet xclip smart-mode-line-powerline-theme smart-mode-line powerline linum-relative gruvbox-theme abyss-theme ccls company-lsp lsp-ui clang-format evil-vimish-fold ace-window ggtags use-package magit fzf evil)))
 '(pdf-view-midnight-colors (quote ("#fdf4c1" . "#32302f"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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

(eval-after-load 'evil-maps
  '(define-key evil-normal-state-map (kbd "M-.") nil))

(define-key evil-normal-state-map (kbd "C-]") 'ggtags-find-tag-dwim)
;;(define-key evil-normal-state-map (kbd "C-]") 'xref-find-definitions)
(define-key evil-normal-state-map (kbd "C-r") 'ggtags-find-reference)
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

(global-set-key (kbd "C-c p p") 'fzf)


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

;; ccls

;;(use-package lsp-mode :commands lsp)
;;(use-package lsp-ui :commands lsp-ui-mode)
;;(use-package company-lsp :commands company-lsp)

;;(use-package ccls
;;  :hook ((c-mode c++-mode objc-mode) .
;;        (lambda () (require 'ccls) (lsp))))

;;(setq ccls-executable "/usr/local/bin/ccls")
;; theme
(load-theme #'tango-dark t)


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
