;;; Set up package management, adding various sources to search
(package-initialize nil)

(add-to-list
 'package-archives
 '("melpa" . "http://melpa.org/packages/") t)

;; Get the "use-package" package for simple package configuration
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-verbose t)
(setq use-package-always-ensure t)
(require 'use-package)
(setq load-prefer-newer t)

;; Company-mode for autocompletion
(use-package company
  :demand t
  :bind (("C-." . company-complete))
  :diminish company-mode)

(add-hook 'after-init-hook 'global-company-mode)

;; Helm for file navigation and autocomplete UI
(use-package helm
  :demand t
  :diminish helm-mode
  :bind (("M-x" . helm-M-x)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("C-h SPC" . helm-all-mark-rings)
         ("M-y" . helm-show-kill-ring))
  :config
  (progn
    (bind-key "<tab>" 'helm-execute-persistent-action helm-map)
    (bind-key "C-i" 'helm-execute-persistent-action helm-map)
    (bind-key "C-z" 'helm-select-action helm-map)
    (setq helm-google-suggest-use-curl-p t
          helm-quick-update t
          helm-split-window-in-side-p t
          helm-scroll-amount 4
          helm-buffers-fuzzy-matching t
          helm-recentf-fuzzy-match t
          helm-M-x-fuzzy-match t)
    (helm-mode)))


(use-package lsp-mode
  :diminish "L"
  :init (setq lsp-keymap-prefix "C-l"
              lsp-enable-file-watchers nil
              lsp-enable-on-type-formatting nil
              lsp-enable-indentation nil
              lsp-diagnostic-package :none)

  :hook ((c-mode-common . lsp-deferred)
         (python-mode . lsp-deferred))
  :commands (lsp lsp-deferred))


(setq lsp-file-watch-threshold nil)
(add-hook 'c++-mode-hook 'lsp)
(add-hook 'c-mode-hook 'lsp)

(add-hook 'c-mode-hook
      (lambda ()
        (setq c-indent-offset 8)
        (setq c-indent-level 8)
        (setq tab-width 8)))

(use-package lsp-ui
  :ensure t
  :config
  (require 'lsp-ui)
  (setq lsp-ui-doc-position 'at-point
         lsp-ui-doc-alignment 'window
         lsp-ui-doc-include-signature nil
         lsp-ui-sideline-show-symbol nil)
  )

(add-hook 'lsp-mode-hook 'lsp-ui-mode)

;; evil
(use-package evil
  :init
  (setq evil-want-integration nil)
  (progn
    (use-package evil-leader
      :init (global-evil-leader-mode)
      :config
      (progn
        (evil-leader/set-leader ",")
        (setq evil-leader/in-all-states t)
        (evil-leader/set-key
          "ci" 'evilnc-comment-or-uncomment-lines
          "b" 'helm-buffers-list
          "f" 'helm-find-files
          "g" 'magit-status
          "x" 'helm-M-x)
          "e" 'helm-projectile
          "b" 'helm-mini
          "s" 'helm-projectile-grep
          "z" 'previous-buffer
          "x" 'next-buffer
          "c" 'kill-buffer
          "v" 'split-window-below
          "h" 'split-window-right
          "w" 'other-window
          "t" 'term
          "r" 'term-char-mode
          "a" 'org-agenda)))
        :config
          (evil-set-undo-system 'undo-tree)
    (evil-mode t))

(define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
(define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)

(define-key evil-normal-state-map (kbd "C-]") 'xref-find-definitions)
(define-key evil-normal-state-map (kbd "C-r") 'xref-find-references)
(define-key evil-normal-state-map (kbd "C-\\") 'find-tag)
(define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop)

;; ccls
(use-package ccls
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda () (require 'ccls) (lsp)))
  :config
  (dolist (dir '(".ccls-cache" "build"))
    (add-to-list 'lsp-file-watch-ignored dir))
  (setq ccls-executable "~/.local/bin/ccls")
  (setq ccls-args '("--init={\"cache\": {\"directory\": \"/home/alvaro/.cache/ccls-cache\"}}"))
)

(add-hook 'c-mode-common-hook #'hs-minor-mode)

;; Projectile for large project utilities
(use-package projectile
  :demand t
  :diminish projectile-mode
  :config
  (progn
    (projectile-global-mode t)
    (setq projectile-completion-system 'helm)
    (setq projectile-enable-caching t)))

(use-package helm-projectile
  :bind (("C-x f" . helm-projectile-find-file)
         ("C-x g" . helm-projectile-grep))
  :config (helm-projectile-on))


;; ripgrep-search-mode
(use-package ripgrep
  :if (executable-find "rg")
  :commands (ripgrep-regexp)
  :ensure t
  :bind (:map ripgrep-search-mode-map
              ("n" . next-error-no-select)
              ("p" . previous-error-no-select)))

(use-package projectile-ripgrep
  :if (executable-find "rg")
  :ensure t
  :commands (projectile-ripgrep)
  :bind (:map projectile-mode-map
              ("C-x C-g" . projectile-ripgrep))
  :config (require 'projectile-settings))

;; Paradox for better package viewing (use M-x paradox-list-packages)
(use-package paradox :config (setq paradox-execute-asynchronously t))

;; Better text replacement
(use-package visual-regexp
  :bind (("M-%" . vr/query-replace))
  :config
  (use-package visual-regexp-steroids))

;; Magit for git integrated to emacs
(use-package magit :bind (("C-c C-g" . magit-status)))

;; For fun looking delimeters
(use-package rainbow-delimiters
  :demand t
  :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; Load in environment variables to shells we use
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (mapc 'exec-path-from-shell-copy-env
          '("LD_LIBRARY_PATH" "DYLD_LIBRARY_PATH" "CAML_LD_LIBRARY_PATH"))
    (exec-path-from-shell-initialize)))

;; Go back to where you were if you close a file
(use-package saveplace :config (setq-default save-place t))

;; Highlight lines over N characters
(use-package whitespace
  :diminish whitespace-mode
  :config
  (progn
    (setq whitespace-line-column 120)
    (setq whitespace-style '(face lines-tail))
    (add-hook 'prog-mode-hook 'whitespace-mode)))

;; Language-specific syntax highlighting
; (use-package rust-mode
  ; :mode ("\\.rs$" . rust-mode)
  ; :config
  ; (progn
    ; (setq rust-indent-offset 4
          ; rust-indent-level 4)))

; (add-hook 'rust-mode-hook #'lsp)
; (add-hook 'rust-mode-hook 'cargo-minor-mode)
(add-hook 'rust-mode-hook #'hs-minor-mode)

; (setq lsp-rust-server 'rust-analyzer)
;
(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  ;; (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t)))

(use-package markdown-mode)

(use-package c++-mode
  :ensure nil
  :mode ("\\.h$"   . c++-mode)
  :mode ("\\.inl$" . c++-mode)
  :config
  (progn
    (setq c-default-style "linux")
    (c-set-offset 'innamespace 0) ; No indent in namespace
    (c-set-offset 'arglist-intro '+)
    (c-set-offset 'arglist-close 0)
    (setq c-basic-offset 8
          c-indent-level 8)))

;; undo-tree
(use-package undo-tree
  :config
  (global-undo-tree-mode))

;; enable mouse
(require 'mouse)
(xterm-mouse-mode t)
(mouse-wheel-mode t)

;; yasnippet
(use-package yasnippet
  :ensure t
  :config
  (yas-reload-all)
  (add-to-list 'yas-snippet-dirs (expand-file-name "snippets" user-emacs-directory))
  (setq tab-always-indent 'complete)
  (setq yas-prompt-funcitons '(yas-completing-prompt
                               yas-ido-prompt
                               yas-dropdown-prompt))
  (define-key yas-minor-mode-map (kbd "<escape>") 'yas-exit-snippet)
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t)

(use-package fzf
  :ensure t)

(global-set-key (kbd "C-c p p") 'fzf-find-file)

(use-package term
  :config
  (setq explicit-shell-file-name "zsh") ;; Change this to zsh, etc
  ;;(setq explicit-zsh-args '())         ;; Use 'explicit-<shell>-args for shell-specific args

  ;; Match the default Bash shell prompt.  Update this if you have a custom prompt
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

(add-hook 'term-mode-hook (lambda () (read-only-mode -1)))

;; clipboard
(use-package xclip)

(xclip-mode 1)

;; python
(use-package python
  :mode ("\\.py" . python-mode)
  :ensure t
  :config
  (flymake-mode) ;; <- This line makes the trick of disabling flymake in python mode!
  (use-package elpy
    :ensure t
    :init
    (add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
    :config
    (remove-hook 'elpy-modules 'elpy-module-flymake) ;; <- This removes flymake from elpy
    (setq elpy-rpc-backend "jedi")
    :bind (:map elpy-mode-map
              ("M-." . elpy-goto-definition)
              ("M-," . pop-tag-mark))
  )
  (elpy-enable)
)

(add-hook 'python-mode-hook
      (lambda ()
        (setq indent-tabs-mode nil)
        (setq python-indent 4)
        (setq tab-width 4)
        (flymake-mode))
      (untabify (point-min) (point-max)))

(require 'whitespace)
(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face lines-tail))

(add-hook 'prog-mode-hook 'whitespace-mode)

;;;; Language-specific extensions
(provide 'config-packages)
