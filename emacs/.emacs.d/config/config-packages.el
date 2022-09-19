(use-package diminish)

(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Company-mode for autocompletion
(use-package company
  :demand t
  :bind (("C-." . company-complete))
  :diminish company-mode)


(setq evil-want-keybinding nil)

(require 'use-package)
(require 'quelpa-use-package)
(use-package quelpa)

(add-hook 'after-init-hook 'global-company-mode)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq tab-bar-show nil)

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

(use-package rg)
(use-package helm-rg)
(use-package bpr)

(require 'vc)

(defun gtags-reindex ()
    "Kick off gtags reindexing."
    (interactive)
    (let* ((root-path (expand-file-name (vc-git-root (buffer-file-name))))
           (gtags-filename (expand-file-name "GTAGS" root-path)))
      (if (file-exists-p gtags-filename)
          (gtags-index-update root-path)
        (gtags-index-initial root-path))))

  (defun gtags-index-initial (path)
    "Generate initial GTAGS files for PATH."
    (let ((bpr-process-directory path))
      (bpr-spawn "gtags")))

  (defun gtags-index-update (path)
    "Update GTAGS in PATH."
    (let ((bpr-process-directory path))
      (bpr-spawn "global -uv")))

(use-package ggtags)

(defun gtags-root-dir ()
"Returns GTAGS root directory or nil if doesn't exist."
(with-temp-buffer
    (if (zerop (call-process "global" nil t nil "-pr"))
        (buffer-substring (point-min) (1- (point-max)))
    nil)))

(defun gtags-update-single(filename)
    "Update Gtags database for changes in a single file"
    (interactive)
    (start-process "update-gtags" "update-gtags" "bash" "-c" (concat "cd " (gtags-root-dir) " ; gtags --single-update " filename )))

(defun gtags-update-current-file()
    (interactive)
    (defvar filename)
    (setq filename (replace-regexp-in-string (gtags-root-dir) "." (buffer-file-name (current-buffer))))
    (gtags-update-single filename)
    (message "Gtags updated for %s" filename))

(defun gtags-update-hook()
    "Update GTAGS file incrementally upon saving a file"
    (when helm-gtags-mode
    (when (gtags-root-dir)
      (gtags-update-current-file))))

(with-eval-after-load 'ggtags
  (add-hook 'c-mode-hook   'ggtags-mode)
  (add-hook 'c++-mode-hook 'ggtags-mode)
  (add-hook 'asm-mode-hook 'ggtags-mode)
)

(use-package helm-gtags)

(use-package bm)

(global-set-key (kbd "<f2>") 'bm-toggle)
(global-set-key (kbd "<S-f2>")   'bm-next)

(use-package helm-bm)

(global-set-key (kbd "M-*")  'helm-bm)

(autoload 'bm-toggle   "bm" "Toggle bookmark in current buffer." t)
(autoload 'bm-next     "bm" "Goto bookmark."                     t)
(autoload 'bm-previous "bm" "Goto previous bookmark."            t)

(setq bm-highlight-style 'bm-highlight-only-fringe)
(setq bm-cycle-all-buffers t)

(autoload 'helm-bm "helm-bm" "List All Bookmarks with helm.el." t)
(autoload 'bm-bookmark-add "bm" "Add bookmark at current line.")

(with-eval-after-load 'helm-gtags
  (add-hook 'helm-gtags-goto-line-before-hook 'bm-bookmark-add)
  (add-hook 'helm-gtags-quit-or-no-candidates-hook 'bm-bookmark-remove)
  (add-hook 'helm-gtags-goto-line-after-hook 'bm-bookmark-remove)
)

(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)

;; (use-package eglot)

;; (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))

(use-package lsp-mode
  :diminish "L"
  :init (setq lsp-keymap-prefix "C-l"
              lsp-enable-file-watchers nil
              lsp-enable-on-type-formatting nil
              lsp-enable-indentation nil
              lsp-enable-lenses nil
              lsp-diagnostic-package :none)
  :hook (((c-mode-common python-mode c++-mode) . lsp-deferred))
  :commands (lsp lsp-deferred))

 (with-eval-after-load 'lsp-mode
   (lsp-register-client
    (make-lsp-client
     :new-connection (lsp-tramp-connection "ccls")
     :major-modes '(c++-mode)
     :remote? t
     ))
   (lsp-register-client
    (make-lsp-client
     :new-connection (lsp-tramp-connection "ccls")
     :major-modes '(c-mode)
     :remote? t
     ))
   )

(setq lsp-file-watch-threshold nil)
(setq lsp-lens-enable nil)

(with-eval-after-load 'lsp
        (add-hook 'c++-mode-hook 'lsp)
        (add-hook 'c-mode-hook 'lsp)
        )

;; (add-hook 'c-mode-hook 'eglot-ensure)
;; (add-hook 'c++-mode-hook 'eglot-ensure)

(add-hook 'c-mode-hook
      (lambda ()
        (setq c-indent-offset 8)
        (setq indent-tabs-mode t)
        (setq c-basic-offset 8)
        (setq tab-width 8)
        (tree-sitter-hl-mode)
        ))

(add-hook 'c++-mode-hook
      (lambda ()
        (setq c-indent-offset 2)
        (setq indent-tabs-mode nil)
        (setq c-indent-level 2)
        (setq tab-width 2)
        (c-set-offset 'arglist-intro 4)
        (c-set-offset 'arglist-cont-nonempty 4)
        (c-set-offset 'arglist-close 4)
        (c-set-offset 'stream-op 4)
        (c-set-offset 'template-args-cont 4)
        (tree-sitter-hl-mode)
      )
      )
(use-package lsp-ui
  :ensure t
  :after (lsp-mode)
  :commands lsp-ui-mode
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-find-definitions)
              ([remap xref-find-references] . lsp-find-references)
              ;; ("C-c u" . lsp-ui-imenu))
              )
  :init (setq
              lsp-ui-doc-enable t
              lsp-ui-doc-position 'at-point
              lsp-ui-doc-use-webkit nil
              lsp-ui-doc-header nil
              lsp-ui-doc-delay 0.1
              lsp-ui-doc-frame 'frame
              ;; lsp-ui-doc-show-with-cursor t
              lsp-ui-doc-include-signature t
              lsp-ui-doc-alignment 'frame
              lsp-ui-doc-use-childframe t
              ;; lsp-ui-doc-border (face-foreground 'default)
              lsp-ui-peek-enable t
              lsp-ui-peek-show-directory t
              lsp-ui-sideline-update-mode 'line
              ;; lsp-ui-sideline-enable t
              ;; lsp-ui-sideline-show-code-actions t
              ;; lsp-ui-sideline-show-diagnostics t
              ;; lsp-ui-sideline-show-hover t
              ;; lsp-ui-sideline-ignore-duplicate t)
  :config
  ;; (add-to-list 'lsp-ui-doc-frame-parameters '(right-fringe . 8))

  ;; Reset `lsp-ui-doc-background' after loading theme
  (add-hook 'after-load-theme-hook
            (lambda ()
              (setq lsp-ui-doc-border (face-foreground 'default))
              (set-face-background 'lsp-ui-doc-background
                                   (face-background 'tooltip))))
  ;; WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
  ;; @see https://github.com/emacs-lsp/lsp-ui/issues/243
  (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
    (setq mode-line-format nil)))
  )

;; (with-eval-after-load 'lsp-ui-doc
;;   (lsp-ui-doc-frame-mode +1))


;; undo-tree
(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (progn
    (setq undo-tree-enable-undo-in-region t)
    (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
    )
  (global-undo-tree-mode))

(use-package evil-nerd-commenter)

;; evil
(use-package evil
  :diminish evil-mode
  :init
  (setq evil-want-integration nil)
  (setq evil-want-keybinding nil)
  (progn
    (use-package evil-leader
      :init (global-evil-leader-mode)
      :config
      (evil-set-undo-system 'undo-tree)
      (progn
        (evil-leader/set-leader ",")
        (setq evil-leader/in-all-states t)
        (evil-leader/set-key
          "c" 'evilnc-comment-or-uncomment-lines
          "C" 'emacs-workspaces/create-workspace
          "n" 'emacs-worskpaces/switch-workspace
          "b" 'helm-buffers-list
          "B" 'helm-bm
          "h" 'jpt-toggle-mark-word-at-point
          ;; "L" 'align-regexp
          "l" 'tab-list
          "f" 'helm-find-files
          "g" 'magit-status
          "x" 'helm-M-x
          "e" 'helm-projectile
          "m" 'helm-mini
          "M" 'minimap-mode
          "s" 'ace-swap-window
          "z" 'sr-speedbar-toggle
          "d" 'lsp-ui-doc-show
          "D" 'lsp-ui-doc-hide
          "k" 'kill-buffer
          "v" 'async-shell-command
          ;; "w" 'other-window
          "t" 'gtags-reindex
          "T" 'multi-term
          "p" 'multi-term-prev
          "r" 'term-char-mode
          "a" 'align-regexp))))
  (evil-mode t))

(evil-global-set-key 'normal
                     (kbd "q") nil)
(evil-global-set-key 'normal
                     (kbd "Q") 'evil-record-macro)


(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(define-key evil-normal-state-map (kbd "C-]") 'xref-find-definitions)
(define-key evil-normal-state-map (kbd "C-,") 'xref-pop-marker-stack)
(define-key evil-normal-state-map (kbd "C-r") 'xref-find-references)
(define-key evil-normal-state-map (kbd "C-\\") 'helm-gtags-find-tag)
(define-key evil-normal-state-map (kbd "<f1>") 'helm-gtags-pop-stack)
(define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop)
(define-key evil-normal-state-map (kbd "C-+") 'text-scale-increase)
(define-key evil-normal-state-map (kbd "C--") 'text-scale-decrease)
(define-key evil-normal-state-map "u" 'undo-tree-undo)


(add-hook 'org-mode-hook
          '(lambda()
             (define-key evil-normal-state-map (kbd "TAB") 'org-cycle)
             ))

;; ccls
(use-package ccls
  :after lsp
  :hook ((c-mode c++-mode) .
         (lambda () (require 'ccls) (lsp)))
  :config
  (dolist (dir '(".ccls-cache" "build"))
    (add-to-list 'lsp-file-watch-ignored dir))
  (setq ccls-executable "~/.local/bin/ccls")
  (setq ccls-args '("--init={\"cache\": {\"directory\": \"/home/alvaro/.cache/ccls-cache\"}}"))
)

(add-hook 'c-mode-common-hook #'hs-minor-mode)


(use-package projectile
  :demand t
  :diminish projectile-mode
  :init
  (progn
    (setq projectile-enable-caching t)
    (setq projectile-tags-command "/usr/bin/ctags -Re -f \"%s\" %s")
    (setq projectile-cache-file
          (expand-file-name "cache/projectile.cache" user-emacs-directory)))
    (setq projectile-mode-line '(:eval (format " [%s]" (projectile-project-name))))

  :config
  (progn

    (projectile-global-mode t)
    ;; Use faster search tools: ripgrep or the silver search
    (let ((command
           (cond
            ((executable-find "rg")
             "rg -0 --files --color=never --hidden --sort-files"))))
      (setq projectile-generic-command command))

    (setq-default projectile-mode-line nil)
    (setq projectile-completion-system 'ivy)
    (setq projectile-indexing-method 'alien)
    (setq projectile-switch-project-action 'projectile-dired)
    (defadvice projectile-dired (after xah-wrapper-dired-commands activate)
      (xah-wrapper-dired-commands))
    (setq projectile-globally-ignored-directories
          '(".idea"
            ".ccls-cache"
            ".git"
            ".hg"
            ".svn"
            "build"))

    (add-to-list 'projectile-globally-ignored-files "*.pyc")
    (add-to-list 'projectile-globally-ignored-files "*.python-version")
    (add-to-list 'projectile-globally-ignored-files "*.egg-info")
    (add-to-list 'projectile-globally-ignored-files "*.class")
    (add-to-list 'projectile-globally-ignored-files "*.jar")
    (add-to-list 'projectile-globally-ignored-files "*.tar")
    (add-to-list 'projectile-globally-ignored-files "*.tar.gz")
    (add-to-list 'projectile-globally-ignored-files "*.zip")
    (add-to-list 'projectile-globally-ignored-files "*.el~")
    (add-to-list 'projectile-globally-ignored-files "*.swp")
    (add-to-list 'projectile-globally-ignored-files "compile_commands.json")
    (setq projectile-globally-ignored-files (append '(".ensime"
                                                      ".gitignore"
                                                      ".bintray"
                                                      ".travis.yml"
                                                      ".mode"
                                                      ".cask")
                                                    projectile-globally-ignored-files))

    (add-to-list 'projectile-globally-ignored-directories "__pycache__")
    (add-to-list 'projectile-globally-ignored-directories ".env")
    (add-to-list 'projectile-globally-ignored-directories ".venv")
    (add-to-list 'projectile-globally-ignored-directories ".cask")
    (add-to-list 'projectile-globally-ignored-directories ".cache")
    (add-to-list 'projectile-globally-ignored-directories "elpa")
    (add-to-list 'projectile-globally-ignored-directories ".node_modules")
    (add-to-list 'projectile-globally-ignored-directories ".m2")
    (setq projectile-sort-order 'recently-active)

    (projectile-mode)))

;; Recommended keymap prefix on Windows/Linux
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(use-package helm-projectile
  :bind (("C-x f" . helm-projectile-find-file)
         ("C-x g" . helm-projectile-grep))
  :config (helm-projectile-on))

(add-to-list 'projectile-globally-ignored-directories ".cache")
(add-to-list 'projectile-globally-ignored-directories ".pytest_cache")
(add-to-list 'projectile-globally-ignored-directories "__pycache__")
(add-to-list 'projectile-globally-ignored-directories "build")
(add-to-list 'projectile-globally-ignored-directories "dist")
(add-to-list 'projectile-globally-ignored-directories "docsets")
(add-to-list 'projectile-globally-ignored-directories "elpa")
(add-to-list 'projectile-globally-ignored-directories "man")
(add-to-list 'projectile-globally-ignored-directories "node_modules")
(add-to-list 'projectile-globally-ignored-directories "venv")
(add-to-list 'projectile-globally-ignored-directories ".ccls-cache")

;; (use-package swiper-helm
;;   ;; :bind (("C-s" . my/isearch-forward)
;;   ;;        ("C-r " . my/isearch-backward))
;;   :config
;;   (setq swiper-helm-display-function 'display-buffer)
;;   (defun my/isearch-forward (&optional regexp-p no-recursive-edit)
;;     (interactive "P\np")
;;     (cond ((equal current-prefix-arg nil)
;;            (if (minibufferp)
;;                (isearch-forward)
;;              (swiper-helm)))
;;           ((equal current-prefix-arg '(4)) (isearch-forward-regexp))
;;           (t (isearch-forward))))
;;   (defun my/isearch-backward (&optional regexp-p no-recursive-edit)
;;     (interactive "P\np")
;;     (cond ((equal current-prefix-arg nil)
;;            (if (minibufferp)
;;                (isearch-backward)
;;              (swiper-helm)))
;;           ((equal current-prefix-arg '(4)) (isearch-backward-regexp))
;;           (t (isearch-backward)))))


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
(use-package exec-path-from-shell)
(exec-path-from-shell-initialize)

;; Go back to where you were if you close a file
(use-package saveplace :config (setq-default save-place t))

;; Highlight lines over N characters
(use-package whitespace
  :diminish whitespace-mode
  :config
  (progn
    (setq whitespace-line-column 80)
    (setq whitespace-style '(face lines-tail))
    (add-hook 'c-mode-hook 'whitespace-mode)
    (add-hook 'c++-mode-hook 'whitespace-mode)
    (add-hook 'rust-mode-hook 'whitespace-mode)
    ))

;; (setq whitespace-style '(tabs tab-mark))
(setq whitespace-style '(tabs))

(add-hook 'rust-mode-hook #'hs-minor-mode)

(add-hook 'rust-mode-hook
      (lambda ()
        (setq rust-indent-offset 4)
        (setq rust-indent-level 4)
        (setq indent-tabs-mode nil)
        (setq tab-width 4)))

; (setq lsp-rust-server 'rust-analyzer)
;
(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ;; ("M-j" . lsp-ui-imenu)
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
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook)
  )


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
  :mode ("\\.hpp$"   . c++-mode)
  :config
  (progn
    (c-set-offset 'innamespace 0) ; No indent in namespace
    (c-set-offset 'arglist-intro '+)
    (setq c-basic-offset 2
          c-indent-level 2)))

(add-hook 'c-mode-hook
          '(lambda()
             (setq c-default-style "linux")
             (setq c-basic-offset 8)
             (setq tab-width c-basic-offset)
             (setq indent-tabs-mode nil)
             (setq comment-start "/* ")
             (setq comment-end "*/")
             (define-key c-mode-base-map (kbd "C-c c")   'compile)
             (define-key c-mode-base-map (kbd "C-c C-c") 'quickrun)
             ))

;; yasnippet
(use-package yasnippet
  :diminish yas-minor-mode
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
  :commands fzf-find-file
  :ensure t)

(global-set-key (kbd "C-x p p") 'fzf-find-file)


(defadvice term-send-input (after update-current-directory)
  (run-at-time "0.1 sec" nil 'term-update-dir)
  )

(defadvice term-send-return (after update-current-directory)
  (run-at-time "0.1 sec" nil 'term-update-dir)
  )

(defun term-update-dir ()
  (let* ((pid (process-id (get-buffer-process (current-buffer))))
         (cwd (file-truename (format "/proc/%d/cwd" pid))))
    (unless (equal (file-name-as-directory cwd) default-directory)
      (cd cwd)))
  )

(use-package term
  :commands term
  :config
  (setq explicit-shell-file-name "zsh") ;; Change this to zsh, etc
  ;;(setq explicit-zsh-args '())         ;; Use 'explicit-<shell>-args for shell-specific args

  ;; Match the default Bash shell prompt.  Update this if you have a custom prompt
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

(add-hook 'term-mode-hook
          (lambda ()
            (read-only-mode -1)
            (ad-activate 'term-send-return)
            (ad-activate 'term-send-input)
            )
          )

(use-package multi-term
  :commands multi-term
  )

;; clipboard
(use-package xclip)

(xclip-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Python
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
    (setq elpy-rpc-python-command "python")
    :bind (:map elpy-mode-map
                ("M-." . elpy-goto-definition)
                ("M-," . pop-tag-mark))
    )
  (elpy-enable)
  )

(add-to-list 'process-coding-system-alist '("python" . (utf-8 . utf-8)))

(add-hook 'python-mode-hook
      (lambda ()
        (setq indent-tabs-mode nil)
        (setq python-indent 4)
        (setq tab-width 4)
        (flymake-mode)
        (hs-minor-mode)
        (tree-sitter-hl-mode)
        )
      (untabify (point-min) (point-max)))

(use-package lsp-pyright
  :hook (python-mode . (lambda () (require 'lsp-pyright)))
  :init (when (executable-find "python3")
          (setq lsp-pyright-python-executable-cmd "python3")))


(use-package all-the-icons)

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agXlho --group-directories-first"))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer))

(use-package dired-single
  :after dired
  )

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-open
  :after dired
  :config
  ;; Doesn't work as expected!
  ;;(add-to-list 'dired-open-functions #'dired-open-xdg t)
  (setq dired-open-extensions '(("png" . "feh")
                                ("mkv" . "mpv"))))

(use-package dired-hide-dotfiles
  :after dired
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

(use-package ace-window
  :commands ace-window
  :ensure t
  :init
  (progn
    (global-set-key [remap other-window] 'ace-window)
    ))

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

(use-package tramp
  :custom
  (tramp-use-ssh-controlmaster-options nil) ; Don't override SSH config.
  (tramp-default-method "ssh")    ; ssh is faster than scp and supports ports.
  )

(use-package js2-mode
  :mode ("\\.js" . js2-mode)
  :config
    (setq-default js2-basic-offset 2)
    (setq js-indent-level 2))

(use-package prettier-js
  :hook (
         (typescript-mode . prettier-js-mode)
         (js-mode . prettier-js-mode)
         (json-mode . prettier-js-mode)))

(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'prettier-js-mode)
(add-hook 'typescript-mode-hook 'prettier-mode)
(add-hook 'js2-mode-hook 'lsp)
(add-hook 'typescript-mode-hook 'lsp)



(diminish 'which-key-mode)
(diminish 'evil-collection-unimpaired-mode)
(diminish 'lisp-interaction-mode)
(diminish 'eldoc-mode)
(diminish 'helm-gtags-mode)
(diminish 'ggtags-mode)
(diminish 'abbrev-mode)
(diminish 'auto-fill-mode)
(diminish 'auto-revert-mode)
(diminish 'hs-minor-mode)

(use-package org)
(use-package org-bullets)

(add-hook 'org-mode-hook (lambda () (org-bullets-mode t)))
(setq org-hide-leading-stars t)
(setq org-src-fontify-natively t)

(use-package evil-org
  :ensure t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package dockerfile-mode)

(use-package typescript-mode
  :ensure t)

(use-package tide
  :ensure t
  :hook (typescript-mode . tide-setup)
        (typescript-mode . tide-hl-identifier-mode)
        (typescript-mode . flycheck-mode)
        (typescript-mode . company-mode)
        (js2-mode . tide-setup)
        (js2-mode . tide-hl-identifier-mode)
        (js2-mode . flycheck-mode)
        (js2-mode . company-mode)
        )

(use-package deno-fmt
  :ensure t
  :hook (js2-mode typescript-mode))

(use-package lsp-java :config (add-hook 'java-mode-hook 'lsp))
(use-package dap-mode :after lsp-mode :config (dap-auto-configure-mode))
(use-package dap-java :ensure nil)
(use-package helm-lsp)

(add-hook 'java-mode-hook
      (lambda ()
        (setq indent-tabs-mode t)
        (setq tab-width 4)))

(use-package minimap)

(use-package dts-mode)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(use-package emacs-workspaces
  :straight (:type git :host github :repo "mclear-tools/emacs-workspaces")
  :commands (emacs-workspaces/create-workspace
             emacs-workspaces/create-new-project-and-workspace
             emacs-workspaces/open-existing-project-and-workspace
             emacs-workspaces/switch-workspace))

(use-package sr-speedbar
  :ensure t
  :defer t
  :init
  (setq sr-speedbar-right-side nil)
  (setq sr-speedbar-left-side t)
  (setq speedbar-show-unknown-files t)
  (setq sr-speedbar-width 34)
  (setq sr-speedbar-max-width 35)
  (setq speedbar-use-images t)
  )

(use-package pdf-tools
  :quelpa (pdf-tools :fetcher github :repo "vedang/pdf-tools" :files ("lisp/*" "server/*"))
  :hook ((pdf-view-mode . disable-line-numbers)
         (pdf-view-mode . pdf-sync-minor-mode)
         (pdf-view-mode . pdf-links-minor-mode)
         (pdf-view-mode . pdf-history-minor-mode)
         (pdf-view-mode . pdf-annot-minor-mode)
         (pdf-view-mode . pdf-view-themed-minor-mode))
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :custom
  (pdf-view-display-size 'fit-page)
  (pdf-annot-activate-created-annotations nil)
  (pdf-view-resize-factor 1.1)
  :bind
  (:map pdf-view-mode-map
        ("M-w" . pdf-view-kill-ring-save)
        ("o"   . pdf-outline)
        ("b"   . pdf-history-backward)
        ("C-c p g" . pdf-view-goto-page)
  ))

  (with-eval-after-load 'pdf-tools
  (pdf-tools-install))

(use-package org-noter
  :config
  ;; Your org-noter config ........
  (require 'org-noter-pdftools))

(use-package org-pdftools
  :hook (org-mode . org-pdftools-setup-link))

(use-package org-noter-pdftools
  :after org-noter
  :config
  ;; Add a function to ensure precise note is inserted
  (defun org-noter-pdftools-insert-precise-note (&optional toggle-no-questions)
    (interactive "P")
    (org-noter--with-valid-session
     (let ((org-noter-insert-note-no-questions (if toggle-no-questions
                                                   (not org-noter-insert-note-no-questions)
                                                 org-noter-insert-note-no-questions))
           (org-pdftools-use-isearch-link t)
           (org-pdftools-use-freestyle-annot t))
       (org-noter-insert-note (org-noter--get-precise-info)))))

  ;; fix https://github.com/weirdNox/org-noter/pull/93/commits/f8349ae7575e599f375de1be6be2d0d5de4e6cbf
  (defun org-noter-set-start-location (&optional arg)
    "When opening a session with this document, go to the current location.
With a prefix ARG, remove start location."
    (interactive "P")
    (org-noter--with-valid-session
     (let ((inhibit-read-only t)
           (ast (org-noter--parse-root))
           (location (org-noter--doc-approx-location (when (called-interactively-p 'any) 'interactive))))
       (with-current-buffer (org-noter--session-notes-buffer session)
         (org-with-wide-buffer
          (goto-char (org-element-property :begin ast))
          (if arg
              (org-entry-delete nil org-noter-property-note-location)
            (org-entry-put nil org-noter-property-note-location
                           (org-noter--pretty-print-location location))))))))
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))

(global-set-key (kbd "<f5>") (lambda ()
                               (interactive)
                               (setq-local compilation-read-command nil)
                               (call-interactively 'compile)))

(use-package redacted)

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/n/roam/")
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert))
  :config
  (org-roam-setup))

(use-package pulsar
  :straight (pulsar :type git :host gitlab :repo "protesilaos/pulsar")
  :custom
  (pulsar-pulse-functions ; Read the doc string for why not `setq'
   '(recenter-top-bottom
     move-to-window-line-top-bottom
     reposition-window
     bookmark-jump
     other-window
     delete-window
     delete-other-windows
     forward-page
     backward-page
     scroll-up-command
     scroll-down-command
     windmove-right
     windmove-left
     windmove-up
     windmove-down
     windmove-swap-states-right
     windmove-swap-states-left
     windmove-swap-states-up
     windmove-swap-states-down
     tab-new
     tab-close
     tab-next
     org-next-visible-heading
     org-previous-visible-heading
     org-forward-heading-same-level
     org-backward-heading-same-level
     outline-backward-same-level
     outline-forward-same-level
     outline-next-visible-heading
     outline-previous-visible-heading
     outline-up-heading))
  :config
  (setq pulsar-face 'pulsar-blue)
  (setq pulsar-delay 0.055)
  (setq pulsar-pulse-on-window-change t)
  (setq pulsar-pulse t)
  (setq pulsar-iterations 10)
  )

;; integration with the `consult' package:
(add-hook 'consult-after-jump-hook #'pulsar-recenter-top)
(add-hook 'consult-after-jump-hook #'pulsar-reveal-entry)

;; integration with the built-in `imenu':
(add-hook 'imenu-after-jump-hook #'pulsar-recenter-top)
(add-hook 'imenu-after-jump-hook #'pulsar-reveal-entry)

(let ((map global-map))
  (define-key map (kbd "C-x l") #'pulsar-pulse-line)
  (define-key map (kbd "C-x L") #'pulsar-highlight-line))

(use-package tree-sitter
  :ensure t
  :init
  (global-tree-sitter-mode))

(use-package tree-sitter-langs
  :after (tree-sitter)
  :ensure t)

(use-package tree-sitter-hl
  :after (tree-sitter)
  :ensure t
  :hook (tree-sitter-after-on))

(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

;; (use-package dtrt-indent)

(use-package iedit)

(use-package anzu)

(use-package php-mode)

(use-package dtrt-indent
  :ensure t
  :config (dtrt-indent-mode t))

(use-package counsel
  :bind
  (("M-x" . counsel-M-x)
   ("M-y" . counsel-yank-pop)
   :map ivy-minibuffer-map
   ("M-y" . ivy-next-line)))

(use-package swiper
  ;;    :pin melpa-stable
  :diminish ivy-mode

  :bind*
  (("C-s" . swiper)
   ("C-c C-r" . ivy-resume)
   ("C-x C-f" . counsel-find-file)
   ("C-c h f" . counsel-describe-function)
   ("C-c h v" . counsel-describe-variable)
   ("C-c i u" . counsel-unicode-char)
   ("M-i" . counsel-imenu)
   ("C-c g" . counsel-git)
   ("C-c j" . counsel-git-grep)
   ("C-c k" . counsel-rg)
   ("C-x b" . counsel-switch-buffer)

   )
  :custom
  (ivy-use-virtual-buffers t)
  (counsel-switch-buffer-preview-virtual-buffers nil)
  :config
  (progn
    (ivy-mode 1)

    (define-key read-expression-map (kbd "C-r") #'counsel-expression-history)
    (ivy-set-actions
     'counsel-find-file
     '(("d" (lambda (x) (delete-file (expand-file-name x)))
        "delete"
        )))
    (ivy-set-actions
     'ivy-switch-buffer
     '(("k"
        (lambda (x)
          (kill-buffer x)
          (ivy--reset-state ivy-last))
        "kill")
       ("j"
        ivy--switch-buffer-other-window-action
        "other window")))))

(use-package counsel-projectile
  :init
  (counsel-projectile-mode))

;; for use in selections, press C-o and select multiple
(use-package ivy-hydra )

(use-package ibuffer
  :ensure nil
  :bind ("C-x C-b" . ibuffer)
  :init (setq ibuffer-filter-group-name-face '(:inherit (font-lock-string-face bold)))
  :config
  ;; Display icons for buffers
  (use-package all-the-icons-ibuffer
    :init
    ;;(setq all-the-icons-ibuffer-icon centaur-icon)
    (all-the-icons-ibuffer-mode 1))

  (with-eval-after-load 'counsel
    (with-no-warnings
      (defun my-ibuffer-find-file ()
        (interactive)
        (let ((default-directory (let ((buf (ibuffer-current-buffer)))
                                   (if (buffer-live-p buf)
                                       (with-current-buffer buf
                                         default-directory)
                                     default-directory))))
          (counsel-find-file default-directory)))
      (advice-add #'ibuffer-find-file :override #'my-ibuffer-find-file))))

;; Group ibuffer's list by project root
(use-package ibuffer-projectile
  :functions all-the-icons-octicon ibuffer-do-sort-by-alphabetic
  :hook ((ibuffer . (lambda ()
                      (ibuffer-projectile-set-filter-groups)
                      (unless (eq ibuffer-sorting-mode 'alphabetic)
                        (ibuffer-do-sort-by-alphabetic)))))
  :config
  (setq ibuffer-projectile-prefix
        (concat
         (all-the-icons-octicon "file-directory"
                                :face ibuffer-filter-group-name-face
                                :v-adjust 0.0
                                :height 1.0)
         " ")
          ))

;; Show the version control status of the buffer
(use-package ibuffer-vc
  :config
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-vc-set-filter-groups-by-vc-root)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))

(use-package yaml-mode)

(use-package gn-mode
  :mode ("BUILD.gn" "\\.gni\\'"))

(use-package vterm)
(use-package multi-vterm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Minibuffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Enhanced M-x
(use-package counsel
  :ensure t
  :bind
  (("M-x" . counsel-M-x)))

;; IDO
(use-package ido
  :init
  (setq ido-everywhere nil
        ido-enable-flex-matching t
        ido-create-new-buffer 'always
        ido-file-extensions-order '(".java" ".js" ".el" ".xml")
        ido-use-filename-at-point 'guess
        ido-use-faces t)

  :config
  (ido-mode 'buffer)

  :bind ("C-x b" . ido-switch-buffer)
  )

;; Improved flex matching
(use-package flx-ido)

;; Vertical completion menu
(use-package ido-vertical-mode
  :init
  (setq ido-vertical-indicator ">>"
        ido-vertical-show-count nil
        ido-vertical-define-keys 'C-n-C-p-up-and-down)
  :config
  (ido-vertical-mode)
  (ido-vertical-mode nil))

;; If not using ido-vertical-mode, make the minibuff stay still,
;; i.e. never change height, set this to nil.
;; (setq resize-mini-windows 'grow-only)

;; IDO support pretty much everwhere, including eclim-java-implement
(use-package ido-completing-read+
  :config
  (ido-ubiquitous-mode))

;; Sub word support
(add-hook 'minibuffer-setup-hook 'subword-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun spawn-shell (name)
  "Create a new shell buffer
taken from http://stackoverflow.com/a/4116113/446256"

  (interactive "MName of shell buffer to create: ")
  (pop-to-buffer (get-buffer-create (generate-new-buffer-name name)))
  (shell (current-buffer)))

(defun my-shell-mode-hook ()
  (process-send-string (get-buffer-process (current-buffer))
                       "export PAGER=cat\n")
  (process-send-string (get-buffer-process (current-buffer))
                       "uprompt\n\n\n"))(
  add-hook 'shell-mode-hook 'my-shell-mode-hook)

(setq-default explicit-shell-file-name "/bin/zsh")

(use-package multi-line)
(global-set-key (kbd "C-;") 'multi-line)

(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02))

(use-package git-gutter-fringe
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))

(global-set-key (kbd "<backspace>")
                '(lambda () (interactive) (backward-delete-char-untabify 1 nil)))

(require 'hi-lock)
(defun jpt-toggle-mark-word-at-point ()
  (interactive)
  (if hi-lock-interactive-patterns
      (unhighlight-regexp (car (car hi-lock-interactive-patterns)))
    (highlight-symbol-at-point)))

(provide 'config-packages)
