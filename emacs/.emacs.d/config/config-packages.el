;; Set up package management, adding various sources to search
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

(use-package rg)
(use-package helm-rg)

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
  (add-hook 'after-save-hook #'gtags-update-hook)
)

(use-package helm-gtags)

(use-package bm)

(global-set-key (kbd "<C-f2>") 'bm-toggle)
(global-set-key (kbd "<f2>")   'bm-next)
(global-set-key (kbd "<S-f2>") 'bm-previous)

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

(with-eval-after-load 'lsp
        (add-hook 'c++-mode-hook 'lsp)
        (add-hook 'c-mode-hook 'lsp)
)

(add-hook 'c-mode-hook
      (lambda ()
        (setq c-indent-offset 8)
        (setq indent-tabs-mode nil)
        (setq c-indent-level 8)
        (setq tab-width 8)))

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
      )
      )

(use-package lsp-ui
  :after lsp
  :ensure t
  :config
  (require 'lsp-ui)
  (setq lsp-ui-doc-position 'at-point
         lsp-ui-doc-alignment 'window
         lsp-ui-doc-include-signature nil
         lsp-ui-sideline-show-symbol nil)
  )

(with-eval-after-load 'lsp-ui
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)

  )


(use-package evil-nerd-commenter)
;; evil
(use-package evil
  :init
  (setq evil-want-integration nil)
  (setq evil-want-keybinding nil)
  (progn
    (use-package evil-leader
      :init (global-evil-leader-mode)
      :config
      (progn
        (evil-leader/set-leader ",")
        (setq evil-leader/in-all-states t)
        (evil-leader/set-key
          "c" 'evilnc-comment-or-uncomment-lines
          "b" 'helm-buffers-list
          "f" 'helm-find-files
          "g" 'magit-status
          "x" 'helm-M-x
          "e" 'helm-projectile
          "m" 'helm-mini
          "s" 'helm-projectile-rg
          "z" 'previous-buffer
          "d" 'cd
          "k" 'kill-buffer
          "v" 'split-window-below
          "h" 'split-window-right
          "w" 'other-window
          "t" 'multi-term
          "n" 'multi-term-next
          "p" 'multi-term-prev
          "r" 'term-char-mode
          "a" 'org-agenda))))
    (evil-mode t))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(define-key evil-normal-state-map (kbd "C-]") 'xref-find-definitions)
(define-key evil-normal-state-map (kbd "C-r") 'xref-find-references)
(define-key evil-normal-state-map (kbd "C-\\") 'helm-gtags-find-tag)
(define-key evil-normal-state-map (kbd "<f1>") 'helm-gtags-pop-stack)
(define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop)
(define-key evil-normal-state-map (kbd "C-+") 'text-scale-increase)
(define-key evil-normal-state-map (kbd "C--") 'text-scale-decrease)

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

;; Projectile for large project utilities
(use-package projectile
  :demand t
  :diminish projectile-mode
  :config
  (progn
    (projectile-global-mode t)
    (setq projectile-completion-system 'helm)
    (setq projectile-mode-line " Projectile")
    (setq projectile-enable-caching t)))

(use-package helm-projectile
  :bind (("C-x f" . helm-projectile-find-file)
         ("C-x g" . helm-projectile-grep))
  :config (helm-projectile-on))

(use-package swiper-helm
  :bind (("C-s" . my/isearch-forward)
         ("C-r " . my/isearch-backward))
  :config
  (setq swiper-helm-display-function 'display-buffer)
  (defun my/isearch-forward (&optional regexp-p no-recursive-edit)
    (interactive "P\np")
    (cond ((equal current-prefix-arg nil)
           (if (minibufferp)
               (isearch-forward)
             (swiper-helm)))
          ((equal current-prefix-arg '(4)) (isearch-forward-regexp))
          (t (isearch-forward))))
  (defun my/isearch-backward (&optional regexp-p no-recursive-edit)
    (interactive "P\np")
    (cond ((equal current-prefix-arg nil)
           (if (minibufferp)
               (isearch-backward)
             (swiper-helm)))
          ((equal current-prefix-arg '(4)) (isearch-backward-regexp))
          (t (isearch-backward)))))


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
    (add-hook 'prog-mode-hook 'whitespace-mode)))

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
             (c-set-style "linux")
             (setq c-basic-offset 8)
             (setq tab-width c-basic-offset)
             (setq indent-tabs-mode nil)
             (setq comment-start "/* ")
             (setq comment-end "*/")
             (define-key c-mode-base-map (kbd "C-c c")   'compile)
             (define-key c-mode-base-map (kbd "C-c C-c") 'quickrun)
             ))
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
  :commands fzf-find-file
  :ensure t)

(global-set-key (kbd "C-c p p") 'fzf-find-file)


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
  (progn
    (ad-activate 'term-send-return)
    (ad-activate 'term-send-input)
    )
  :commands term
  :config
  (setq explicit-shell-file-name "zsh") ;; Change this to zsh, etc
  ;;(setq explicit-zsh-args '())         ;; Use 'explicit-<shell>-args for shell-specific args

  ;; Match the default Bash shell prompt.  Update this if you have a custom prompt
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

(add-hook 'term-mode-hook (lambda () (read-only-mode -1)))

(use-package multi-term
  :commands multi-term
  )

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
    (setq elpy-rpc-python-command "python")
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

(use-package neotree
  :config
  (setq neo-theme(if (display-graphic-p) 'icons 'arrow))
  :init
  (progn
    (global-set-key [f8] 'neotree-toggle))
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



(provide 'config-packages)
