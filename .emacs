(setq-default c-basic-offset 8)
    (setq c-default-style "k&r"
        c-basic-offset 8)
(setq-default c-basic-offset 8
        tab-width 8
        indent-tabs-mode t)

(require 'package)
  (push '("marmalade" . "http://marmalade-repo.org/packages/")
        package-archives )
  (push '("melpa" . "http://melpa.milkbox.net/packages/")
        package-archives)
  (package-initialize)

(add-hook 'c-mode-common-hook '(lambda () (c-toggle-auto-state 1)))

(require 'evil)
(evil-mode 1)


;; Enable mouse support
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (global-set-key [mouse-4] (lambda ()
			      (interactive)
			      (scroll-down 1)))
  (global-set-key [mouse-5] (lambda ()
			      (interactive)
			      (scroll-up 1)))
  (defun track-mouse (e))
  (setq mouse-sel-mode t)
)
