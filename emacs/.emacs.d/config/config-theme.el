;; Disable a bunch of bars

(use-package vscode-dark-plus-theme
  :ensure t
  :config
  (load-theme 'vscode-dark-plus t))

(when (functionp 'menu-bar-mode)
  (menu-bar-mode -1))
(when (functionp 'set-scroll-bar-mode)
  (set-scroll-bar-mode 'nil))
(when (functionp 'tooltip-mode)
  (tooltip-mode -1))
(when (functionp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (functionp 'blink-cursor-mode)
  (blink-cursor-mode -1))

(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

(when (member "Inconsolata Nerd Font" (font-family-list))
  (set-frame-font "Inconsolata Nerd Font-10" t))

;; Fancy mode line
(use-package smart-mode-line
  :demand t
  :config
  (progn
    (setq sml/no-confirm-load-theme t)
    (setq sml/theme 'respectful)
    (sml/setup)
    (set-face-attribute 'mode-line nil
                        :underline nil)
    (set-face-attribute 'mode-line-inactive nil
                        :underline nil
                        :overline nil)
    (set-face-attribute 'header-line nil
                        :box nil
                        :underline nil)))

(set-cursor-color "#b60900")

(provide 'config-theme)
