;; Disable a bunch of bars

;; (use-package vscode-dark-plus-theme
;;  :ensure t
;;  :config
;;  (load-theme 'vscode-dark-plus t))

;; (use-package color-theme-modern
;;    :config
;;    (load-theme 'simple-1 t))

;; (use-package kaolin-themes
;;   :config
;;   (load-theme 'kaolin-galaxy t)
;;   (kaolin-treemacs-theme))

;; Fancy mode line
(use-package smart-mode-line
  :demand t
  :config
  (progn
    (setq sml/no-confirm-load-theme t)
    (setq sml/theme 'dark)
    (sml/setup)
    (set-face-attribute 'mode-line nil
                        :underline nil)
    (set-face-attribute 'mode-line-inactive nil
                        :underline nil
                        :overline nil)
    (set-face-attribute 'header-line nil
                        :box nil
                        :underline nil)))

(use-package powerline
  :ensure t
  :config
  (setq powerline-default-separator (if (display-graphic-p) 'arrow
                                      nil))
  (air--powerline-default-theme))

(use-package powerline-evil
  :ensure t)

;;; Code:
(defface my-pl-segment1-active
  '((t (:foreground "#000000" :background "#E1B61A")))
  "Powerline first segment active face.")
(defface my-pl-segment1-inactive
  '((t (:foreground "#FFFFFF" :background "#224488")))
  "Powerline first segment inactive face.")
(defface my-pl-segment2-active
  '((t (:foreground "#F5E39F" :background "#8A7119")))
  "Powerline second segment active face.")
(defface my-pl-segment2-inactive
  '((t (:foreground "#FFFFFF" :background "#224488")))
  "Powerline second segment inactive face.")
(defface my-pl-segment3-active
  '((t (:foreground "#FFFFFF" :background "#224488")))
  "Powerline third segment active face.")
(defface my-pl-segment3-inactive
  '((t (:foreground "#FFFFFF" :background "#224488")))
  "Powerline third segment inactive face.")

(defun air--powerline-default-theme ()
  "Set up my custom Powerline with Evil indicators."
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (seg1 (if active 'my-pl-segment1-active 'my-pl-segment1-inactive))
                          (seg2 (if active 'my-pl-segment2-active 'my-pl-segment2-inactive))
                          (seg3 (if active 'my-pl-segment3-active 'my-pl-segment3-inactive))
                          (separator-left (intern (format "powerline-%s-%s"
                                                          (powerline-current-separator)
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           (powerline-current-separator)
                                                           (cdr powerline-default-separator-dir))))
                          (lhs (list (let ((evil-face (powerline-evil-face)))
                                       (if evil-mode
                                           (powerline-raw (powerline-evil-tag) evil-face)
                                         ))
                                     (if evil-mode
                                         (funcall separator-left (powerline-evil-face) seg1))
                                     (powerline-buffer-id seg1 'l)
                                     (powerline-raw "[%*]" seg1 'l)
                                     (when (and (boundp 'which-func-mode) which-func-mode)
                                       (powerline-raw which-func-format seg1 'l))
                                     (powerline-raw " " seg1)
                                     (funcall separator-left seg1 seg2)
                                     (when (boundp 'erc-modified-channels-object)
                                       (powerline-raw erc-modified-channels-object seg2 'l))
                                     (powerline-major-mode seg2 'l)
                                     (powerline-process seg2)
                                     (powerline-minor-modes seg2 'l)
                                     (powerline-narrow seg2 'l)
                                     (powerline-raw " " seg2)
                                     (funcall separator-left seg2 seg3)
                                     (powerline-vc seg3 'r)
                                     (when (bound-and-true-p nyan-mode)
                                       (powerline-raw (list (nyan-create)) seg3 'l))))
                          (rhs (list (powerline-raw global-mode-string seg3 'r)
                                     (funcall separator-right seg3 seg2)
                                     (unless window-system
                                       (powerline-raw (char-to-string #xe0a1) seg2 'l))
                                     (powerline-raw "%4l" seg2 'l)
                                     (powerline-raw ":" seg2 'l)
                                     (powerline-raw "%3c" seg2 'r)
                                     (funcall separator-right seg2 seg1)
                                     (powerline-raw " " seg1)
                                     (powerline-raw "%6p" seg1 'r)
                                     (when powerline-display-hud
                                       (powerline-hud seg1 seg3)))))
                     (concat (powerline-render lhs)
                             (powerline-fill seg3 (powerline-width rhs))
                             (powerline-render rhs)))))))



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

;; (when (member "Iosevka Term SS03" (font-family-list))
;;   (set-frame-font "Iosevka Term SS03-11" t))

;; (use-package modus-themes
;;   :init
;;   ;; Add all your customizations prior to laoding the themes
;;   (setq modus-themes-italic-constructs t
;;      modus-themes-bold-constructs t
;;      ;;modus-themes-region '(bg-only no-extend)
;;      modus-themes-completions '(opinionated)
;;      modus-themes-paren-match '(bold intense underline)
;;      modus-themes-syntax '(alt-syntax yellow-comments)
;;      modus-themes-org-blocks 'gray-background
;;      modus-themes-headings
;;      '((1 . (rainbow 1.1))
;;        (2 . (rainbow 1))
;;        (3 . (rainbow bold 1.2))
;;        (t . (semilight 1)))
;;      modus-themes-scale-headings t
;;      modus-themes-mode-line '(moody accented 3d padded borderless))
  
;;   ;; Load the theme files before enabling a theme (else you get an error)
;;   (modus-themes-load-themes)
;;   :config
;;   ;; Load the theme of your choice:
;;   (modus-themes-load-vivendi) ;; OR (modus-themes-load-operandi)
;;   :bind ("<f5>" . modus-themes-toggle))

(set-cursor-color "#b60900")


(load-theme 'clarity t)


;; (setq term-default-bg-color "#000000")
;; M-x customize-face RET term RET

;; (setq-default mode-line-format nil)

(provide 'config-theme)
