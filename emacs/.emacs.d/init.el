(require 'cl-lib)

(setq package-native-compile t)

(add-to-list 'load-path (expand-file-name "config" "~/.emacs.d"))
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

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

(require 'config-packages)
(require 'config-prelude)
(require 'config-theme)
(require 'config-misc)
(require 'soong-mode)

;; enable mouse
(require 'mouse)
(xterm-mouse-mode 1)
(mouse-wheel-mode 1)

; (require 'config-eshell)
