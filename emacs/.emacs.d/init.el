(require 'cl-lib)

(setq package-native-compile t)

(add-to-list 'load-path (expand-file-name "config" "~/.emacs.d"))

(require 'config-prelude)
(require 'config-theme)
(require 'config-packages)
(require 'config-misc)
; (require 'config-eshell)
