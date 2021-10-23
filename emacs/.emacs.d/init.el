(require 'cl-lib)

(add-to-list 'load-path (expand-file-name "config" "~/.emacs.d"))

(require 'config-prelude)
(require 'config-packages)
(require 'config-misc)
; (require 'config-eshell)
(require 'config-theme)
