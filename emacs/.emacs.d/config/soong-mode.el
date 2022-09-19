;;; soong-mode.el --- Emacs major mode for Android Soong Build system


;;; Commentary:
;;

;;; Code:

(defconst soong-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; " is a string delimiter too
    (modify-syntax-entry ?\" "\"" table)
    ;; // is a comment
    (modify-syntax-entry ?/ "<2" table)
    ;; \n is a comment ender
    (modify-syntax-entry ?\n ">" table)
    table))

(define-derived-mode soong-mode prog-mode "Soong build system mode"
  :syntax-table soong-mode-syntax-table
  (font-lock-fontify-buffer))

;; keywords
(font-lock-add-keywords 'soong-mode
                        '(("cc_benchmark_host" . font-lock-keyword-face)
                          ("cc_benchmark" . font-lock-keyword-face)
                          ("cc_binary_host" . font-lock-keyword-face)
                          ("cc_binary" . font-lock-keyword-face)
                          ("cc_defaults" . font-lock-keyword-face)
                          ("cc_genrule" . font-lock-keyword-face)
                          ("cc_library_headers" . font-lock-keyword-face)
                          ("cc_library_host_shared" . font-lock-keyword-face)
                          ("cc_library_host_static" . font-lock-keyword-face)
                          ("cc_library_shared" . font-lock-keyword-face)
                          ("cc_library_static" . font-lock-keyword-face)
                          ("cc_library" . font-lock-keyword-face)
                          ("cc_object" . font-lock-keyword-face)
                          ("cc_prebuilt_binary" . font-lock-keyword-face)
                          ("cc_prebuilt_library_shared" . font-lock-keyword-face)
                          ("cc_prebuilt_library_static" . font-lock-keyword-face)
                          ("cc_test" . font-lock-keyword-face)
                          ("cc_test_host" . font-lock-keyword-face)
                          ("cc_test_library" . font-lock-keyword-face)))

;; constants
;; TODO: add more here
(font-lock-add-keywords 'soong-mode
                        '(("true" . font-lock-constant-face)
                          ("false" . font-lock-constant-face)))

(add-to-list 'auto-mode-alist '("\\.bp\\'" . soong-mode))

(provide 'soong-mode)
