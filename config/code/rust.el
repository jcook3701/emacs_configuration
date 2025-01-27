;;; rust.el --- Summary
;;; Commentary:
;; Emacs rust packages configurations
;;----------------------------------------------------------------------------------------------
;;; Code:

;; rust-mode makes editing Rust code with Emacs enjoyable. It requires Emacs 25
;; or later, and is included in both Emacs Prelude and Spacemacs by default.
;;
;; Link: https://github.com/rust-lang/rust-mode
(use-package rust-mode
  :commands (rust-ts-mode rust-mode)
  :hook ((rust-ts-mode rust-mode) . lsp-deferred)
  :mode ("\\.rs\\'" . rust-mode)
  :interpreter ("rust" . rust-mode)
  :ensure t)

;;; rust.el ends here
