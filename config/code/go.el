;;; go.el --- Summary
;;; Commentary:
;; Emacs go packages configurations
;;----------------------------------------------------------------------------------------------
;;; Code:

;; go-mode covers the basic features you need for working with the Go code
;; but adds some extended things like imports managing or interacting with
;; play.golang.org. You may also find useful to add the additional features noted below.
;;
;; Link: https://github.com/dominikh/go-mode.el
(use-package go-mode
  :commands (go-mode)
  :mode ("\\.go\\'" . go-mode)
  :hook ((go-ts-mode go-mode) . lsp-deferred)
  :ensure t)

;;; go.el ends here
