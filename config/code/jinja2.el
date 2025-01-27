;;; jinja2.el --- Summary
;;; Commentary:
;; Emacs jinja2 packages configurations
;;----------------------------------------------------------------------------------------------
;;; Code:

;; Link: https://github.com/paradoxxxzero/jinja2-mode
(use-package jinja2-mode
  ;; :hook (jinja2-mode . lsp-deferred)
  :mode ("\\.j2\\'" . jinja2-mode)
  :ensure t)

;;; jinja2.el ends here
