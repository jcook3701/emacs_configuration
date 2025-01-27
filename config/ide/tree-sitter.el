;;; tree-sitter.el --- Summary
;;; Commentary:
;; Emacs tree-sitter package configurations
;;----------------------------------------------------------------------------------------------
;;; Code:

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode)
  :ensure t)

;;; tree-sitter.el ends here
