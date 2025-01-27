;;; tree-sitter.el --- Summary
;;; Commentary:
;; Emacs tree-sitter package configurations
;;----------------------------------------------------------------------------------------------
;;; Code:

;; This is a convenient language bundle for the Emacs package tree-sitter.
;; It serves as an interim distribution mechanism, until tree-sitter is
;; widespread enough for language-specific major modes to incorporate its
;; functionalities.
;;
;; Link: https://github.com/emacs-tree-sitter/tree-sitter-langs
(use-package tree-sitter-langs
  :ensure t)

;; Automatically install and use tree-sitter major modes in Emacs 29+.
;; If the tree-sitter version can’t be used, fall back to the original
;; major mode.
;;
;; Link: https://github.com/renzmann/treesit-auto
(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode)
  :ensure t)

;;; tree-sitter.el ends here
