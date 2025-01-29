;;; tree-sitter.el --- Summary
;;; Commentary:
;; Emacs tree-sitter package configurations
;;----------------------------------------------------------------------------------------------
;;; Code:

;; tree-sitter is an Emacs binding for Tree-sitter, an incremental parsing
;; system. It aims to be the foundation for a new breed of Emacs packages
;; that understand code structurally.
;;
;; Link: https://www.masteringemacs.org/article/how-to-get-started-tree-sitter
(use-package treesit
  :ensure nil ;; 'treesit' is built-in
  :custom
  (treesit-debug t)
  :config
  (setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     ;; NOTE: at the moment yaml-mode is more developed than yaml-ts-mode
     ;; (yaml "https://github.com/ikatyang/tree-sitter-yaml")
     ))
  (message "Tree-sitter available: %s" (treesit-available-p)))

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
  ;; (treesit-auto-install 'prompt) -> TODO: Figure out how to get this to ignore yaml-mode.
  ;; Explicitly disable prompting for yaml-ts-mode
  (treesit-auto-recipe-list
   (assq-delete-all 'yaml-ts-mode treesit-auto-recipe-list))
  ;; Remove yaml-ts-mode from auto-mode-alist
  (treesit-language-source-alist
   (assq-delete-all 'yaml-ts-mode treesit-language-source-alist))
  :config
  ;; Ensure Tree-sitter is enabled for other modes (but not for YAML)
  (treesit-auto-add-to-auto-mode-alist 'all)

  ;; Enable global Tree-sitter auto-mode for supported languages, excluding YAML
  (global-treesit-auto-mode)
  :ensure t)

;;; tree-sitter.el ends here
