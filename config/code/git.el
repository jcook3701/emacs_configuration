;;; git.el --- Summary
;;; Commentary:
;; Emacs git packages configurations
;;----------------------------------------------------------------------------------------------
;;; Code:

;; Emacs major modes for various Git configuration files.
;;
;; Link: https://github.com/magit/git-modes
(use-package git-modes
  :mode
  (("\\.gitignore\\'" . gitignore-mode)          ;; Use gitignore-mode for .gitignore files
   ("\\.gitattributes\\'" . gitattributes-mode)  ;; Use gitattributes-mode for .gitattributes files
   ("\\.gitconfig\\'" . gitconfig-mode)          ;; Use gitconfig-mode for .gitconfig files
   ("config\\'" . gitconfig-mode)                ;; For files named 'config' in Git directories
   ("/\\.gitmodules\\'" . gitconfig-mode))       ;; Use gitconfig-mode for .gitmodules files
  :ensure t)

;;; git.el ends here
