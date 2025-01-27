;;; eldoc.el --- Summary
;;; Commentary:
;; Emacs eldoc package configuration
;;----------------------------------------------------------------------------------------------
;;; Code:

;; A very simple but effective thing, eldoc-mode is a MinorMode which shows
;; you, in the echo area, the argument list of the function call you are currently
;; writing. Very handy. By NoahFriedman. Part of Emacs.
;; 
;; Link: https://www.emacswiki.org/emacs/ElDoc
(use-package eldoc
  :commands (eldoc-mode)
  :ensure t)

;;; eldoc.el ends here
