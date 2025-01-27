;;; yaml.el --- Summary
;;; Commentary:
;; Emacs yaml packages configurations
;;----------------------------------------------------------------------------------------------
;;; Code:

;; Note: The hook to ansible-mode might need to be removed when working on non-ansible projects
;;       This was needed to enable ls-ansible within lsp.  Otherwise only yamlls server starts.
(use-package yaml
  :hook ((yaml-ts-mode yaml-mode) . ansible-mode)
  :mode ("\\.yml\\'" . yaml-mode)
  :interpreter ("yaml" . yaml-mode)
  :config
  (setq yaml-indent-offset 2) ;; Set the indentation width to 2 spaces
  :ensure t)

;; Link
;;(use-package yaml-pro
;;  :config
;;  (setq max-lisp-eval-depth 2000)
;;  (setq max-specpdl-size 2000)
;;  :ensure t)

;;; yaml.el ends here
