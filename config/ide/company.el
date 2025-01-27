;;; company.el --- Summary
;;; Commentary:
;; Emacs company packages configurations
;;----------------------------------------------------------------------------------------------
;;; Code:

;; To Company-lsp users:
;; Company-lsp is no longer maintained and has been removed from MELPA.
;; Please migrate to company-capf.
;;
;; Company is a text and code completion framework for Emacs. The name
;; stands for "complete anything". It uses pluggable back-ends and
;; front-ends to retrieve and display completion candidates.
;;
;; Link: https://github.com/company-mode/company-mode?tab=readme-ov-file
(use-package company
  :hook ((scala-mode . company-mode)
	 ((yaml-ts-mode yaml-mode) . company-mode)
	 (after-init-hook . global-company-mode))
  :config
  (setq lsp-completion-provider :capf)
  ;; changed hot keys to scroll through elpy jedi configuration which uses
  ;; company under the hood.
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
   ;;  Already defined however I am putting this here for my reference.
   ;; (define-key company-active-map (kbd "C-d") ')  ;; display a temporary window with documentation.
   ;; (define-key company-active-map (kbd "C-w") ')  ;; will display a temporary window showing the
                                                  ;; source code of the completion to get some context. 
  (global-company-mode 1)
  :bind
  ("<C-tab>" . company-complete)
  :ensure t)

;; This package provides a company-mode asynchronous completion
;; backend for the C, C++ and Objective-C languages.
;;
;; Link: https://github.com/Sarcasm/company-irony
(use-package company-irony
  :config
  ;;(setq company-backends (delete 'company-semantic company-backends))
  (add-to-list 'company-backends 'company-irony)
  :after company
  :ensure t)

;; This package provides a company-mode backend for C/C++ header files
;; that works with irony-mode. This package is meant to be complementary
;; to company-irony by offering completion suggestions to header files.
;;
;; Link: https://github.com/hotpxl/company-irony-c-headers
(use-package company-irony-c-headers
  :config
  (add-to-list 'company-backends 'company-irony-c-headers)
  :after company
  :ensure t)

;;; company.el ends here
