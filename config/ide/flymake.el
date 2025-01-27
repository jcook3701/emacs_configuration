;;; flymake.el --- Summary
;;; Commentary:
;; Emacs flymake packages configurations
;;----------------------------------------------------------------------------------------------
;;; Code:

;; Link: https://github.com/flymake/emacs-flymake
(use-package flymake
  :config
  ;; I want to see all errors for the line.
  (setq flymake-number-of-errors-to-display nil)
  :ensure t)

;; Link: https://github.com/purcell/flymake-flycheck
(use-package flymake-flycheck
  :after (flymake flycheck)
  :hook (flymake-mode . flymake-flycheck-auto)
  :ensure t)

;; Link: https://github.com/jamescherti/flymake-ansible-lint.el
(use-package flymake-ansible-lint
  :after flymake
  :commands flymake-ansible-lint-setup
  :hook (((yaml-ts-mode yaml-mode) . flymake-ansible-lint-setup)
         ((yaml-ts-mode yaml-mode) . flymake-mode))
  :ensure t)

;; Link: https://codeberg.org/shaohme/flymake-markdownlint
(use-package flymake-markdownlint
  :after flymake
  :hook ((markdown-mode gfm-mode) . flymake-markdownlint-setup)
  :ensure t)

;; Link: https://github.com/orzechowskid/flymake-eslint
(use-package flymake-eslint
  :after flymake
  ;;   :init
  ;;  (add-hook 'web-mode-hook ; or whatever the mode-hook is for your mode of choice
  ;;	    (lambda ()
  ;;	      (flymake-eslint-enable)))
  :ensure t)

;; Link: https://codeberg.org/shaohme/flymake-yamllint
(use-package flymake-yamllint
  :after flymake
  :hook ((yaml-ts-mode yaml-mode)  . flymake-yamllint-setup)
  :ensure t)

;;; flymake.el ends here
