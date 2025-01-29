;;; flymake.el --- Summary
;;; Commentary:
;; Emacs flymake packages configurations
;;----------------------------------------------------------------------------------------------
;;; Code:

;; Link: https://github.com/flymake/emacs-flymake
(use-package flymake
  :custom
  (flymake-number-of-errors-to-display nil) ;; I want to see all errors for the line.
  :ensure t)

;; An Emacs flymake handler for syntax-checking CSS source code using csslint.
;;
;; Link: https://github.com/purcell/flymake-css
(use-package flymake-css
  :after flymake
  :hook ((css-ts-mode css-mode) . flymake-css-load)
  :ensure t)

;; An Emacs flymake handler for syntax-checking JSON using jsonlint.
;;
;; Link: https://github.com/purcell/flymake-json
(use-package flymake-json
  :after flymake
  :hook ((json-ts-mode json-mode) . flymake-json-load)
  :ensure t)

;; An Emacs flymake handler for syntax-checking assemlby source code using nasm.
;;
;; Link: https://github.com/juergenhoetzel/flymake-nasm
(use-package flymake-nasm
  :after flymake
  :hook (asm-mode . flymake-nasm-setup)
  :ensure t)

;; An Emacs flymake handler for syntax-checking Ruby source code.
;;
;; Link: https://github.com/purcell/flymake-ruby
(use-package flymake-ruby
  :hook ((ruby-ts-mode ruby-mode) . flymake-ruby-load)
  :ensure t)

;; An Emacs flymake handler for syntax-checking SASS source code.
;;
;; Link: https://github.com/purcell/flymake-sass
(use-package flymake-sass
  :after flymake
  :hook (sass-mode . flymake-sass-load)
  :ensure t)

;; Flymake backend for Javascript using eslint
;;
;; Link: https://github.com/orzechowskid/flymake-eslint
(use-package flymake-eslint
  :after flymake
  ;;   :init
  ;;  (add-hook 'web-mode-hook ; or whatever the mode-hook is for your mode of choice
  ;;	    (lambda ()
  ;;	      (flymake-eslint-enable)))
  :ensure t)

;; This package provides support for running any flycheck checker as a flymake
;; diagnostic backend. The effect is that flymake will control when the checker
;; runs, and flymake will receive its errors.
;;
;; Link: https://github.com/purcell/flymake-flycheck
(use-package flymake-flycheck
  :after (flymake flycheck)
  :hook (flymake-mode . flymake-flycheck-auto)
  :ensure t)

;; A Flymake backend for validating YAML files for Emacs (26+), using yamllint
;;
;; Link: https://codeberg.org/shaohme/flymake-yamllint
;; NOTE: Swapping to flycheck yamllint
;;;(use-package flymake-yamllint
;;;  :after flymake
;;;  :hook ((yaml-ts-mode yaml-mode)  . flymake-yamllint-setup)
;;;
;;;:ensure t)

;; The 'flymake-ansible-lint' package provides a Flymake backend for ansible-lint,
;; enabling real-time syntax and style checking for Ansible playbooks and roles within Emacs.
;;
;; Link: https://github.com/jamescherti/flymake-ansible-lint.el
(use-package flymake-ansible-lint
  :after flymake
  :commands flymake-ansible-lint-setup
  :hook (((yaml-ts-mode yaml-mode) . flymake-ansible-lint-setup)
         ((yaml-ts-mode yaml-mode) . flymake-mode))
  :ensure t)

;; A Flymake backend for validating Markdown files for Emacs (27+), using markdownlint-cli
;;
;; Link: https://codeberg.org/shaohme/flymake-markdownlint
(use-package flymake-markdownlint
  :after flymake
  :hook ((markdown-mode gfm-mode) . flymake-markdownlint-setup)
  :ensure t)

;; Default load-path for flymake on Emacs Lisp mode can be set through
;; elisp-flymake-byte-compile-load-path, but it is just a global variable.
;; When you are editing init.el, flymake should use all the load-path.
;; When you update some packages, load-path also should be updated.
;; When you are editing your package, flymake should use paths provided
;; by cask or keg.
;;
;; Link: https://github.com/ROCKTAKEY/flymake-elisp-config
(use-package flymake-elisp-config
  :after flymake
  :config
  (flymake-elisp-config-global-mode)  ; Enable Flymake globally for Emacs Lisp
  (flymake-elisp-config-auto-mode)    ; Automatically manage `load-path`
  :ensure t)

;; An Emacs flymake handler for syntax-checking Python source code using pyflakes or flake8.
;;
;; Link: https://github.com/purcell/flymake-python-pyflakes
(use-package flymake-python-pyflakes
  :after flymake
  :custom
  (flymake-python-pyflakes-executable "flake8")
  :hook ((python-ts-mode python-mode) . flymake-python-pyflakes-load)
  :ensure t)

;; I think this is now part of package-lint
;; (use-package package-lint-flymake
;;   :after flymake
;;  :ensure t)

;;; flymake.el ends here
