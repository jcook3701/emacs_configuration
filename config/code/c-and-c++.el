;;; c-and-c++.el --- Summary
;;; Commentary:
;; Emacs c and c++ packages configurations
;;----------------------------------------------------------------------------------------------
;;; Code:

;; Company Irony
;; irony-mode is an Emacs minor-mode that aims at improving the editing
;; experience for the C, C++ and Objective-C languages. It works by using
;; a combination of an Emacs package and a C++ program (irony-server)
;; exposing libclang.
;;
;; Link: https://github.com/Sarcasm/irony-mode
(use-package irony
  :hook (((c-ts-mode c-mode) . irony-mode)
	 ((c++-ts-mode c++-mode) . irony-mode)
	 (objc-mode . irony-mode))
  :init
  ;; C++ Mode Settings
  (setq-default c-basic-offset 4)

  (defun my-irony-mode ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))
  (add-hook 'irony-mode-hook 'my-irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
  
  :ensure t)

;; This implements eldoc support in irony-mode. eldoc is a built-in Emacs
;; mode for displaying documentation about a symbol or function call at
;; point in the message buffer (see eldoc-mode).
;;
;; Link: https://github.com/ikirill/irony-eldoc
(use-package irony-eldoc
  :after (irony)
  :hook (irony-mode . irony-eldoc)
  :ensure t)

;; 
;; 
;; Link: https://github.com/Andersbakken/rtags
(use-package rtags
  :hook (c-mode-common . rtags-start-process-unless-running)
  :init
  (setq rtags-autostart-diagnostics t)
  :config
  (rtags-enable-standard-keybindings)
  :bind
  (("M-." . rtags-find-symbol-at-point)
   ("M-," . rtags-find-references-at-point))
  :ensure t)

;; Code completion
;;
;; Link: https://github.com/Andersbakken/rtags
(use-package company-rtags
  :after (company rtags)
  :config
  (setq rtags-autostart-diagnostics t)   ;; Enable autostart for diagnostics
  (rtags-diagnostics)                    ;; Start RTags diagnostics
  (setq rtags-completions-enabled t)     ;; Enable RTags completions
  (push 'company-rtags company-backends) ;; Add company-rtags as a backend for company-mode
  :bind
  (("M-." . company-rtags-find-symbol-at-point)  ;; Go to symbol
   ("M-," . company-rtags-find-references-at-point))  ;; Find references
  :ensure t)

;; GGTAGS setup
(use-package ggtags
  :hook
  (((c-ts-mode c-mode) . ggtags-mode)
   ((c++-ts-mode c++-mode) . ggtags-mode))
  :config
  (setq ggtags-completion-mode 'helm) ;; Using Helm for ggtags completion
  :bind
  (("C-c g" . ggtags-find-tag-dwim))
  :ensure t)

;; Provides syntax highlighting and indentation for CMakeLists.txt and
;; *.cmake source files.
;;
;; Link: https://github.com/Kitware/CMake/blob/master/Auxiliary/cmake-mode.el
(use-package cmake-mode
  :mode(("CMakeLists\\.txt\\'" . cmake-mode)
	("\\.cmake\\'" . cmake-mode))
  :interpreter ("cmake" . cmake-mode)
  :hook ((cmake-ts-mode cmake-mode) . lsp-deferred)
  :ensure t)


;;;cmake-ide
;;;(setq cmake-ide-build-dir "~/.emacs.d/cmake-ide-build-dir")
;;;(setq cmake-ide-build-pool-dir "~/.emacs.d/cmake-ide-build-dir")
;;;(setq cmake-ide-build-pool-use-persistent-naming t)
;;;(require 'rtags)
;;;(require 'company)
;;;(cmake-ide-setup)

;;; Rtags Package
;;;(add-hook 'c-mode-hook 'rtags-start-process-unless-running)
;;;(add-hook 'c++-mode-hook 'rtags-start-process-unless-running)
;;;(add-hook 'objc-mode-hook 'rtags-start-process-unless-running)
;;;(setq company-backends '(company-rtags))

;;; c-and-c++.el ends here
