;;; lsp.el --- Summary
;;; Commentary:
;; Emacs lsp packages configurations
;;----------------------------------------------------------------------------------------------
;;; Code:

;; Client for Language Server Protocol (v3.14). lsp-mode aims to provide IDE-like experience by
;; providing optional integration with the most popular Emacs packages like company, flycheck
;; and projectile
;; 
;; Link: https://github.com/emacs-lsp/lsp-mode
;; Helpful: https://emacs-lsp.github.io/lsp-mode/page/lsp-eslint/
(use-package lsp-mode
  :after company
  ;; Optional - enable lsp-mode automatically in scala files
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . lsp-lens-mode)
  :custom
  (lsp-enable-file-watchers t)
  (lsp-file-watch-threshold 6000)
  (lsp-prefer-flymake nil)
  (lsp-eslint-auto-fix-on-save t) ; Automatically fix errors on save
  (lsp-eslint-enable t)
  (lsp-file-watch-ignored-directories '(".emacs.d"))
  ;; ( lsp-enable-snippet nil)
  
  ;; :config
  ;; Uncomment following section if you would like to tune lsp-mode performance according to
  ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
  ;;       (setq gc-cons-threshold 100000000) ;; 100mb
  ;;       (setq read-process-output-max (* 1024 1024)) ;; 1mb
  ;;       (setq lsp-idle-delay 0.500)
  ;;       (setq lsp-log-io nil)
  ;;       (setq lsp-completion-provider :capf)

  ;; (lsp-register-custom-settings
  ;; '(("pyls.plugins.pyls_mypy.enabled" t t)
  ;; ("pyls.plugins.pyls_mypy.live_mode" nil t)
  ;; ("pyls.plugins.pyls_black.enabled" t t)
  ;; ("pyls.plugins.pyls_isort.enabled" t t)
  ;; ("pyls.plugins.rope_completion.enabled" t t)
  ;; ("pyls.plugins.yapf.enabled." t t)))
  :ensure t)

;; This package contains all the higher level UI modules of lsp-mode, like flycheck support and code lenses.
;; Enable nice rendering of documentation on hover
;; Warning: on some systems this package can reduce your emacs responsiveness significally.
;; (See: https://emacs-lsp.github.io/lsp-mode/page/performance/)
;; In that case you have to not only disable this but also remove from the packages since
;; lsp-mode can activate it automatically.
;;
;; Link: https://github.com/emacs-lsp/lsp-ui
(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-sideline-delay 0.5)
  (lsp-ui-doc-delay 5)
  (lsp-ui-sideline-ignore-duplicates t)
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-alignment 'frame)
  (lsp-ui-doc-header nil)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-use-childframe t)
  (lsp-ui-imenu-enable t)
  (lsp-ui-peek-enable t)
  (lsp-ui-doc-enable t)
  :ensure t)

;; Integration between lsp-mode and treemacs and implementation of
;;   treeview controls using treemacs as a tree renderer.
;;
;; Link: https://github.com/emacs-lsp/lsp-treemacs
(use-package lsp-treemacs
  :commands (lsp-treemacs-errors-list)
  :ensure t)

;; TODO: https://emacs-lsp.github.io/lsp-mode/tutorials/reactjs-tutorial/

;;; lsp.el ends here
