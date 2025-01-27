;;; web.el --- Summary
;;; Commentary:
;; Emacs web editing packages configurations
;;----------------------------------------------------------------------------------------------
;;; Code:

;;;(use-package multi-web-mode
;;;  :init
;;;  (setq mweb-default-major-mode 'html-mode)
;;;  (setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
;;;                    (web-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
;;;                    (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
;;;  
;;;  (setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
;;;  
;;;  :ensure t)

;; web-mode.el is an autonomous emacs major-mode for editing web templates.
;; HTML documents can embed parts (CSS / JavaScript) and blocks (client / server side).
;;
;; Link: https://github.com/fxbois/web-mode
(use-package web-mode
  :hook (web-mode . lsp-deferred)
  :mode
  (("\\.html?\\'" . web-mode)
   ("\\.css?\\'" . web-mode)
   ("\\.scss?\\'" . web-mode)
   ("\\.jsx\\'" . web-mode)
   ("\\.ts\\'" . web-mode)
   ("\\.tsx\\'" . web-mode)
   ("\\.launch\\'" . web-mode))
  :init
  
;;  (add-hook 'web-mode-hook	    
;;            (lambda ()
;;              (when (string-equal "jsx" (file-name-extension buffer-file-name))
;;		(setup-tide-mode))))

;;  (add-hook 'web-mode-hook
;;            (lambda ()
;;              (when (string-equal "tsx" (file-name-extension buffer-file-name))
  ;;		(setup-tide-mode))))

  ;; (add-hook 'tsx-ts-mode-hook #'setup-tide-mode)


  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-enable-current-element-highlight t)

  (setq web-mode-attr-indent-offset 2)       ;; html - Fixed indentation
  (setq web-mode-attr-value-indent-offset 2) ;; 
  (setq web-mode-markup-indent-offset 2)     ;; html
  (setq web-mode-css-indent-offset 2)        ;; css
  (setq web-mode-code-indent-offset 2)       ;; js/jsx
  (setq web-mode-sql-indent-offset 2)        ;; sql
  (setq web-mode-indent-style 2)

  ;; css color codes should show as the colors that they represent in emacs
  (setq web-mode-enable-css-colorization t)

  (setq web-mode-enable-block-face t)
  (setq web-mode-enable-part-face t)  
  :ensure t)

;; Improved JavaScript editing mode for GNU Emacs. 
;;
;; Link: https://github.com/mooz/js2-mode
(use-package js2-mode
  :mode ("\\.js\\'" . js2-mode)
  :hook ((js2-mode . lsp-deferred)
	 (js2-minor-mode . lsp-deferred))
  :bind (:map js2-mode-map
              ("M-r"        . node-js-eval-region-or-buffer)
              ("M-R"        . refresh-chrome)
              ("M-s-<up>"   . js2r-move-line-up)
              ("M-s-<down>" . js2r-move-line-down)
              ("C-<left>"   . js2r-forward-barf)
              ("C-<right>"  . js2r-forward-slurp)
              ("M-m S"      . js2r-split-string))
  :config
  ;; Formatting
  (setq js2-basic-offset 2)
  ;; Errors and Warnings
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil)
  :ensure t)

;; A JavaScript refactoring library for emacs.
;;
;; This is a collection of small refactoring functions to further
;; the idea of a JavaScript IDE in Emacs that started with js2-mode.
;;
;; Link: https://github.com/js-emacs/js2-refactor.el
(use-package js2-refactor
  :hook (js2-mode . js2-refactor-mode)
  :ensure t)

;; TypeScript Interactive Development Environment for Emacs
;;
;; Link: https://github.com/ananthakumaran/tide
(use-package tide
  :after (company flycheck)
  :hook (((typescript-ts-mode typescript-mode) . setup-tide-mode)
	 (js2-mode . setup-tide-mode)
	 (web-mode . setup-tide-mode)
         (tide-mode . flycheck-mode)
         (tide-mode . company-mode)
         (tide-mode . tide-hl-identifier-mode))
         ;;(before-save . tide-format-before-sve))
  :config
  (defun setup-tide-mode ()
    "Configure Tide mode for JavaScript/TypeScript."
    (interactive)
    (when (or (string-equal "tsx" (file-name-extension buffer-file-name))
              (string-equal "ts" (file-name-extension buffer-file-name))
	      (string-equal "jsx" (file-name-extension buffer-file-name))
              (string-equal "js" (file-name-extension buffer-file-name)))
      (tide-setup)
      (setq flycheck-check-syntax-automatically '(save mode-enabled))))

  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)
  ;; Disable Tide's formatting if using LSP's formatting
  (setq tide-format-options nil)
  :ensure t)

;; typescript-mode is a major-mode for editing Typescript-files in GNU Emacs.
;;
;; Link: https://github.com/emacs-typescript/typescript.el
(use-package typescript-mode
  :hook ((typescript-ts-mode typescript-mode) . lsp-deferred)
  :ensure t)

;; This file provides add-node-modules-path, which searches the current files parent
;; directories for the node_modules/.bin/ directory and adds it to the buffer local
;; exec-path. This allows Emacs to find project based installs of e.g. eslint.
;; 
;; Link: https://github.com/codesuki/add-node-modules-path
(use-package add-node-modules-path
  :hook
  ((j2s-mode . add-node-modules-path)
   (tide-mode . add-node-modules-path)
   (web-mode . add-node-modules-path))
  :ensure t)

;; Run Node.js REPL in Emacs
;; 
;; Link: https://github.com/abicky/nodejs-repl.el
(use-package nodejs-repl
  :hook ((js2-mode . nodejs-repl-minor-mode)
	 (tide-mode . nodejs-repl-minor-mode)
	 (web-mode . nodejs-repl-minor-mode))
  :config
  (defun nvm-which ()
    (let ((output (shell-command-to-string "source ~/.nvm/nvm.sh; nvm which")))
      (cadr (split-string output "[\n]+" t))))
  :ensure t)

;; Extends the builtin js-mode to add better syntax highlighting for JSON and some
;; nice editing keybindings.
;;
;; Link: https://github.com/joshwnj/json-mode
(use-package json-mode
  :hook ((json-ts-mode json-mode) . lsp-deferred)
  :config
  (setq js-indent-level 2)
  :ensure t)

;; Unmaintained: Last commit was 8 years ago
;; Link: https://github.com/antonj/scss-mode
;;(use-package scss-mode
;;  :config
;;  (setq css-indent-offset 2)        ;; css
;;  :ensure t)

;; Emacs client/library for Debug Adapter Protocol is a wire protocol for communication
;; between client and Debug Server. It’s similar to the LSP but provides integration with
;; debug server.

;; yarn-mode is a major mode designed to be used to look at yarn.lock
;; files generated by Facebook's yarn package manager
;;
;; Link: https://github.com/anachronic/yarn-mode
(use-package yarn-mode
  :ensure t)

;; This is npm-mode, an Emacs minor mode for working with NPM projects.
;;
;; Link: https://github.com/mojochao/npm-mode
(use-package npm-mode
  :ensure t)

;; prettier-js is a function that formats the current buffer using prettier.
;; The package also exports a minor mode that applies (prettier-js) on save.
;;
;; Link: https://github.com/prettier/prettier-emacs
(use-package prettier-js
  :hook ((js2-mode . prettier-js-mode)
	 (tide-mode . prettier-js-mode)
	 (web-mode . prettier-js-mode))
  :config
;;;  (setq prettier-js-args
;;;	'("--trailing-comma" "all"))
  :ensure t)

;; A major mode for editing nginx config files
;;
;; Link: https://github.com/ajc/nginx-mode
(use-package nginx-mode
  :commands nginx-mode
;;  :mode ("/nginx/sites-\\(?:available\\|enabled\\)/" . nginx-mode)
  :ensure t)

;; Django project management package with the goodies you would expect and
;; then some. The project buffer workings is pretty much inspired by the good
;; ol' magit-status buffer.
;;
;; Link: https://code.djangoproject.com/wiki/Emacs
(use-package django-mode
  :ensure t)

;;; web.el ends here
