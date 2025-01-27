;;; debug.el --- Summary
;;; Commentary:
;; Emacs debug packages configuration
;;----------------------------------------------------------------------------------------------
;;; Code:

;;
;; Link: https://github.com/emacs-lsp/dap-mode
(use-package dap-mode
  :hook
  ((prog-mode . dap-mode)       ;; Enable dap-mode for programming modes
   (dap-mode . dap-ui-mode))    ;; Enable the UI for debugging
  :config
  ;; Optional: Configure features for better debugging experience
  ;; (dap-auto-configure-mode)      ;; Automatically configures the buffer
  ;; (setq dap-auto-configure-features '(sessions locals breakpoints expressions repl)) ;; Features to auto-configure
  
  ;; Language-specific configurations
  (require 'dap-python)         ;; Python support
  (require 'dap-node)           ;; Node.js support
  (dap-node-setup)
  (require 'dap-lldb)          ;; C++/Rust with LLDB
  :ensure t)

;; Optional: Treemacs integration for visual debugging (if you use Treemacs)
(use-package dap-ui
  :ensure nil
  :after (treemacs dap-mode)
  :config
  (dap-ui-mode 1)
  ;;(dap-ui-breakpoints)
  )

;; Python Debugger Configuration
(use-package dap-python
  :ensure nil
  :after dap-mode
  :config
  (setq dap-python-debugger 'debugpy)  ;; Use `debugpy` as the Python debugger
  (setq dap-python-executable "python3"))

;; Node.js Debugger Configuration
(use-package dap-node
  :ensure nil
  :after dap-mode
  :config
  (setq dap-node-debug-program `("node" "--inspect-brk"))) ;; Ensure Node.js is installed

;;; debug.el ends here
