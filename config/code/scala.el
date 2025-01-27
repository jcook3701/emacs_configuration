;;; scala.el --- Summary
;;; Commentary:
;; Emacs scala packages configurations
;;----------------------------------------------------------------------------------------------
;;; Code:

;; ----------------------------------- Scala Setup ----------------------------------
;; The mode intends to provide basic emacs support for the Scala language, including:
;; * local indenting of code, comments and multi-line strings
;; * motion commands
;; * highlighting
;;
;; Link: https://github.com/hvesalai/emacs-scala-mode
(use-package scala-mode
  :interpreter
  ("scala" . scala-mode)
  :ensure t)

;; This mode provides basic functionality required for successfully interacting
;; with sbt inside emacs. The core functionality includes:
;; * interacting with sbt shell and scala console
;; * compiling code and navigating to errors
;;
;; Link: https://github.com/hvesalai/emacs-sbt-mode
(use-package sbt-mode
  :commands (sbt-start sbt-command)
  :config
  ;; WORKAROUND: allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false"))
  :ensure t)

;; Add metals backend for lsp-mode
;; Emacs Scala IDE using lsp-mode to connect to Metals.
;;
;; Link: https://github.com/emacs-lsp/lsp-metals
(use-package lsp-metals
  :hook (scala-mode . lsp)
  :ensure t)

;;; scala.el ends here
