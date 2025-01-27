;;; gcode.el --- Summary
;;; Commentary:
;; Emacs gcode packages configurations
;;----------------------------------------------------------------------------------------------
;;; Code:

;; `gcode-mode' performs basic syntax highlighting on G-Code files
;; (mostly aimed at 3D printers), also providing optional instruction
;; lookup with ElDoc.
;;
;; Link: https://gitlab.com/wavexx/gcode-mode.el/tree/1f83845af4102efc5e5856b55bd5ad165b2f0cdd
(use-package gcode-mode
  :commands (gcode-mode)
  :mode ("\\.gcode\\'" . gcode-mode)
  :hook((gcode-mode . eldoc-mode)
	(gcode-mode . lsp-deferred))
  :ensure t)

;;; gcode.el ends here
