;;; verilog.el --- Summary
;;; Commentary:
;; Emacs verilog packages configurations
;;----------------------------------------------------------------------------------------------
;;; Code:

;; Verilog-Mode supports syntax highlighting of SystemVerilog
;; (IEEE 1800-2017), Verilog (IEEE 1364-2005), and the Universal
;; Verification Modeling language (UVM). Verilog-Mode also has
;; AUTOs which greatly accelerate maintaining interconnect, resets,
;; and other boiler-plate code.
;;
;; Link: https://github.com/veripool/verilog-mode
(use-package verilog-mode
  :commands (verilog-mode)
  :mode (("\\.v\\'" . verilog-mode)
	 ("\\.vh\\'" . verilog-mode))
  :interpreter ("verilog" . verilog-mode)
  :ensure t)

;;; verilog.el ends here
