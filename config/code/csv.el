;;; csv.el --- Summary
;;; Commentary:
;; Emacs csv packages configurations
;;----------------------------------------------------------------------------------------------
;;; Code:

;; This package implements CSV mode, a major mode for editing records
;; in a generalized CSV (character-separated values) format.  It binds
;; files with prefix ".csv" to `csv-mode' (and ".tsv" to `tsv-mode') in
;; `auto-mode-alist'.
;;
;; Link: https://github.com/emacsmirror/emacswiki.org/blob/master/csv-mode.el
(use-package csv-mode
  :commands (csv-mode)
  :mode ("\\.csv\\'" . csv-mode)
  :ensure t)

;;; csv.el ends here
