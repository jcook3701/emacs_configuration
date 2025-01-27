;;; melpa.el --- Summary
;;; Commentary:
;; Emacs melpa package manager configuration
;;----------------------------------------------------------------------------------------------
;;; Code:

;; MELPA package repository
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
  		    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; https://emacs.stackexchange.com/questions/233/how-to-proceed-on-package-el-signature-check-failure
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "org" (concat proto "://orgmode.org/elpa/")) t)
  (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;;; melpa.el ends here
