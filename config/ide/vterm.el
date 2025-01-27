;;; vterm.el --- Summary
;;; Commentary:
;; Emacs vterm packages configurations
;;----------------------------------------------------------------------------------------------
;;; Code:

;; Emacs-libvterm (vterm) is fully-fledged terminal emulator inside GNU Emacs
;; based on libvterm, a C library. As a result of using compiled code (instead
;; of elisp), emacs-libvterm is fully capable, fast, and it can seamlessly handle
;; large outputs.
;;
;; vTerm is awesome but it should be noted that it does require that emacs is built
;; from source code with the flag "--with-modules".  All flags that I use to build
;; Emacs are included at the top of this document.
;; 
;; Link: https://github.com/akermu/emacs-libvterm
(use-package vterm
  :bind
  ;; Removed Keybindings that I need for emacs within Vterm
  (:map vterm-mode-map
	("<escape>" . nil)
	("M-w" . nil)
	("C-t" . nil)
;;	("C-c" . vterm-send-C-c)
;;	("C-s" . vterm-send-C-s)
;;	("C-w" . vterm-send-C-w)
;;	("C-x" . vterm-send-C-x)
	)
  :init
  (defun vterm-directory-sync ()
  "Synchronize current working directory."
  (interactive)
  (when vterm--process
    (let* ((pid (process-id vterm--process))
           (dir (file-truename (format "/proc/%d/cwd/" pid))))
      (setq default-directory dir))))
  :ensure (vterm :post-build
                 (progn
                   (setq vterm-always-compile-module t)
                   (require 'vterm)
                   ;;print compilation info for elpaca
                   (with-current-buffer (get-buffer-create vterm-install-buffer-name)
                     (goto-char (point-min))
                     (while (not (eobp))
                       (message "%S"
                                (buffer-substring (line-beginning-position)
                                                  (line-end-position)))
                       (forward-line)))
                   (when-let ((so (expand-file-name "./vterm-module.so"))
                              ((file-exists-p so)))
                     (make-symbolic-link
                      so (expand-file-name (file-name-nondirectory so)
                                           "../../builds/vterm")
                      'ok-if-already-exists)))))

;; This package provides the command vterm-toggle which toggles between the
;; vterm buffer and whatever buffer you are editing.
;;
;; Link: https://github.com/jixiuf/vterm-toggle
(use-package vterm-toggle
  :commands (vterm-toggle vterm-toggle-cd vterm-toggle-insert-cd vterm-toggle-forward vterm-toggle-backward)
  :init
  :bind
  (:map global-map
	("C-t l" . vterm-toggle)
	("C-t t" . vterm-toggle-cd)
	("<control> - <return>" . vterm-toggle-insert-cd)
  	("C-t j" . vterm-toggle-forward)
	("C-t k" . vterm-toggle-backward))
  :ensure t)

;; Managing multiple vterm buffers in Emacs This package is inspired by multi-term.el
;;
;; Link: https://github.com/suonlight/multi-vterm
;; Extra Commands: multi-vterm-next multi-vterm-prev multi-vterm-dedicated-toggle
(use-package multi-vterm
  :commands (multi-vterm multi-vterm-project) 
  :bind
  (:map global-map
	("C-t o" . multi-vterm)
	;; ("C-t k" . multi-vterm-next)
	;; ("C-t j" . multi-vterm-prev)
	;; ("C-t t" . multi-vterm-dedicated-toggle)
	("C-t l" . multi-vterm-project))
  
  ;; (:map vterm-mode-map
  ;;	("<escape>" . nil)
  ;;	("C-t" . nil))
  :ensure t)

;;; vterm.el ends here
