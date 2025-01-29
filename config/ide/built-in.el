;;; built-in.el --- Summary
;;; Commentary:
;; Emacs built-in packages configurations
;;----------------------------------------------------------------------------------------------
;;; Code:

(use-package auth-source
  :ensure nil ;; 'auth-source' is built-in
  :config
  ;; Your custom configuration for auth-source goes here
  (setq auth-sources '("~/.authinfo.gpg")))

;; Ibuffer is an advanced replacement for BufferMenu, which lets you operate
;; on buffers much in the same manner as Dired. The most important Ibuffer
;; features are highlighting and various alternate layouts. Ibuffer is part of
;; Emacs since version 22.
;;
;; Link: https://www.emacswiki.org/emacs/IbufferMode
(use-package ibuffer
  :ensure nil ;; 'ibuffer' is built-in
  :init
  ;; Define Key: C-x C-b
  (define-key global-map [remap list-buffers] 'ibuffer))

;; TRAMP (Transparent Remote Access, Multiple Protocols) is a package for
;; editing remote files, similar to AngeFtp or efs. Whereas the others use
;; FTP to connect to the remote host and to transfer the files, TRAMP uses
;; a remote shell connection (rlogin, telnet, ssh). It can transfer the
;; files using rcp or a similar program, or it can encode the file contents
;; (using uuencode or base64) and transfer them right through the shell connection.
;;
;; Link: https://www.emacswiki.org/emacs/TrampMode
(use-package tramp
  :ensure nil ;; 'tramp' is built-in
  :defer t
  :config
  (setq tramp-verbose 1)
  (setq tramp-default-method "ssh"))

;; “Flyspell enables on-the-fly spell checking in Emacs by the means of a minor mode.
;; It is called Flyspell. This facility is hardly intrusive. It requires no help.
;; Flyspell highlights incorrect words as soon as they are completed or as soon as
;; the TextCursor hits a new word.”
;;
;; Link: https://www.emacswiki.org/emacs/FlySpell
(use-package flyspell
  :ensure nil ;; 'flyspell' is built-in
  :init
  (require 'ispell)
  (defun turn-on-flyspell()
    (flyspell-mode 1))

  (setq ispell-dictionary "en_US")
  
  (mapcar (lambda (mode-hook) (add-hook mode-hook 'turn-on-flyspell))
	  '(markdown-mode-hook gfm-mode-hook text-mode-hook))
  
  (mapcar (lambda (mode-hook) (add-hook mode-hook 'flyspell-prog-mode))
	  '(c-mode-common-hook python-mode-hook emacs-lisp-mode-hook html-mode-hook js-mode-hook))
  :bind
  (:map flyspell-mode-map
	("C-;" . nil)))


;; NOTE: python, dired, ob, & tresitter have been moved outside of this file but are also built-in packages.

;;; built-in.el ends here
