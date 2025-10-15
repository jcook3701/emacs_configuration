;;; settings.el --- Summary
;;; Commentary:
;; Emacs settings configuration
;;----------------------------------------------------------------------------------------------
;;; Code:

;; Only display errors
;; (setq warning-minimum-level "error")
(setq native-comp-async-report-warnings-errors nil)

;; Make necessary directories
(make-directory "~/.emacs.d/tmp" t)
(make-directory "~/.emacs.d/rtags" t)
(make-directory "~/.emacs.d/org" t)
(make-directory "~/.emacs.d/auto-save-list/" t)
(make-directory "~/.emacs.d/emacs-saves/" t)

;; Emacs temporary directory location.
(setq temporary-file-directory "~/.emacs.d/tmp/")

;;Save auto made backup files to below dirs.
(setq auto-save-file-name-transforms `((".*" "~/.emacs.d/auto-save-list/" t)))
(setq backup-directory-alist '(("." . "~/.emacs.d/emacs-saves/")))

;; Set Meta Key - ESC
(setq x-meta-keysym 'meta)

;; Remove Default Keybindings
(define-key global-map (kbd "C-t") nil) ;; This is remapped for vterm
(define-key global-map (kbd "C--") nil) ;; Removed to avoid accidentally using this command.
(define-key global-map (kbd "C-_") nil) ;; This is remapped to scroll-down-command
(define-key global-map (kbd "C-x C-n") nil) ;; This is for dired-sidebar

;; Modify Default key bindings
;; (define-key global-map (kbd "M-v") 'universal-argument)
;; Scroll: Page up the screen.
;; (define-key global-map (kbd "C-u") 'scroll-down-command)

;; Zome in/out like everywhere else
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C-_") 'text-scale-decrease)

;; Follow symlinks
(setq vc-follow-symlinks t)

;; Full Screen

(setq inhibit-startup-screen t)         ;; Inhibit Default Startup Screen
(setq initial-scratch-message "")       ;; No message in scratch buffer

;; Increase max lisp depth for package installation
(setq max-lisp-eval-depth 2000)
(setq max-specpdl-size 2000)


;; TODO: Set: `lsp-enable-file-watchers' and `lsp-file-watch-threshold' variables

;; Shell Feature - Allows ability to open emacs inside of emacs bash
(server-start)
(setq server-socket-dir "~/tmp/emacs1000/server")

;; Set Face Attributes
(set-face-attribute 'mode-line nil :foreground "#030303" :background "#bdbdbd" :box nil)
(set-face-attribute 'mode-line-inactive nil :foreground "#f9f9f9" :background "#666666" :box nil)

;; This forces my setup to connect to MELPA over HTTPS
(require 'gnutls)
;; (require 'tls)
(add-to-list 'gnutls-trustfiles "/usr/local/etc/libressl/cert.Perm")

;; Make Emacs display emoji properly
(when (member "Noto Color Emoji" (font-family-list))
  (set-fontset-font t 'emoji (font-spec :family "Noto Color Emoji") nil 'prepend))

;;; settings.el ends here
