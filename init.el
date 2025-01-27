;;; init.el --- Summary
;;; Commentary:
;; jcook3701's Emacs init.el
;;----------------------------------------------------------------------------------------------
;; For GNU Emacs 29.4 - Linux OS
;;----------------------------------------------------------------------------------------------
;; Configuration Flags:
;;----------------------------------------------------------------------------------------------
;; --with-tree-sitter --with-native-compilation --with-json --with-mailutils --with-jpeg
;; --with-png --with-rsvg --with-tiff --with-gif --with-xft --with-xml2 --without-ns
;; --with-gnutls --with-imagemagick --with-xwidgets --with-x --with-modules --with-harfbuzz
;;----------------------------------------------------------------------------------------------
;;; Code:

(load "~/.emacs.d/config/settings.el")
(load "~/.emacs.d/config/pkg-manager/elpaca.el")

(load "~/.emacs.d/config/ide.el")
(load "~/.emacs.d/config/code.el")

;; TODO: Setup email and calendar
;; (load "~/.emacs.d/config/email.el")
;; (load "~/.emacs.d/config/calendar.el")

;; -------------------------- Helpful keybindings -------------------------- ;;
;;
;; C-h k KEY - describe what KEY is bound to

;; Helpful Links:
;;   Link0: https://stackoverflow.com/questions/11713743/indent-several-lines-in-emacs
;;   Link1:

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(yasnippet yaml writegood-mode web-mode typescript-mode sed-mode scala-mode rust-mode rainbow-delimiters projectile pdf-tools pacmacs nlinum nginx-mode minimap markdown-mode magit json-mode js2-mode jinja2-mode helm go-mode ggtags flycheck exwm exec-path-from-shell editorconfig dockerfile-mode csv-mode company chess)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:foreground "#030303" :background "#bdbdbd" :box nil))))
 '(mode-line-inactive ((t (:foreground "#f9f9f9" :background "#666666" :box nil)))))
