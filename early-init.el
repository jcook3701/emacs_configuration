;;; early-init.el --- Summary
;;; Commentary:
;; jcook3701's Emacs early-init.el
;;----------------------------------------------------------------------------------------------
;;; Code:

;; Disable package.el in favor of Elpaca
(setq package-enable-at-startup nil)
(setq package--init-file-ensured t)

;; https://github.com/progfolio/elpaca/issues/236#issuecomment-1879838229
;; (setq elpaca-menu-functions '(elpaca-menu-extensions elpaca-menu-gnu-devel-elpa))

;;; early-init.el ends here
