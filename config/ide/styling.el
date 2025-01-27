;;; styling.el --- Summary
;;; Commentary:
;; Emacs extra packages configurations
;;----------------------------------------------------------------------------------------------
;;; Code:

;; Background theme
;; A dark theme with lush colors for Emacs24, based on JD Huntington's blackboard theme
;;
;; Link: https://github.com/andre-richter/emacs-lush-theme
(use-package lush-theme
  :config
  (setq custom--inhibit-theme-enable nil)
  (load-theme 'lush t)
  :ensure t)

;; Line numbers in side column
;; Link: https://github.com/emacsmirror/nlinum
(use-package nlinum
  ;; :init (global-nlinum-mode 1) ;; Always on
  :hook (prog-mode . nlinum-mode) ;; Only programing mode
  :init
  ;;line numbers
  ;; (global-display-line-numbers-mode t)
  ;; (setq linum-format "%3d\u2502 ")
  :ensure t)

;; rainbow-delimiters is a "rainbow parentheses"-like mode which highlights
;; delimiters such as parentheses, brackets or braces according to their depth.
;; Each successive level is highlighted in a different color. This makes it
;; easy to spot matching delimiters, orient yourself in the code, and tell
;; which statements are at a given depth.
;;
;; Link: https://github.com/Fanael/rainbow-delimiters
(use-package rainbow-delimiters
  :commands (rainbow-delimiters rainbow-delimiters-mode)
  :hook (prog-mode . rainbow-delimiters-mode)
  :init
  (show-paren-mode 1)
  :ensure t)

;; Emacs fontawesome utility.
;;
;; Link: https://github.com/emacsorphanage/fontawesome
(use-package fontawesome
  :ensure t)

;; Icons
;; Link: https://github.com/domtronn/all-the-icons.el
;;
;; Requires you to run: M-x all-the-icons-install-fonts
(use-package all-the-icons
  :if (display-graphic-p)
  :config
  ;; Function to install all-the-icons fonts
  (defun install-all-the-icons-fonts ()
    "Install the all-the-icons fonts."
    (unless (file-exists-p "~/.local/share/fonts/all-the-icons.ttf")
      (all-the-icons-install-fonts t)))
  
  ;; Run the function to install the fonts
  (install-all-the-icons-fonts)
  :ensure t)

;;;(use-package all-the-icons-dired
;;;  :hook (dired-mode . all-the-icons-dired-mode)
;;;  :ensure t)

;; Display icons for all buffers in ibuffer.
;;
;; Link: https://github.com/seagle0128/all-the-icons-ibuffer
(use-package all-the-icons-ibuffer
  :config
  (all-the-icons-ibuffer-mode 1)
  :after ibuffer
  :ensure t)

;;;(use-package dired-icon
;;;  :hook (dired-mode . dired-icon-mode)
;;;  :ensure t)

;; Treemacs Icons for Dired -
;; Allows you to use treemacs icons in dired buffers with treemacs-icons-dired-mode
;;
;; Link: https://github.com/Alexander-Miller/treemacs
(use-package treemacs-icons-dired
  :config
  (treemacs-icons-dired-mode)
  :after treemacs
  :ensure t)

;; Treemacs Icons for Helm
;;
;; Link: https://github.com/yyoncho/helm-icons
(use-package helm-icons
  :config
  (helm-icons-enable)
  :after helm
  :ensure t)

;;;(use-package vscode-icon
;;;  :commands (vscode-icon-for-file)
;;;  :ensure t)

;;; styling.el ends here
