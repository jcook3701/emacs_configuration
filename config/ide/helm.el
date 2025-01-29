;;; helm.el --- Summary
;;; Commentary:
;; Emacs helm packages configurations
;;----------------------------------------------------------------------------------------------
;;; Code:

;; Helm is an Emacs framework for incremental completions and narrowing
;; selections. It provides an easy-to-use API for developers wishing to
;; build their own Helm applications in Emacs, powerful search tools and
;; dozens of already built-in commands providing completion to almost
;; everything. It is a must-have for anyone using Emacs as a main work
;; environment. Helm has been widely adopted by many Emacs power-users.
;; It is available in Melpa and can be easily installed from the Emacs
;; package manager.
;;
;; Link: https://github.com/emacs-helm/helm
(use-package helm
  :config
  ;; (require 'helm-config)
  (helm-mode 1)
  (define-key global-map [remap find-file] 'helm-find-files)
  (define-key global-map [remap occur] 'helm-occur)
  (define-key global-map [remap switch-to-buffer] 'helm-mini)
  ;; (define-key global-map [remap list-buffers] 'helm-buffers-list)
  (define-key global-map [remap dabbrev-expand] 'helm-dabbrev)
  (define-key global-map [remap execute-extended-command] 'helm-M-x)
  (unless (boundp 'completion-in-region-function)
    (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
    (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))
  
  (setq rtags-display-result-backend 'helm)
  :ensure t)

;; Helm-flyspell - Helm extension for correcting words with Flyspell.
;;
;; Link: https://github.com/pronobis/helm-flyspell
;;(require 'helm-flyspell)
;;(define-key flyspell-mode-map (kbd "C-;") 'helm-flyspell-correct)
(use-package helm-flyspell
  :commands (helm-flyspell-correct)
  :bind
  (:map flyspell-mode-map
	("C-;" . helm-flyspell-correct))
  :after helm flyspell
  :ensure t)

;; Projectile integration with Helm
;;
;; Link: https://github.com/bbatsov/helm-projectile
(use-package helm-projectile
  :after (projectile helm)
  :config
  (helm-projectile-on)
  :bind
  (("C-c p p" . helm-projectile-switch-project)
   ("C-c p f" . helm-projectile-find-file)
   ("C-c p s" . helm-projectile-ag))
  :ensure t)

;; helm-gtags.el is GNU GLOBAL helm interface.
;;
;; Link: https://github.com/emacsorphanage/helm-gtags
(use-package helm-gtags
  :after (helm ggtags)
  :hook
  (((c-ts-mode c-mode) . helm-gtags-mode)
   ((c++-ts-mode c++-mode) . helm-gtags-mode))
  :bind
  (("M-." . helm-gtags-dwim)         ;; Go to symbol
   ("M-," . helm-gtags-pop-stack)     ;; Jump back to previous location
   ("C-c g r" . helm-gtags-find-reference)) ;; Find references
  :ensure t)

;; A call to helm-make will give you a helm selection of this directory
;; Makefile's targets. Selecting a target will call compile on it. You
;; can cancel as usual with C-g. Support is provided for the various
;; flavors of Make tools, as well as the Ninja build tool.
;;
;; Link: https://github.com/abo-abo/helm-make
(use-package helm-make
  :after helm
  :ensure t)

;; This package integrates the catkin build tool for ROS packages into Emacs. With it you can:
;; * Build one, multiple or all packages in the workspace
;; * Setup, initialize and clean the workspace
;; * Configure cmake, make and catkin_make arguments
;; * Blacklist or whitelist packages in the workspace
;;
;; Link: https://github.com/gollth/helm-catkin
(use-package helm-catkin
  :after helm
  :ensure t)

;; Helm Interface for Chrome bookmarks.
;;
;; Link: https://github.com/kawabata/helm-chrome
(use-package helm-chrome
  :after helm
  :ensure t)

;; Browse your Chrome history with Helm.
;;
;; Link: https://github.com/xuchunyang/helm-chrome-history
(use-package helm-chrome-history
  :after helm
  :ensure t)

;; helm interface for codesearch
;;
;; Link: https://github.com/youngker/helm-codesearch.el
(use-package helm-codesearch
  :after helm
  :ensure t)

;;;(use-package helm-ros
;;;  :ensure t)

;; This package lets you Start/Restart/Stop and view status of systemd’s
;; units with helm.
;;
;; Link: https://github.com/Lompik/helm-systemd
(use-package helm-systemd
  :after helm
  :ensure t)

;; helm-themes.el provides Emacs themes selection with helm interface.
;;
;; Link: https://github.com/emacsorphanage/helm-themes
(use-package helm-themes
  :after helm
  :ensure t)

;;;(use-package helm-google-helm
;;;  :ensure t)

;; helm-ag.el provides interfaces of The Silver Searcher with helm.
;;
;; Link: https://github.com/emacsorphanage/helm-ag
(use-package helm-ag
  :after helm  ;; Ensure Helm is loaded before Helm-Ag
  :config
  (setq helm-ag-base-command "ag --nocolor --nogroup --ignore-case")  ;; Customize ag command options if needed
  :bind
  (("M-s a" . helm-ag)         ;; Search the entire project/directory with ag (M-s a)
   ("M-s A" . helm-ag-project-root))  ;; Search within the current project root (M-s A)
  :ensure t)

;; Helm for org headlines and keywords completion.
;;
;; Link: https://github.com/emacs-helm/helm-org
;; (use-package helm-org
;;   :after helm org
;;  :bind ("C-c a" . helm-org-agenda-files-headings))

;;; helm.el ends here
