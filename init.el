;;; init.el --- Summary
;;; Commentary:
;; jcook3701's Emac init
;; For: GNU Emacs 25.3.50.2
;; compiled with flags --with-ns --with-gnutls -with-xwidgets --with-imagemagick

;; Follow symlinks
(setq vc-follow-symlinks t)

;; Full Screen
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(package-selected-packages
   (quote
    (bash-completion vscode-icon dired-sidebar markdown-mode magit winum virtualenvwrapper use-package treemacs-projectile jedi irony-eldoc helm-rtags helm-flyspell flyspell-correct-helm flycheck-rtags flycheck-irony elpy diminish company-rtags company-irony-c-headers company-irony cmake-ide cask))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Shell Feature - Allows ability to open emacs inside of emacs bash
(server-start)
(setq server-socket-dir "~/tmp/emacs1000/server")

;; Background theme
;; This is so low in the init file because it needed to be below spacemacs for the time that I was using that
(load-file "~/.emacs.d/themes/lush-theme-old.el")
;;(load-file "~/.emacs.d/themes/lush-theme.el")

;;line numbers
(global-linum-mode t)
(setq linum-format "%3d\u2502 ")

;;Save auto made backup files to below dirs.
(setq auto-save-file-name-transforms `((".*" "/Users/jared3701/.emacs.d/auto-save-list/" t)))
(setq backup-directory-alist '(("." . "/Users/jared3701/.emacs.d/emacs-saves/")))

;; This forces my setup to connect to MELPA over HTTPS
(require 'gnutls)
(require 'tls)
(add-to-list 'gnutls-trustfiles "/usr/local/etc/libressl/cert.pem")

;; MELPA package repo
(require 'package)
;;(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
		    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;;(load-theme 'spacemacs-dark t)

;; Flyspell Mode Function
(defun turn-on-flyspell()
  (flyspell-mode 1))

(require 'ispell)
(setq ispell-dictionary "en_US")

(mapcar (lambda (mode-hook) (add-hook mode-hook 'turn-on-flyspell))
	'(markdown-mode-hook text-mode-hook))

(mapcar (lambda (mode-hook) (add-hook mode-hook 'flyspell-prog-mode))
	'(c-mode-common-hook python-mode-hook emacs-lisp-mode-hook html-mode-hook js-mode-hook))

;; bash-completion
(autoload 'bash-completion-dynamic-complete 
  "bash-completion"
  "BASH completion hook")
(add-hook 'shell-dynamic-complete-functions
	  'bash-completion-dynamic-complete)

;; ------- Python Packages for Emacs ------- ;;
;; virtualenvwrapper
;;(require 'virtualenvwrapper)
;;(setq venv-location "/Users/jared3701/Documents/College/5th_year/seniorProject/gitHub/supreme_bot")

;; elpy
(elpy-enable)
(setq elpy-rpc-backend "jedi")

;;-------------------------------;;
;; Custom Setup for python shell ;;
;;-------------------------------;;
;;Run python and pop-up its shell.
;; Kill process to solve the reload modules problem.
(defun my-python-shell-run ()
  (interactive)
  (when (get-buffer-process "*Python*")
    (set-process-query-on-exit-flag (get-buffer-process "*Python*") nil)
    (kill-process (get-buffer-process "*Python*"))
    ;; if you want to clean the buffer too.
    ;; (kill-buffer "*Python*")
    ;; Not so fast!
    (sleep-for 0.5))
  (run-python (python-shell-parse-command) nil nil)
  (elpy-shell-send-buffer t)
  ;; Pop a new window only if shell isn't visible
  ;; in any frame.
  (unless (get-buffer-window "*Python*" t)
    (elpy-shell-switch-to-shell)))

(defun my-python-shell-run-region ()
  (interactive)
  (python-shell-send-region (region-beginning) (region-end))
  (python-shell-switch-to-shell))

(eval-after-load 'elpy
  `(progn
     (message "python cfg has finished loading")
     (define-key elpy-mode-map (kbd "C-c C-c") 'my-python-shell-run)
     (define-key elpy-mode-map (kbd "C-c C-r") 'my-python-shell-run-region)
     (define-key elpy-mode-map (kbd "C-c f") 'python-eldoc-at-point)))
    

;; jedi Package
;;(add-hook 'python-mode-hook 'jedi:setup)
;;(setq jedi:complete-on-dot t)     

;; use-package
(eval-when-compile
  (require 'use-package))

;; File Manager
(use-package dired-sidebar
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)

  (setq dired-sidebar-subtree-line-prefix "__")
  (setq dired-sidebar-theme 'vscode)
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t))

;; cmake-ide
(setq cmake-ide-build-dir "/Users/jared3701/.emacs.d/cmake-ide-build-dir")
(setq cmake-ide-build-pool-dir "/Users/jared3701/.emacs.d/cmake-ide-build-dir")
(setq cmake-ide-build-pool-use-persistent-naming t)
(require 'rtags)
(require 'company)
(cmake-ide-setup)

;; Company Package
(add-hook 'after-init-hook 'global-company-mode)

;; Rtags Package
;;(add-hook 'c-mode-hook 'rtags-start-process-unless-running)
;;(add-hook 'c++-mode-hook 'rtags-start-process-unless-running)
;;(add-hook 'objc-mode-hook 'rtags-start-process-unless-running)
;;(setq company-backends '(company-rtags))

(setq rtags-autostart-diagnostics t)
(rtags-diagnostics)
(setq rtags-completions-enabled t)
(push 'company-rtags company-backends)
(global-company-mode)
(define-key c-mode-base-map (kbd "<C-tab>") (function company-complete))

(require 'flycheck-rtags)

;; RTags with Flycheck
(defun my-flycheck-rtags-setup ()
  (flycheck-select-checker 'rtags)
  (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
  (setq-local flycheck-check-syntax-automatically nil))
(add-hook 'c-mode-hook #'my-flycheck-rtags-setup)
(add-hook 'c++-mode-hook #'my-flycheck-rtags-setup)
(add-hook 'objc-mode-hook #'my-flycheck-rtags-setup)

;; Rtag helm
(require 'helm-config)
(helm-mode 1)
(define-key global-map [remap find-file] 'helm-find-files)
(define-key global-map [remap occur] 'helm-occur)
(define-key global-map [remap list-buffers] 'helm-buffers-list)
(define-key global-map [remap dabbrev-expand] 'helm-dabbrev)
(define-key global-map [remap execute-extended-command] 'helm-M-x)
(unless (boundp 'completion-in-region-function)
  (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
  (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))

(setq rtags-display-result-backend 'helm)

;; Helm-flyspell
(require 'helm-flyspell)
(define-key flyspell-mode-map (kbd "C-;") 'helm-flyspell-correct)

;;Irony with Flycheck
(eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

;;Company Irony
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)

;;(setq company-backends (delete 'company-semantic company-backends))
(require 'company-irony-c-headers)
(eval-after-load 'company
  '(add-to-list 'company-backends '(company-irony-c-headers company-irony)))

;; Live Testing for Emacs run on XQartz
;; Version: Emacs 25.3
;; Built with:
;;
;;     ./autogen.sh all
;;     ./configure --with-xwidgets --without-ns --with-gnutls --with-imagemagick --without-dbus --with-x
;;     make install
;;
;; This is actively running on Mac OS High Sierra
;; Version: 10.13.6

;; make these keys behave like normal browser
;; I need to figure out a way to have this config file differentiate between my webkit build
;; and my regular work build.  Until then I will just comment then out when I see an error and
;; un-comment when I'm playing with my webkit build of emacs that uses X11 to run.
;;(define-key xwidget-webkit-mode-map [mouse-4] 'xwidget-webkit-scroll-down)
;;(define-key xwidget-webkit-mode-map [mouse-5] 'xwidget-webkit-scroll-up)
;;(define-key xwidget-webkit-mode-map (kbd "<up>") 'xwidget-webkit-scroll-down)
;;(define-key xwidget-webkit-mode-map (kbd "<down>") 'xwidget-webkit-scroll-up)
;;(define-key xwidget-webkit-mode-map (kbd "M-w") 'xwidget-webkit-copy-selection-as-kill)
;;(define-key xwidget-webkit-mode-map (kbd "C-c") 'xwidget-webkit-copy-selection-as-kill)

;; adapt webkit according to window configuration chagne automatically
;; without this hook, every time you change your window configuration,
;; you must press 'a' to adapt webkit content to new window size
(add-hook 'window-configuration-change-hook (lambda ()
               (when (equal major-mode 'xwidget-webkit-mode)
                 (xwidget-webkit-adjust-size-dispatch))))

;; by default, xwidget reuses previous xwidget window,
;; thus overriding your current website, unless a prefix argument
;; is supplied
;;
;; This function always opens a new website in a new window
(defun xwidget-browse-url-no-reuse (url &optional sessoin)
  (interactive (progn
                 (require 'browse-url)
                 (browse-url-interactive-arg "xwidget-webkit URL: "
                                             )))
  (xwidget-webkit-browse-url url t))

;; make xwidget default browser
(setq browse-url-browser-function (lambda (url session)
                    (other-window 1)
                    (xwidget-browse-url-no-reuse url)))


;;; ----- init.el ends here ----- ;;;
