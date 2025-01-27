;;; ide.el --- Summary
;;; Commentary:
;; Emacs IDE package configurations
;;----------------------------------------------------------------------------------------------
;;; Code:

;; ----------------------------------- IDE Functionality -----------------------------------

;; A GNU Emacs library to ensure environment variables inside Emacs look the same as in the user's shell.
;;
;; Fixes path to npm and other packages to fix lsp-install-packages
;; Link: https://github.com/purcell/exec-path-from-shell
(use-package exec-path-from-shell
  :init
  (when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
  :ensure t)

;; This is an EditorConfig plugin for Emacs.
;;
;; Link: https://github.com/editorconfig/editorconfig-emacs
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))  ;; Enable EditorConfig globally

;; Highlight TODO and similar keywords in comments and strings
;;
;; Link: https://github.com/tarsius/hl-todo
(use-package hl-todo
  :custom-face
  (hl-todo ((t (:inherit hl-todo :italic t))))
  :hook ((prog-mode . hl-todo-mode)
         ((text-mode) . hl-todo-mode))
  :ensure t)

;; Projectile is a project interaction library for Emacs.
;;
;; https://github.com/bbatsov/projectile
(use-package projectile
  :init
  (setq projectile-completion-system 'helm) ;; Using Helm
  :config
  (setq projectile-tags-command "ctags -Re") ;; Command to generate tags
  (projectile-mode 1)
  :hook
  ;; ctags setup (regenerating tags after switching project)
  (projectile-after-switch-project-hook .
        (lambda ()
	  (let ((default-directory (projectile-project-root)))
	    (shell-command "ctags -Re ."))))
  :bind
  (("C-c p p" . projectile-switch-project)
   ("C-c p f" . projectile-find-file))
  :ensure t)

;; Tab Manager - This projects aims to become an aesthetic, functional and efficient tabs
;; plugin for Emacs with a lot of customization options. Although this is a
;; fork from awesome-tab (that I forked with the permission from the author
;; and it’s also based on tabbar) it’s been heavily modified so now it may
;; be considered a different package. Also this package integrates functionalities
;; from tabbar-ruler.
;; 
;; Link: https://github.com/ema2159/centaur-tabs
(use-package centaur-tabs
  :demand
  :init
  (setq centaur-tabs-set-icons t)
  :config
  (centaur-tabs-mode t)
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward)
  :ensure t)

;; This file is an implementation of a minimap sidebar, i.e., a smaller
;; display of the current buffer on the left side. It highlights the
;; currently shown region and updates its position automatically. You
;; can navigate in the minibar by dragging the active region with the mouse,
;; which will scroll the corresponding edit buffer. Additionally, you can
;; overlay information from the tags gathered by CEDET's semantic analyzer.
;;
;; Link: https://github.com/dengste/minimap
(use-package minimap
  ;; :custom
  ;; (window-size-fixed t)
  :init
  ;; (zoom-ignored-buffer-names '("*MINIMAP*" "*minimap*"))
  ;; (setq minimap-dedicated-window t)
  (setq minimap-window-location 'right)
  ;; (minimap-mode 1)
  :ensure t)

;; A nice looking theme for the bottom power-bar
;;
;; Link: https://www.emacswiki.org/emacs/PowerLine
(use-package powerline
  :init
  (setq powerline-default-separator 'arrow-fade)
  (custom-set-faces
   ;; (setq powerline-color1 "grey22")
   '(mode-line ((t (:foreground "#030303" :background "#bdbdbd" :box nil))))
   ;; (setq powerline-color2 "grey40")
   '(mode-line-inactive ((t (:foreground "#f9f9f9" :background "#666666" :box nil)))))
  (powerline-default-theme)
  :ensure t)

;; Window numbers for Emacs: Navigate your windows and frames using numbers!
;;
;; Link: https://github.com/deb0ch/emacs-winum
(use-package winum
  :init
  (setq winum-keymap
	(let ((map (make-sparse-keymap)))
	  (define-key map (kbd "C-`") 'winum-select-window-by-number)
	  (define-key map (kbd "C-²") 'winum-select-window-by-number)
	  (define-key map (kbd "M-0") 'winum-select-window-0-or-10)
	  (define-key map (kbd "M-1") 'winum-select-window-1)
	  (define-key map (kbd "M-2") 'winum-select-window-2)
	  (define-key map (kbd "M-3") 'winum-select-window-3)
	  (define-key map (kbd "M-4") 'winum-select-window-4)
	  (define-key map (kbd "M-5") 'winum-select-window-5)
	  (define-key map (kbd "M-6") 'winum-select-window-6)
	  (define-key map (kbd "M-7") 'winum-select-window-7)
	  (define-key map (kbd "M-8") 'winum-select-window-8)
	  map))
  :config
  (winum-mode)
  :ensure t)

;; EXWM (Emacs X Window Manager) is a full-featured
;; tiling X window manager for Emacs built on top of XELB.
;; NOTE: This can be used to replace i3 or xfce4 window manger
;;       and allow emacs to run everything.
;; Link: https://github.com/emacs-exwm/exwm/wiki
(use-package exwm
  :init
  ;; Set the initial workspace number.
  (setq exwm-workspace-number 4)
  ;; Make class name the buffer name.
  (add-hook 'exwm-update-class-hook
  (lambda () (exwm-workspace-rename-buffer exwm-class-name)))
  ;; Global keybindings.
  (setq exwm-input-global-keys
	`(([?\s-r] . exwm-reset) ;; s-r: Reset (to line-mode).
          ([?\s-w] . exwm-workspace-switch) ;; s-w: Switch workspace.
          ([?\s-&] . (lambda (cmd) ;; s-&: Launch application.
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command cmd nil cmd)))
          ;; s-N: Switch to certain workspace.
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
			(lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))))
  :ensure t)

;; ----------------------------------- Useful Tools -----------------------------------

;; This is a minor mode to aid in finding common writing problems.
;; Matt Might’s weaselwords scripts inspired this mode.
;;
;; https://github.com/bnbeckwith/writegood-mode
(use-package writegood-mode
  :ensure t)

;; A code searching tool similar to ack, with a focus on speed.
;; 
;; Link: https://github.com/ggreer/the_silver_searcher
(use-package ag
  :ensure t
  :config
  (setq ag-highlight-search t))  ;; Enable highlighting for search results

;; YASnippet is a template system for Emacs. It allows you to type an abbreviation
;; and automatically expand it into function templates. Bundled language templates
;; include: C, C++, C#, Perl, Python, Ruby, SQL, LaTeX, HTML, CSS and more. The snippet
;; syntax is inspired from TextMate's syntax, you can even import most TextMate templates
;; to YASnippet. Watch a demo on YouTube.
;;
;; lsp-mode supports snippets, but in order for them to work you need to use yasnippet
;; If you don't want to use snippets set lsp-enable-snippet to nil in your lsp-mode settings
;; to avoid odd behavior with snippets and indentation
;;
;; Link: https://github.com/joaotavora/yasnippet
(use-package yasnippet
  :ensure t)

;; emacs-slack is a Slack client for emacs
;;
;; Link: https://github.com/yuya373/emacs-slack
;; TODO: This needs updates to do anything.
(use-package slack
  :ensure t)

;; PDF Tools is, among other things, a replacement of DocView for PDF files.
;; The key difference is that pages are not pre-rendered by e.g. ghostscript
;; and stored in the file-system, but rather created on-demand and stored in memory.
;; 
;; This rendering is performed by a special library named, for whatever reason,
;; poppler, running inside a server program. This program is called epdfinfo and
;; its job is to successively read requests from Emacs and produce the proper
;; results, i.e. the PNG image of a PDF page.
;;
;; Actually, displaying PDF files is just one part of PDF Tools. Since poppler can
;; provide us with all kinds of information about a document and is also able to
;; modify it, there is a lot more we can do with it. Watch
;;
;; Link: https://github.com/politza/pdf-tools
(use-package pdf-tools
  :config
  (pdf-loader-install)
  :ensure t)

;; google-this.el is a package that provides a set of functions and
;; keybindings for launching google searches from within emacs.
;; 
;; This gives ability to start search in google with
;; kdb "C-c /-Enter"
;;
;; Link: https://github.com/Malabarba/emacs-google-this
(use-package google-this
  :config
  ;; (setq google-this-keybind (kbd "C-x g"))
  (google-this-mode 1)
  :ensure t)

;; i3 Window/Tiling Manager -- Future
;;;(use-package i3wm
;;;  :init
;;;  (i3-mode 1)
;;;  :ensure t)

;;;(use-package i3wm-config-mode
;;;  :ensure t)

;; Not a fan of this package with this setup.  Might try and lock dired-sidebar,
;; ibuffer-sidebar, and minimap in their respective locations but at this moment
;; it really messes up the window layout that I am trying to create with these
;; applications.  I also had issues with this app causing the vterm window to
;; sporadically change size making it unusable.
;; Window Manager
;;
;; Link: https://github.com/cyrus-and/zoom
;;;(use-package zoom
;;;  :custom
;;;  (zoom-mode t)
;;;  (zoom-size '(0.618 . 0.618))
;;;  (zoom-ignored-major-modes '(dired-mode markdown-mode))
;;;  (zoom-ignored-buffer-name-regexps '("^\*MINIMAP.*$"))
;;;  (zoom-ignored-buffer-name-regexps '(".*vterm.*"))
;;;  (zoom-ignore-predicates '((lambda () (> (count-lines (point-min) (point-max)) 20))))
;;;  :init
;;;  ;; Note this should be fixed tomorrow....
;;;  (defun my/fix-imenu-size ()
;;;    (with-selected-window (get-buffer-window "*Ilist*")
;;;      (setq window-size-fixed t)
;;;      (window-resize (selected-window) (- 30 (window-total-width)) t t)))
;;;  
;;;  (add-hook 'imenu-list-update-hook 'my/fix-imenu-size)
;;;  :ensure t)

;; smooth-scrolling, minimap and distraction-free mode (inspired by the sublime editor)
;;
;; Link: https://github.com/zk-phi/sublimity
;;;(use-package sublimity
;;;  :commands (sublimity-map-show)
;;;  :hook (sublimity-mode . sublimity-map-show)
;;;  :init
;;;  (require 'sublimity-scroll)
;;;  (require 'sublimity-map)
;;;  (require 'sublimity-attractive)
;;;  ;; Scroll Settings
;;;  (setq sublimity-scroll-weight 10)
;;;  (setq sublimity-scroll-drift-length 5)
;;;  ;; Map Settings
;;;  (setq sublimity-map-size 20)
;;;  (setq sublimity-map-fraction 0.3)
;;;  (setq sublimity-map-text-scale -7)
;;;  (sublimity-map-set-delay 5)
;;;  
;;;  ;; Attractive Settings
;;;  (setq sublimity-attractive-centering-width 100)
;;;  
;;;  (sublimity-mode 1)
;;;  :ensure t)

;; bash-completion ;; Replaced by vterm -- to be removed
;;;(autoload 'bash-completion-dynamic-complete
;;;  "bash-completion"
;;;  "BASH completion hook")
;;;(add-hook 'shell-dynamic-complete-functions
;;;	  'bash-completion-dynamic-complete)

;;; ide.el ends here
