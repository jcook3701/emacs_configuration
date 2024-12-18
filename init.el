;;; init.el --- Summary
;;; Commentary:
;; jcook3701's Emacs init.el
;;----------------------------------------------------------------------------------------------
;; For GNU Emacs 28.0.91 - Linux OS
;;----------------------------------------------------------------------------------------------
;; Configuration Flags:
;;----------------------------------------------------------------------------------------------
;; --with-mailutils --with-native-compilation --with-json --with-xwidgets
;; --with-modules --with-gnutls --with-jpeg --with-png --with-imagemagick --with-rsvg
;; --with-tiff --with-wide-int --with-xft --with-xml2 --with-xpm
;;----------------------------------------------------------------------------------------------
;;; Code:

;; Only display errors
;; (setq warning-minimum-level "error")
(setq native-comp-async-report-warnings-errors nil)

;;line numbers
(display-line-numbers-mode t)
(setq linum-format "%3d\u2502 ")

;;Save auto made backup files to below dirs.
(setq auto-save-file-name-transforms `((".*" "~/.emacs.d/auto-save-list/" t)))
(setq backup-directory-alist '(("." . "~/.emacs.d/emacs-saves/")))

(setenv "PYTHONIOENCODING" "utf-8")
(add-to-list 'process-coding-system-alist '("python" . (utf-8 . utf-8)))
;; (add-to-list 'process-coding-system-alist '("elpy" . (utf-8 . utf-8)))
(add-to-list 'process-coding-system-alist '("flake8" . (utf-8 . utf-8)))
(add-to-list 'process-coding-system-alist '("python" . (utf-8 . utf-8)))

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(markdown-toc-user-toc-structure-manipulation-fn
   (lambda
     (toc-structure)
     (-filter
      (lambda
	(l)
	(let
	    ((index
	      (car l)))
	  (<= 1 index)))
      toc-structure)))
 '(package-selected-packages
   '(hl-todo flymake-ansible-lint flymake-flycheck ansible-mode ac-haskell-process pyvenv yasnippet dap-mode dired-subtree json-mode lsp-pyright pyvenv-mode python-mode treemacs-icons-dired sed-mode csv-mode org-indent npm-mode go-mode gcode-mode django-mode cmake-mode lsp-metals lsp-jedi lsp-mode tramp tramp--startup-hook tide google-this multi-web-mode web-mode rainbow-delimiters helm-themes helm-systemd helm-ros helm-codesearch helm-chrome-history helm-chrome helm-catkin lush-theme helm-make flycheck-rust js2-mode rust-mode pdf-tools pacmacs chess ansible helm-slack sl slack i3wm-config-mode i3wm ox-gfm org grip-mode markdown-toc magit-todos magit-lfs forge helm-icon helm-icons helm dried-icon dried-icon-mode minimap dired-rainbow dired-open dired-icon all-the-icons-ibuffer all-the-icons-dired ibuffer-sidebar vterm-toggle dired dired-k dired-collapse all-the-icons centaur-tabs sublimity multi-vterm vterm treemacs helm-apt yarn-mode dockerfile-mode docker-compose-mode docker company lsp-ui typescript-mode sbt-mode scala-mode matlab-mode helm-ispell bash-completion vscode-icon dired-sidebar markdown-mode magit winum treemacs-projectile jedi irony-eldoc helm-rtags helm-flyspell flyspell-correct-helm flycheck-rtags flycheck-irony elpy diminish company-rtags company-irony-c-headers company-irony cmake-ide cask)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mode-line ((t (:foreground "#030303" :background "#bdbdbd" :box nil))))
 '(mode-line-inactive ((t (:foreground "#f9f9f9" :background "#666666" :box nil)))))

;; Shell Feature - Allows ability to open emacs inside of emacs bash
(server-start)
(setq server-socket-dir "~/tmp/emacs1000/server")

;; C++ Mode Settings
(setq-default c-basic-offset 4)

;; This forces my setup to connect to MELPA over HTTPS
(require 'gnutls)
;; (require 'tls)
(add-to-list 'gnutls-trustfiles "/usr/local/etc/libressl/cert.Perm")

;; MELPA package repo
(require 'package)
;;(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
		    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; https://emacs.stackexchange.com/questions/233/how-to-proceed-on-package-el-signature-check-failure
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "org" (concat proto "://orgmode.org/elpa/")) t)
  (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;; Insure that use-package is installed and can install the rest of the software on the system
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; bash-completion ;; Replaced by vterm -- to be removed
;;;(autoload 'bash-completion-dynamic-complete
;;;  "bash-completion"
;;;  "BASH completion hook")
;;;(add-hook 'shell-dynamic-complete-functions
;;;	  'bash-completion-dynamic-complete)

;; The use-package macro allows you to isolate package configuration
;; in your .emacs file in a way that is both performance-oriented and,
;; well, tidy.
;;
;; Link: https://github.com/jwiegley/use-package
(eval-when-compile
  (require 'use-package))

;; Fixes path to npm and other packages to fix lsp-install-packages
;; Link: https://github.com/purcell/exec-path-from-shell
(use-package exec-path-from-shell
  :init
  (when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
  :ensure t)

;; Background theme
;; A dark theme with lush colors for Emacs24, based on JD Huntington's blackboard theme
;;
;; Link: https://github.com/andre-richter/emacs-lush-theme
(use-package lush-theme
  :init
  (setq custom--inhibit-theme-enable nil)
  (load-theme 'lush t)
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

;; Icons
;; Link: https://github.com/domtronn/all-the-icons.el
;;
;; Requires you to run: M-x all-the-icons-install-fonts
(use-package all-the-icons
  :ensure t)

;;;(use-package all-the-icons-dired
;;;  :hook
;;;  (dired-mode . all-the-icons-dired-mode)
;;;  :ensure t)

;; Display icons for all buffers in ibuffer.
;;
;; Link: https://github.com/seagle0128/all-the-icons-ibuffer
(use-package all-the-icons-ibuffer
  :init
  (all-the-icons-ibuffer-mode 1)
  :ensure t)

;;;(use-package dired-icon
;;;  :hook (dired-mode . dired-icon-mode)
;;;  :ensure t)

;; Treemacs Icons for Dired -
;; Allows you to use treemacs icons in dired buffers with treemacs-icons-dired-mode
;;
;; Link: https://github.com/Alexander-Miller/treemacs
(use-package treemacs-icons-dired
  :init
  (treemacs-icons-dired-mode)
  :ensure t)

;; Treemacs Icons for Helm
;;
;; Link: https://github.com/yyoncho/helm-icons
(use-package helm-icons
  :defer
  :init
  (helm-icons-enable)
  :ensure t)

;;;(use-package vscode-icon
;;;  :commands (vscode-icon-for-file)
;;;  :ensure t)

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

;; Left Side Bar - File Explorer & More -- Treemacs is a file and project
;; explorer similar to NeoTree or vim’s NerdTree, but largely inspired by
;; the Project Explorer in Eclipse. It shows the file system outlines of
;; your projects in a simple tree layout allowing quick navigation and
;; exploration, while also possessing basic file management utilities. Specifically
;; a quick feature overview looks as follows:
;; 
;; Link: https://github.com/Alexander-Miller/treemacs
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                5000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;; (treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)        
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))


(message "screen width `%s'..." (window-body-width))

;; Ibuffer is an advanced replacement for BufferMenu, which lets you operate
;; on buffers much in the same manner as Dired. The most important Ibuffer
;; features are highlighting and various alternate layouts. Ibuffer is part of
;; Emacs since version 22.
;;
;; Link: https://www.emacswiki.org/emacs/IbufferMode
(use-package ibuffer
  :init
  ;; Define Key: C-x C-b
  (define-key global-map [remap list-buffers] 'ibuffer)
  :ensure t)

;; Buffer Side Bar
;;;(use-package ibuffer-sidebar
;;;  :commands (ibuffer-sidebar-toggle-sidebar)
;;;  ;; :hook (dired-sidebar-mode . ibuffer-sidebar-toggle-sidebar)
;;;  :init  
;;;  (add-hook 'ibuffer-sidebar-mode-hook (lambda () (linum-mode -1)))
;;;  
;;;  ;; (setq ibuffer-sidebar-use-custom-font t)
;;;  ;; (setq ibuffer-sidebar-face `(:family "Helvetica" :height 120))
;;;  (setq ibuffer-sidebar-display-column-titles t)
;;;  ;; (setq ibuffer-sidebar-refresh-on-special-commands t)
;;;  (setq ibuffer-sidebar-display-summary t)
;;;  (setq ibuffer-sidebar-pop-to-sidebar-on-toggle-open nil)
;;;  :ensure t)

;; Dired makes an Emacs buffer containing a listing of a directory, and
;; optionally some of its subdirectories as well. You can use the normal
;; Emacs commands to move around in this buffer, and special Dired commands
;; to operate on the listed files. Dired works with both local and remote
;; directories.
;;
;; Link: https://www.gnu.org/software/emacs/manual/html_node/emacs/Dired.html
(use-package dired
  :bind
  (:map dired-mode-map
   ("i" . nil)))

;; dired-k.el highlights dired buffer like k.
;;
;; Link: https://github.com/emacsorphanage/dired-k
(use-package dired-k
  :after dired
  :commands dired-k
  :bind
  (:map dired-mode-map
	("K" . dired-k))
  :init
  ;; (setq dired-listing-switches "-laGh1v") ;; --group-directories-first
  (setq dired-k-human-readable t)
  (setq dired-k-style "k.zsh")
  ;; (add-hook 'dired-mode-hook 'dired-k)
  (add-hook 'dired-initial-position-hook 'dired-k)
  (add-hook 'dired-after-readin-hook #'dired-k-no-revert)
  :ensure t)

;; This package adds more customizable highlighting for files in dired listings.
;; The group dired-faces provides only nine faces and isn't very fine-grained.
;;
;; Link: https://github.com/Fuco1/dired-hacks
(use-package dired-rainbow
  :config
  (progn
    (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
    (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
    (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata" "launch"))
    (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
    (dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
    (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
    (dired-rainbow-define media "#de751f" ("mp3" "mp4" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
    (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
    (dired-rainbow-define log "#c17d11" ("log"))
    (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
    (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
    (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
    (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
    (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
    (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
    (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
    (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
    (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
    (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
    (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*"))
  :ensure t)

;; Often times we find ourselves in a situation where a single file or directory is
;; nested in a chain of nested directories with no other content. This is sometimes
;; due to various mandatory layouts demanded by packaging tools or tools generating
;; these deeply-nested "unique" paths to disambiguate architectures or versions
;; (but we often use only one anyway). If the user wants to access these directories
;; they have to quite needlessly drill-down through varying number of "uninteresting"
;; directories to get to the content.
;;
;; Link: https://github.com/Fuco1/dired-hacks
(use-package dired-collapse
  ;; :after dired-sidebar
  :hook ((dired-mode . dired-collapse-mode)
	 (dired-sidebar-mode . dired-collapse-mode))
  :ensure t)

;; The basic command to work with subdirectories in dired is i, which inserts the
;; sub-directory as a separate listing in the active dired buffer.
;;
;; This package defines function dired-subtree-insert which instead inserts the
;; sub-directory directly below its line in the original listing, and indent the
;; listing of subdirectory to resemble a tree-like structure (somewhat similar
;; to tree(1) except the pretty graphics). The tree display is somewhat more intuitive
;; than the default "flat" subdirectory manipulation provided by i.
;;
;; Link: https://github.com/Fuco1/dired-hacks
(use-package dired-subtree
  ;; :after dired-sidebar
  :commands dired-subtree-insert dired-subtre-remove dired-subtree-toggle
  :bind
  (:map dired-mode-map
   ("i" . dired-subtree-toggle))
  :ensure t)

;; File Browser - This loads after the ibuffer-sidebar to ensure that the command
;; 'ibuffer-sidebar-toggle-sidebar is available when called from the
;; '+sidebar-toggle command.
;;
;; Link: https://github.com/jojojames/dired-sidebar
(use-package dired-sidebar
  :commands (dired-sidebar-toggle-sidebar)
  :bind ("C-x C-n" . dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
	    (lambda ()
	      (unless (file-remote-p default-directory)
		(auto-revert-mode))))

  (add-hook 'dired-sidebar-mode-hook (lambda () (linum-mode -1)))

  ;; (defun sidebar-toggle ()
    ;; Toggle both `dired-sidebar' and `ibuffer-sidebar'."
    ;;(interactive)
    ;; (ibuffer-sidebar-toggle-sidebar)
    ; (dired-sidebar-toggle-sidebar))

  ;; (define-key global-map (kbd "C-x C-n") 'sidebar-toggle)

  (setq dired-sidebar-subtree-line-prefix "  ")
  ;; (setq dired-sidebar-theme 'vscode)
  (setq dired-sidebar-theme 'none)
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t)
  
  :config  
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)

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
  (minimap-mode 1)
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
	("C-t" . nil))
  :init
  (add-hook 'vterm-mode-hook (lambda () (linum-mode -1)))
  :ensure t)

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

;; Link: https://github.com/tarsius/hl-todo
(use-package hl-todo
  :custom-face
  (hl-todo ((t (:inherit hl-todo :italic t))))
  :hook ((prog-mode . hl-todo-mode)
         (yaml-mode . hl-todo-mode))
  :ensure t)

;; A very simple but effective thing, eldoc-mode is a MinorMode which shows
;; you, in the echo area, the argument list of the function call you are currently
;; writing. Very handy. By NoahFriedman. Part of Emacs.
;; 
;; Link: https://www.emacswiki.org/emacs/ElDoc
(use-package eldoc
  :commands (eldoc-mode)
  :ensure t)

;; Note: The hook to ansible-mode might need to be removed when working on non-ansible projects
;;       This was needed to enable ls-ansible within lsp.  Otherwise only yamlls server starts.
(use-package yaml
  :hook (yaml-mode . ansible-mode)
  :mode ("\\.yml\\'" . yaml-mode)
  :interpreter ("yaml" . yaml-mode)
  :ensure t)				

;; Ansible minor mode designed to be used for modifying Ansible files.
;; 
;; Requirement
;; * yasnippet
;; *auto-complete
;;
;; Link: https://github.com/k1LoW/emacs-ansible
(use-package ansible
  :after yaml
  :hook (ansible-mode . lsp-deferred)
  :interpreter ("ansible" . ansible-mode)
  :ensure t)

;; Emacs integration for Docker. -- Supports docker containers,
;; images, volumes, networks and docker-compose.
;;
;; Link: https://github.com/Silex/docker.el
(use-package docker
  :commands (docker)
  :bind ("C-c d" . docker)
  :ensure t)

;; A Dockerfile mode for emacs
;;
;; Link: https://github.com/spotify/dockerfile-mode
(use-package dockerfile-mode
  :commands (dockerfile-mode)
  :hook
  (dockerfile-mode . lsp-deferred)
  :mode ("\\Dockerfile\\'" . dockerfile-mode)
  :interpreter ("dockerfile" . dockerfile-mode)
  :ensure t)

;; Major mode for editing docker-compose files, providing context-aware completion of docker-compose
;; keys through completion-at-point-functions.
;; 
;; Link: https://github.com/meqif/docker-compose-mode
(use-package docker-compose-mode
  :ensure t)

;; Org Mode -- A GNU Emacs major mode for keeping notes, authoring documents,
;; computational notebooks, literate programming, maintaining to-do lists,
;; planning projects, and more — in a fast and effective plain text system.
;;
;; Link: https://orgmode.org/
(use-package org
  ;; :pin manual
  ;; :load-path ("lisp/org-mode/lisp" "lisp/org-mode/lisp/contrib/lisp")
  ;; :bind
  ;; (:map org-mode-map)
  :mode ("\\.org\\'" . org-mode)
  :custom
  (org-directory "~/org")
  (org-startup-indented t)
  (org-log-done t)
  (org-log-into-drawer t)
  ;; :custom-face
  ;; :hook
  :init
  ;; Allows Org mode to run bash scrips
  (org-babel-do-load-languages 'org-babel-load-languages
			       '(
				 (shell . t)
				 )
			       )

  (setq org-confirm-babel-evaluate nil)
  :config
  (setq org-todo-keywords
	'(
          (sequence "IDEA(i)" "TODO(t)" "STARTED(s)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)")
          (sequence "|" "CANCELED(c)" "DELEGATED(l)" "SOMEDAY(f)")
          ))

  (setq org-todo-keyword-faces
	'(("IDEA" . (:foreground "GoldenRod" :weight bold))
          ("NEXT" . (:foreground "IndianRed1" :weight bold))
          ("STARTED" . (:foreground "OrangeRed" :weight bold))
          ("WAITING" . (:foreground "coral" :weight bold))
          ("CANCELED" . (:foreground "LimeGreen" :weight bold))
          ("DELEGATED" . (:foreground "LimeGreen" :weight bold))
          ("SOMEDAY" . (:foreground "LimeGreen" :weight bold))
          ))
  
  (setq org-tag-persistent-alist
	'((:startgroup . nil)
          ("HOME" . ?h)
          ("RESEARCH" . ?r)
          ("TEACHING" . ?t)
          (:endgroup . nil)
          (:startgroup . nil)
          ("OS" . ?o)
          ("DEV" . ?d)
          ("WWW" . ?w)
          (:endgroup . nil)
          (:startgroup . nil)
          ("EASY" . ?e)
          ("MEDIUM" . ?m)
          ("HARD" . ?a)
          (:endgroup . nil)
          ("UCANCODE" . ?c)
          ("URGENT" . ?u)
          ("KEY" . ?k)
          ("BONUS" . ?b)
          ("noexport" . ?x)
          )
	)

  (setq org-tag-faces
	'(
          ("HOME" . (:foreground "GoldenRod" :weight bold))
          ("RESEARCH" . (:foreground "GoldenRod" :weight bold))
          ("TEACHING" . (:foreground "GoldenRod" :weight bold))
          ("OS" . (:foreground "IndianRed1" :weight bold))
          ("DEV" . (:foreground "IndianRed1" :weight bold))
          ("WWW" . (:foreground "IndianRed1" :weight bold))
          ("URGENT" . (:foreground "Red" :weight bold))
          ("KEY" . (:foreground "Red" :weight bold))
          ("EASY" . (:foreground "OrangeRed" :weight bold))
          ("MEDIUM" . (:foreground "OrangeRed" :weight bold))
          ("HARD" . (:foreground "OrangeRed" :weight bold))
          ("BONUS" . (:foreground "GoldenRod" :weight bold))
          ("UCANCODE" . (:foreground "GoldenRod" :weight bold))
          ("noexport" . (:foreground "LimeGreen" :weight bold))
          )
	)
  :ensure t)

;; This is an implementation of dynamic virtual indentation.  It works
;; by adding text properties to a buffer to make sure lines are
;; indented according to outline structure.
;;
;; Link: https://github.com/tkf/org-mode/blob/master/lisp/org-indent.el
(use-package org-indent
  :diminish
  :custom
  (org-indent-indentation-per-level 4)
  :ensure nil)

;;;(use-package ox-gfm
;;;  :defer
;;;  :init
;;;  (require 'ox-gfm nil t)
;;;  :ensure t)

(eval-after-load "org"
  '(require 'ox-gfm nil t))

;; markdown-mode is a major mode for editing Markdown-formatted text.
;; The latest stable version is markdown-mode 2.5, released on Feb 12, 2022.
;; See the release notes for details. markdown-mode is free software, licensed
;; under the GNU GPL, version 3 or later.
;;
;; Link: https://jblevins.org/projects/markdown-mode/
(use-package markdown-mode
  :commands (markdown-mode)
  :mode ("\\.md\\'" . markdown-mode)
  :interpreter ("markdown" . markdown-mode)
  :ensure t)

;; Can be used to generate a Table of Contents within a Markdown file.
;;
;; Link: https://github.com/ardumont/markdown-toc
(use-package markdown-toc
  :init
  (require 'dash)
  (custom-set-variables '(markdown-toc-user-toc-structure-manipulation-fn
  (lambda (toc-structure)
  (-filter (lambda (l) (let ((index (car l)))
                    (<= 1 index)))
           toc-structure))))
  :ensure t)

;; Instant Github-flavored Markdown/Org preview using Grip (GitHub Readme Instant Preview).
;; https://github.com/seagle0128/grip-mode
(use-package grip-mode
  :hook ((markdown-mode org-mode) . grip-mode)
  :init
  ;; (setq grip-binary-path "/usr/bin/grip"
  (setq grip-github-user "jcook3701")
  (setq grip-github-password "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDfhI7V7AEj+xL9aUn2kD8MWmypDUjOTW9akCAOCSPDOz9voRMFgQcf+GQd/2xYDMsXGUgwns96xDLUq36ga/MDf4h7x95vOtLDcobzRP1LMqjxe/0yw19JerTYhb1oMcl7tZl63NTh+Nx/c+sPKB8j05yF/dIsbNduAOx1ZSJm9FiAoF47uRPC5PSK+7sJtnF0KuWI3a7dLNGTSDmu4ipqiks5rxh5mb50rlE5Sf5E1XeiTSzuRG8VRVDEHd1KfxEC63NCy+dtnmk6sPyqFTH7igLxSZzIQJyyb4Ou40p4VqYBEglDnTQRAKfo0H1Xknq5IaGApAI75HcQ6D3xrcVv jcook@second-chance")
  ;; (setq grip-github-password "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")
  (setq grip-update-after-change nil)
  (setq grip-preview-use-webkit nil)
  :ensure t)

;; yarn-mode is a major mode designed to be used to look at yarn.lock
;; files generated by Facebook's yarn package manager
;;
;; Link: https://github.com/anachronic/yarn-mode
(use-package yarn-mode
  :ensure t)

;; rust-mode makes editing Rust code with Emacs enjoyable. It requires Emacs 25
;; or later, and is included in both Emacs Prelude and Spacemacs by default.
;;
;; Link: https://github.com/rust-lang/rust-mode
(use-package rust-mode
  :commands (rust-mode)
  :mode ("\\.rs\\'" . rust-mode)
  :interpreter ("rust" . rust-mode)
  :ensure t)

;;;(use-package multi-web-mode
;;;  :init
;;;  (setq mweb-default-major-mode 'html-mode)
;;;  (setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
;;;                    (web-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
;;;                    (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
;;;  
;;;  (setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
;;;  
;;;  :ensure t)

;; web-mode.el is an autonomous emacs major-mode for editing web templates.
;; HTML documents can embed parts (CSS / JavaScript) and blocks (client / server side).
;;
;; Link: https://github.com/fxbois/web-mode
(use-package web-mode
  :hook (web-mode . lsp-deferred)
  :init
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.launch\\'" . web-mode))

  (add-hook 'web-mode-hook	    
            (lambda ()
              (when (string-equal "jsx" (file-name-extension buffer-file-name))
		(setup-tide-mode))))

  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
		(setup-tide-mode))))

  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-enable-current-element-highlight t)

  (setq web-mode-attr-indent-offset 2)       ;; html - Fixed indentation
  (setq web-mode-attr-value-indent-offset 2) ;; 
  (setq web-mode-markup-indent-offset 2)     ;; html
  (setq web-mode-css-indent-offset 2)        ;; css
  (setq web-mode-code-indent-offset 2)       ;; js/jsx
  (setq web-mode-sql-indent-offset 2)        ;; sql
  (setq web-mode-indent-style 2)

  ;; css color codes should show as the colors that they represent in emacs
  (setq web-mode-enable-css-colorization t)

  (setq web-mode-enable-block-face t)
  (setq web-mode-enable-part-face t)  
  :ensure t)

;; Improved JavaScript editing mode for GNU Emacs. 
;;
;; Link: https://github.com/mooz/js2-mode
(use-package js2-mode
  ;; :init
  ;; (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  :ensure t)

;; TypeScript Interactive Development Environment for Emacs
;;
;; Link: https://github.com/ananthakumaran/tide
(use-package tide
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save))
  :ensure t)

;; typescript-mode is a major-mode for editing Typescript-files in GNU Emacs.
;;
;; Link: https://github.com/emacs-typescript/typescript.el
(use-package typescript-mode  
  :ensure t)

;; Extends the builtin js-mode to add better syntax highlighting for JSON and some
;; nice editing keybindings.
;;
;; Link: https://github.com/joshwnj/json-mode
(use-package json-mode
  :ensure t)

;; ----------------------------------- Scala Setup ----------------------------------
;; The mode intends to provide basic emacs support for the Scala language, including:
;; * local indenting of code, comments and multi-line strings
;; * motion commands
;; * highlighting
;;
;; Link: https://github.com/hvesalai/emacs-scala-mode
(use-package scala-mode
  :interpreter
  ("scala" . scala-mode)
  :ensure t)

;; This mode provides basic functionality required for successfully interacting
;; with sbt inside emacs. The core functionality includes:
;; * interacting with sbt shell and scala console
;; * compiling code and navigating to errors
;;
;; Link: https://github.com/hvesalai/emacs-sbt-mode
(use-package sbt-mode
  :commands (sbt-start sbt-command)
  :config
  ;; WORKAROUND: allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false"))
  :ensure t)

;; Link: https://github.com/flymake/emacs-flymake
(use-package flymake
  :config
  ;; This lets me say where my temp dir is.
  (setq temporary-file-directory "~/.emacs.d/tmp/")
  ;; I want to see all errors for the line.
  (setq flymake-number-of-errors-to-display nil)
  :ensure t)

;; Link: https://github.com/purcell/flymake-flycheck
(use-package flymake-flycheck
  :hook (flymake-mode . flymake-flycheck-auto)
  :ensure t)

;; Link: https://github.com/jamescherti/flymake-ansible-lint.el
(use-package flymake-ansible-lint
  :commands flymake-ansible-lint-setup
  :hook (((yaml-ts-mode yaml-mode) . flymake-ansible-lint-setup)
         ((yaml-ts-mode yaml-mode) . flymake-mode))
  :ensure t)

;; Link: https://codeberg.org/shaohme/flymake-markdownlint
(use-package flymake-markdownlint
  :hook (markdown-mode . flymake-markdownlint-setup)
  :ensure t)

;; Link: https://github.com/orzechowskid/flymake-eslint
(use-package flymake-eslint
  :init
  (add-hook 'web-mode-hook ; or whatever the mode-hook is for your mode of choice
	    (lambda ()
	      (flymake-eslint-enable)))
  :ensure t)

;; Link: https://codeberg.org/shaohme/flymake-yamllint
(use-package flymake-yamllint
  :hook (yaml-mode . flymake-yamllint-setup)
  :ensure t)

;; Enable nice rendering of diagnostics like compile errors.
;;
;; Link: https://www.flycheck.org/en/latest/
(use-package flycheck
  :init (global-flycheck-mode)
  :ensure t)

;; This package provides a flycheck checker for the C, C++ and Objective-C languages.
;;
;; Link: https://github.com/Sarcasm/flycheck-irony
(use-package flycheck-irony
  :init
  (eval-after-load 'flycheck
    '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
  :ensure t)

;; This Flycheck extension configures Flycheck automatically for the current Cargo project.
;;
;; Link: https://github.com/flycheck/flycheck-rust
(use-package flycheck-rust
  :init
  (with-eval-after-load 'rust-mode
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
  :ensure t)

;; Error Highlighting/Navigation
;;
;; Link: https://github.com/Andersbakken/rtags
(use-package flycheck-rtags
  :init
  (defun my-flycheck-rtags-setup ()
    (flycheck-select-checker 'rtags)
    (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
    (setq-local flycheck-check-syntax-automatically nil))  
  :hook ((c-mode . my-flycheck-rtags-setup)
	 (c++-mode . my-flycheck-rtags-setup)
	 (objc-mode . my-flycheck-rtags-setup))
  :ensure t)

;; “Flyspell enables on-the-fly spell checking in Emacs by the means of a minor mode.
;; It is called Flyspell. This facility is hardly intrusive. It requires no help.
;; Flyspell highlights incorrect words as soon as they are completed or as soon as
;; the TextCursor hits a new word.”
;;
;; Link: https://www.emacswiki.org/emacs/FlySpell
(use-package flyspell
  :init
  (require 'ispell)
  (defun turn-on-flyspell()
    (flyspell-mode 1))

  (setq ispell-dictionary "en_US")
  
  (mapcar (lambda (mode-hook) (add-hook mode-hook 'turn-on-flyspell))
	  '(markdown-mode-hook text-mode-hook))
  
  (mapcar (lambda (mode-hook) (add-hook mode-hook 'flyspell-prog-mode))
	  '(c-mode-common-hook python-mode-hook emacs-lisp-mode-hook html-mode-hook js-mode-hook))
  :bind
  (:map flyspell-mode-map
	("C-;" . nil))
  :ensure t)

;; Client for Language Server Protocol (v3.14). lsp-mode aims to provide IDE-like experience by
;; providing optional integration with the most popular Emacs packages like company, flycheck
;; and projectile
;; 
;; Link: https://github.com/emacs-lsp/lsp-mode
(use-package lsp-mode
  :after company
  ;; Optional - enable lsp-mode automatically in scala files
  :commands(lsp lsp-deferred)
  :hook (lsp-mode . lsp-lens-mode)
  :config
  ;; Uncomment following section if you would like to tune lsp-mode performance according to
  ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
  ;;       (setq gc-cons-threshold 100000000) ;; 100mb
  ;;       (setq read-process-output-max (* 1024 1024)) ;; 1mb
  ;;       (setq lsp-idle-delay 0.500)
  ;;       (setq lsp-log-io nil)
  ;;       (setq lsp-completion-provider :capf)

  ;; (lsp-register-custom-settings
  ;; '(("pyls.plugins.pyls_mypy.enabled" t t)
  ;; ("pyls.plugins.pyls_mypy.live_mode" nil t)
  ;; ("pyls.plugins.pyls_black.enabled" t t)
  ;; ("pyls.plugins.pyls_isort.enabled" t t)
  ;; ("pyls.plugins.rope_completion.enabled" t t)
  ;; ("pyls.plugins.yapf.enabled." t t)))
  
  (setq lsp-prefer-flymake nil)
  :ensure t)

(use-package dap-mode
  :ensure t)

;; Add metals backend for lsp-mode
;; Emacs Scala IDE using lsp-mode to connect to Metals.
;;
;; Link: https://github.com/emacs-lsp/lsp-metals
(use-package lsp-metals
  :hook (scala-mode . lsp)
  :ensure t)

;; This package contains all the higher level UI modules of lsp-mode, like flycheck support and code lenses.
;; Enable nice rendering of documentation on hover
;; Warning: on some systems this package can reduce your emacs responsiveness significally.
;; (See: https://emacs-lsp.github.io/lsp-mode/page/performance/)
;; In that case you have to not only disable this but also remove from the packages since
;; lsp-mode can activate it automatically.
;;
;; Link: https://github.com/emacs-lsp/lsp-ui
(use-package lsp-ui
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-sideline-show-hover t
                lsp-ui-sideline-delay 0.5
                lsp-ui-doc-delay 5
                lsp-ui-sideline-ignore-duplicates t
                lsp-ui-doc-position 'bottom
                lsp-ui-doc-alignment 'frame
                lsp-ui-doc-header nil
                lsp-ui-doc-include-signature t
                lsp-ui-doc-use-childframe t)
  :ensure t)

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

;; To Company-lsp users:
;; Company-lsp is no longer maintained and has been removed from MELPA.
;; Please migrate to company-capf.
;;
;; Link:
(use-package company
  :hook ((scala-mode . company-mode)
	 (yaml-mode . company-mode))
  
  :init
  ;; Company Package
  (add-hook 'after-init-hook 'global-company-mode)
  
  :config
  (setq lsp-completion-provider :capf)
  ;; changed hot keys to scroll through elpy jedi configuration which uses
  ;; company under the hood.
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
   ;;  Already defined however I am putting this here for my reference.
   ;; (define-key company-active-map (kbd "C-d") ')  ;; display a temporary window with documentation.
   ;; (define-key company-active-map (kbd "C-w") ')  ;; will display a temporary window showing the
                                                  ;; source code of the completion to get some context. 
  :ensure t)

;;;cmake-ide
;;;(setq cmake-ide-build-dir "~/.emacs.d/cmake-ide-build-dir")
;;;(setq cmake-ide-build-pool-dir "~/.emacs.d/cmake-ide-build-dir")
;;;(setq cmake-ide-build-pool-use-persistent-naming t)
;;;(require 'rtags)
;;;(require 'company)
;;;(cmake-ide-setup)

;;; Rtags Package
;;;(add-hook 'c-mode-hook 'rtags-start-process-unless-running)
;;;(add-hook 'c++-mode-hook 'rtags-start-process-unless-running)
;;;(add-hook 'objc-mode-hook 'rtags-start-process-unless-running)
;;;(setq company-backends '(company-rtags))

;; Provides syntax highlighting and indentation for CMakeLists.txt and
;; *.cmake source files.
;;
;; Link: https://github.com/Kitware/CMake/blob/master/Auxiliary/cmake-mode.el
(use-package cmake-mode
  :mode(("CMakeLists\\.txt\\'" . cmake-mode)
	("\\.cmake\\'" . cmake-mode))
  :interpreter ("cmake" . cmake-mode)
  :ensure t)

;; This package implements CSV mode, a major mode for editing records
;; in a generalized CSV (character-separated values) format.  It binds
;; files with prefix ".csv" to `csv-mode' (and ".tsv" to `tsv-mode') in
;; `auto-mode-alist'.
;;
;; Link: https://github.com/emacsmirror/emacswiki.org/blob/master/csv-mode.el
(use-package csv-mode
  :commands (csv-mode)
  :mode ("\\.csv\\'" . csv-mode)
  :interpreter ("csv" . csv-mode)
  :ensure t)

;; Django project management package with the goodies you would expect and
;; then some. The project buffer workings is pretty much inspired by the good
;; ol' magit-status buffer.
;;
;; Link: https://code.djangoproject.com/wiki/Emacs
(use-package django-mode
  :ensure t)

;; `gcode-mode' performs basic syntax highlighting on G-Code files
;; (mostly aimed at 3D printers), also providing optional instruction
;; lookup with ElDoc.
;;
;; Link: https://gitlab.com/wavexx/gcode-mode.el/tree/1f83845af4102efc5e5856b55bd5ad165b2f0cdd
(use-package gcode-mode
  :commands (gcode-mode)
  :hook((gcode-mode . eldoc-mode)
	(gcode-mode . lsp-deferred))
  :config
  (add-to-list 'auto-mode-alist '("\\.gcode\\'" . gcode-mode))
  :ensure t)

;; go-mode covers the basic features you need for working with the Go code
;; but adds some extended things like imports managing or interacting with
;; play.golang.org. You may also find useful to add the additional features noted below.
;;
;; Link: https://github.com/dominikh/go-mode.el
(use-package go-mode
  :commands (go-mode)
  :hook
  (go-mode . lsp-deferred)
  :config
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
  :ensure t)

;; This is npm-mode, an Emacs minor mode for working with NPM projects.
;;
;; Link: https://github.com/mojochao/npm-mode
(use-package npm-mode
  :ensure t)

;; A major mode for editing sed source code.
;;
;; Link: https://github.com/eschulte/sed-mode
(use-package sed-mode
  :ensure t)

;; Verilog-Mode supports syntax highlighting of SystemVerilog
;; (IEEE 1800-2017), Verilog (IEEE 1364-2005), and the Universal
;; Verification Modeling language (UVM). Verilog-Mode also has
;; AUTOs which greatly accelerate maintaining interconnect, resets,
;; and other boiler-plate code.
;;
;; Link: https://github.com/veripool/verilog-mode
(use-package verilog-mode
  :commands (verilog-mode)
  :mode (("\\.v\\'" . verilog-mode)
	 ("\\.vh\\'" . verilog-mode))
  :interpreter ("verilog" . verilog-mode)
  :ensure t)

;; Code completion
;;
;; Link: https://github.com/Andersbakken/rtags
(use-package company-rtags
  :init
  (setq rtags-autostart-diagnostics t)
  (rtags-diagnostics)
  (setq rtags-completions-enabled t)
  ;;      rtags-path "/home/jcook/.emacs.d/elpa/rtags-20201008.1707/rtags.el"
  ;;      rtags-rc-binary-name "/home/jcook/.emacs.d/elpa/rtags-20201008.1707/rtags-2.38/bin/rc"
  ;;      rtags-use-helm t
  ;;      rtags-rdm-binary-name "/home/jcook/.emacs.d/elpa/rtags-20201008.1707/rtags-2.38/bin/rdm")
  :config
  (push 'company-rtags company-backends)
  (global-company-mode)
  (define-key c-mode-base-map (kbd "<C-tab>") (function company-complete))
  :ensure t)


;; ------- Python Packages for Emacs ------- ;;
;;; virtualenvwrapper
;;;(require 'virtualenvwrapper)
;;;(setq venv-location "~/Documents/College/5th_year/seniorProject/gitHub/supreme_bot")

;; The built-in ‘python-mode’ supports, from its help, “Syntax highlighting,
;; Indentation, Movement, Shell interaction, Shell completion, Shell virtualenv
;; support, Shell package support, Shell syntax highlighting, Pdb tracking,
;; Symbol completion, Skeletons, FFAP, Code Check, ElDoc, Imenu.
;;
;; Link: https://www.emacswiki.org/emacs/PythonProgrammingInEmacs
(use-package python
  :commands (python-mode)
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :config
  (setenv "FREECAD_MOD" "/usr/share/freecad/Mod/Web:/usr/share/freecad/Mod/Tux:/usr/share/freecad/Mod/Draft:/usr/share/freecad/Mod/OpenSCAD:/usr/share/freecad/Mod/Import:/usr/share/freecad/Mod/Path:/usr/share/freecad/Mod/Drawing:/usr/share/freecad/Mod/Part:/usr/share/freecad/Mod/Material:/usr/share/freecad/Mod/Points:/usr/share/freecad/Mod/Test:/usr/share/freecad/Mod/Arch:/usr/share/freecad/Mod/Image:/usr/share/freecad/Mod/Robot:/usr/share/freecad/Mod/AddonManager:/usr/share/freecad/Mod/Start:/usr/share/freecad/Mod/Inspection:/usr/share/freecad/Mod/PartDesign:/usr/share/freecad/Mod/ReverseEngineering:/usr/share/freecad/Mod/Fem:/usr/share/freecad/Mod/Surface:/usr/share/freecad/Mod/Sketcher:/usr/share/freecad/Mod/Measure:/usr/share/freecad/Mod/TechDraw:/usr/share/freecad/Mod/Show:/usr/share/freecad/Mod/Spreadsheet:/usr/share/freecad/Mod/Raytracing:/usr/share/freecad/Mod/MeshPart:/usr/share/freecad/Mod/Mesh:/usr/share/freecad/Mod/Idf:/usr/share/freecad/Mod:/usr/lib/freecad/Mod")
  (setenv "FREECAD_LIB" "/usr/lib/freecad/lib:/usr/lib/freecad-python3/lib")
  (setenv "FREECAD_EXT" "/usr/lib/freecad/Ext")
  (setenv "FREECAD_BIN" "/usr/lib/freecad/bin")
  (setenv "FREECAD_MACRO" "/home/jcook/.FreeCAD/Macro:/usr/lib/freecad/Macro")
  ;; (setenv "FREECAD_STUBS" "/home/jcook/Documents/git_repo/freecad-stubs/out")
  (setenv "ROS_LIB" "/opt/ros/melodic/lib/python2.7/dist-packages")
  (setenv "PYTHONPATH" (concat (getenv "FREECAD_MOD")
			       ":"
			       (getenv "FREECAD_LIB")
			       ":"
			       (getenv "FREECAD_EXT")
			       ":"
			       ;;(getenv "FREECAD_STUBS")
			       ;;":"
			       (getenv "FREECAD_BIN")
			       ":"
			       (getenv "FREECAD_MACRO")
			       ":"
			       (getenv "ROS_LIB")
			       ":"
			       (getenv "PYTHONPATH")))
  ;; (define-key python-mode-map (kbd "C-c C-c") 'python-shell-r)
  (require 'dap-mode)
  :ensure t)

(use-package pyvenv
  :demand t
  :config
  ;; (setenv "JAVA_HOME" "~/Documents/python_virtual_envs/nodejs/")
  (setenv "WORKON_HOME" "~/Documents/python_virtual_envs/python3")
  
  (pyvenv-tracking-mode 1)
  (setq pyvenv-mode-line-indicator '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] ")))
  (pyvenv-mode t)

  ;; Set correct Python interpreter
  (setq pyvenv-post-activate-hooks
        (list (lambda ()
                (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python3"))
		(lsp))))
  (setq pyvenv-post-deactivate-hooks
        (list (lambda ()
                (setq python-shell-interpreter "python3")
		(lsp-shutdown-workspace))))

  :ensure t)

;; lsp-jedi
;; Helpful Link: https://www.mattduck.com/lsp-python-getting-started.html
;;
;; Link: https://github.com/fredcamps/lsp-jedi
;; Link: https://github.com/python-lsp/python-lsp-server
;;(use-package lsp-jedi
;;  :config
;;  (with-eval-after-load "lsp-mode"
;;    (add-to-list 'lsp-disabled-clients 'pyls)
;;    (add-to-list 'lsp-enabled-clients 'jedi))
  ;; (add-to-list 'lsp-enabled-clients 'pyls))
  ;; (add-to-list 'lsp-enabled-clients 'pylsp)
  ;; (add-to-list 'lsp-enabled-clients 'rope)
  ;; (add-to-list 'lsp-enabled-clients 'yapf))

;;  ;; Custom path to system FreeCAD libraries. 
;;  (setq lsp-jedi-workspace-extra-paths
;;	(vconcat lsp-jedi-workspace-extra-paths
;;		 ["/usr/lib/freecad-python3/lib"]))
  
;;  :ensure t)


;; importmagic

(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)))
  ;; (lsp)))  ; or lsp-deferre
  :config
  (setq lsp-pyright-python-executable-cmd "python3")
  (setq lsp-pyright-log-level "trace")
  (setq lsp-pyright-stub-path "/home/jcook/.python3_stubs/FreeCAD")
  (setq lsp-pyright-extra-paths
	(vconcat lsp-pyright-extra-paths
		 ["/usr/share/freecad/Mod/Web",
		  "/usr/share/freecad/Mod/Tux",
		  "/usr/share/freecad/Mod/Draft",
		  "/usr/share/freecad/Mod/OpenSCAD",
		  "/usr/share/freecad/Mod/Import",
		  "/usr/share/freecad/Mod/Path",
		  "/usr/share/freecad/Mod/Drawing",
		  "/usr/share/freecad/Mod/Part",
		  "/usr/share/freecad/Mod/Material",
		  "/usr/share/freecad/Mod/Points",
		  "/usr/share/freecad/Mod/Test",
		  "/usr/share/freecad/Mod/Arch",
		  "/usr/share/freecad/Mod/Image",
		  "/usr/share/freecad/Mod/Robot",
		  "/usr/share/freecad/Mod/AddonManager",
		  "/usr/share/freecad/Mod/Start",
		  "/usr/share/freecad/Mod/Inspection",
		  "/usr/share/freecad/Mod/PartDesign",
		  "/usr/share/freecad/Mod/ReverseEngineering",
		  "/usr/share/freecad/Mod/Fem",
		  "/usr/share/freecad/Mod/Surface",
		  "/usr/share/freecad/Mod/Sketcher",
		  "/usr/share/freecad/Mod/Measure",
		  "/usr/share/freecad/Mod/TechDraw",
		  "/usr/share/freecad/Mod/Show",
		  "/usr/share/freecad/Mod/Spreadsheet",
		  "/usr/share/freecad/Mod/Raytracing",
		  "/usr/share/freecad/Mod/MeshPart",
		  "/usr/share/freecad/Mod/Mesh",
		  "/usr/share/freecad/Mod/Idf",
		  "/usr/share/freecad/Mod",
		  "/usr/lib/freecad/Mod",
		  "/usr/lib/freecad-python3/lib",
		  "/usr/lib/freecad/lib",
		  "/usr/lib/freecad/Ext",		  
		  "/usr/lib/freecad/bin",
		  "/home/jcook/.FreeCAD/Macro",
		  "/usr/lib/freecad/Macro"]))
  :ensure t)

;; elpy
;;; (use-package elpy
;;;    :init
;;;    (elpy-enable)
;;;   (load "elpy")
;;;   (load "elpy-rpc")
;;;   (load "elpy-shell")
;;;   (load "elpy-profile")
;;;   (load "elpy-refactor")
;;;   (load "elpy-django")
;;;   
;;;  (setq elpy-rpc-python-command "python3")
;;;   
;;;   ;; (setq elpy-rpc-virtualenv-path "current")
;;;   ;; (setq elpy-rpc-backend "jedi") ;; Deprecated
;;;
;;;   ;;-------------------------------;;
;;;   ;; Custom Setup for python shell ;;
;;;   ;;-------------------------------;;
;;;   ;;Run python and pop-up its shell.
;;;   ;; Kill process to solve the reload modules problem.
;;;   (defun my-python-shell-run ()
;;;     (interactive)
;;;     (when (get-buffer-process "*Python*")
;;;       (set-process-query-on-exit-flag (get-buffer-process "*Python*") nil)
;;;       (kill-process (get-buffer-process "*Python*"))
;;;       ;; if you want to clean the buffer too.
;;;       ;; (kill-buffer "*Python*")
;;;       ;; Not so fast!
;;;       (sleep-for 0.5))
;;;     (run-python (python-shell-parse-command) nil nil)
;;;     (elpy-shell-send-buffer t)
;;;     ;; Pop a new window only if shell isn't visible
;;;     ;; in any frame.
;;;     (unless (get-buffer-window "*Python*" t)
;;;       (elpy-shell-switch-to-shell)))
;;;  
;;;   (defun my-python-shell-run-region ()
;;;     (interactive)
;;;     (python-shell-send-region (region-beginning) (region-end))
;;;     (python-shell-switch-to-shell))
;;;  
;;;   (eval-after-load 'elpy
;;;     `(progn
;;;	(message "python cfg has finished loading")
;;;	(define-key elpy-mode-map (kbd "C-c C-c") 'my-python-shell-run)
;;;	(define-key elpy-mode-map (kbd "C-c C-r") 'my-python-shell-run-region)
;;;	(define-key elpy-mode-map (kbd "C-c f") 'python-eldoc-at-point)))
;;;   :ensure t)

  
;; jedi Package - This is outdated
;;;(add-hook 'python-mode-hook 'jedi:setup)
;;;(setq jedi:complete-on-dot t)     

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
  :init
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
  :ensure t)

;; A call to helm-make will give you a helm selection of this directory
;; Makefile's targets. Selecting a target will call compile on it. You
;; can cancel as usual with C-g. Support is provided for the various
;; flavors of Make tools, as well as the Ninja build tool.
;;
;; Link: https://github.com/abo-abo/helm-make
(use-package helm-make
  :ensure t)

;; This package integrates the catkin build tool for ROS packages into Emacs. With it you can:
;; * Build one, multiple or all packages in the workspace
;; * Setup, initialize and clean the workspace
;; * Configure cmake, make and catkin_make arguments
;; * Blacklist or whitelist packages in the workspace
;;
;; Link: https://github.com/gollth/helm-catkin
(use-package helm-catkin
  :ensure t)

;; Helm Interface for Chrome bookmarks.
;;
;; Link: https://github.com/kawabata/helm-chrome
(use-package helm-chrome
  :ensure t)

;; Browse your Chrome history with Helm.
;;
;; Link: https://github.com/xuchunyang/helm-chrome-history
(use-package helm-chrome-history
  :ensure t)

;; helm interface for codesearch
;;
;; Link: https://github.com/youngker/helm-codesearch.el
(use-package helm-codesearch
  :ensure t)

;;;(use-package helm-ros
;;;  :ensure t)

;; This package lets you Start/Restart/Stop and view status of systemd’s
;; units with helm.
;;
;; Link: https://github.com/Lompik/helm-systemd
(use-package helm-systemd
  :ensure t)

;; helm-themes.el provides Emacs themes selection with helm interface.
;;
;; Link: https://github.com/emacsorphanage/helm-themes
(use-package helm-themes
  :ensure t)

;;;(use-package helm-google-helm
;;;  :ensure t)

;; This implements eldoc support in irony-mode. eldoc is a built-in Emacs
;; mode for displaying documentation about a symbol or function call at
;; point in the message buffer (see eldoc-mode).
;;
;; Link: https://github.com/ikirill/irony-eldoc
(use-package irony-eldoc
  :init
  (add-hook 'irony-mode-hook #'irony-eldoc)
  :ensure t)

;; Company Irony
;; irony-mode is an Emacs minor-mode that aims at improving the editing
;; experience for the C, C++ and Objective-C languages. It works by using
;; a combination of an Emacs package and a C++ program (irony-server)
;; exposing libclang.
;;
;; Link: https://github.com/Sarcasm/irony-mode
(use-package irony
  :init
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
  
  :ensure t)

;;(setq company-backends (delete 'company-semantic company-backends))

;; This package provides a company-mode backend for C/C++ header files
;; that works with irony-mode. This package is meant to be complementary
;; to company-irony by offering completion suggestions to header files.
;;
;; Link: https://github.com/hotpxl/company-irony-c-headers
(use-package company-irony-c-headers
  :init
  (eval-after-load 'company
    '(add-to-list 'company-backends '(company-irony-c-headers company-irony)))
  :ensure t)

;; ROS Emacs
;;; (add-to-list 'load-path' "/opt/ros/melodic/share/emacs/site-lisp")
;;; (require 'rosemacs-config)
;;; (require 'helm-ros)

;; ----------------------------------- Useful Tools -----------------------------------

;; emacs-slack is a Slack client for emacs
;;
;; Link: https://github.com/yuya373/emacs-slack
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
  :init
  (add-hook 'pdf-view-mode-hook (lambda () (linum-mode -1)))
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
  :init
  ;; (setq google-this-keybind (kbd "C-x g"))
  (google-this-mode 1)
  :ensure t)

;; TRAMP (Transparent Remote Access, Multiple Protocols) is a package for
;; editing remote files, similar to AngeFtp or efs. Whereas the others use
;; FTP to connect to the remote host and to transfer the files, TRAMP uses
;; a remote shell connection (rlogin, telnet, ssh). It can transfer the
;; files using rcp or a similar program, or it can encode the file contents
;; (using uuencode or base64) and transfer them right through the shell connection.
;;
;; Link: https://www.emacswiki.org/emacs/TrampMode
(use-package tramp
  :init
  (setq tramp-default-method "ssh")
  :ensure t)

;; Magit - Magit is an interface to the version control system Git,
;; implemented as an Emacs package. Magit aspires to be a complete
;; Git porcelain. While we cannot (yet) claim that Magit wraps and
;; improves upon each and every Git command, it is complete enough
;; to allow even experienced Git users to perform almost all of their
;; daily version control tasks directly from within Emacs. While many
;; fine Git clients exist, only Magit and Git itself deserve to be called
;; porcelains.
;; 
;; Link: https://github.com/magit/magit
(use-package magit
  :defer
  ;; :init
  (setq auth-sources '("~/.authinfo"))
  :ensure t)

;; Work with Git forges, such as Github and Gitlab, from the comfort of Magit and the rest of Emacs.
;;
;; Link: https://github.com/magit/forge
(use-package forge
  :after magit
  ;; :init
  ;; (ghub-request 
  :ensure t)

;; ----------------------------------- Games/Easter Eggs ----------------------------------

;; Displays an animated train
;;
;; Link: https://www.emacswiki.org/emacs/sl.el
(use-package sl
  :ensure t)

;; A complete chess client written in elisp
;;
;; Link: https://github.com/jwiegley/emacs-chess
(use-package chess
  :ensure t)

;; Pacman for Emacs
;;
;; Link: https://github.com/codingteam/pacmacs.el
(use-package pacmacs
  :ensure t)

;;;;; Live Testing for Emacs run on XQartz
;;;;; Version: Emacs 25.3
;;;;; Built with:
;;;;;
;;;;;     ./autogen.sh all
;;;;;     ./configure --with-xwidgets --without-ns --with-gnutls --with-imagemagick --without-dbus --with-x
;;;;;     make install
;;;;;
;;;;; This is actively running on Mac OS High Sierra
;;;;; Version: 10.13.6
;;;
;;;;; make these keys behave like normal browser
;;;(define-key xwidget-webkit-mode-map [mouse-4] 'xwidget-webkit-scroll-down)
;;;(define-key xwidget-webkit-mode-map [mouse-5] 'xwidget-webkit-scroll-up)
;;;(define-key xwidget-webkit-mode-map (kbd "<up>") 'xwidget-webkit-scroll-down)
;;;(define-key xwidget-webkit-mode-map (kbd "<down>") 'xwidget-webkit-scroll-up)
;;;(define-key xwidget-webkit-mode-map (kbd "M-w") 'xwidget-webkit-copy-selection-as-kill)
;;;(define-key xwidget-webkit-mode-map (kbd "C-c") 'xwidget-webkit-copy-selection-as-kill)
;;;
;;;;; adapt webkit according to window configuration chagne automatically
;;;;; without this hook, every time you change your window configuration,
;;;;; you must press 'a' to adapt webkit content to new window size
;;;(add-hook 'window-configuration-change-hook (lambda ()
;;;               (when (equal major-mode 'xwidget-webkit-mode)
;;;                 (xwidget-webkit-adjust-size-dispatch))))
;;;
;;;;; by default, xwidget reuses previous xwidget window,
;;;;; thus overriding your current website, unless a prefix argument
;;;;; is supplied
;;;
;;;;; This function always opens a new website in a new window
;;;(defun xwidget-browse-url-no-reuse (url &optional sessoin)
;;;  (interactive (progn
;;;                 (require 'browse-url)
;;;                 (browse-url-interactive-arg "xwidget-webkit URL: "
;;;                                             )))
;;;  (xwidget-webkit-browse-url url t))
;;;
;;;;; make xwidget default browser
;;;(setq browse-url-browser-function (lambda (url session)
;;;                    (other-window 1)
;;;                    (xwidget-browse-url-no-reuse url)))
;;;
;;;(put 'erase-buffer 'disabled nil) 
;;; ----------------------------------- init.el ends here ----------------------------------- ;;;

