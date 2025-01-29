;;; dired.el --- Summary
;;; Commentary:
;; Emacs Dired package configurations
;;----------------------------------------------------------------------------------------------
;;; Code:

;; Dired makes an Emacs buffer containing a listing of a directory, and
;; optionally some of its subdirectories as well. You can use the normal
;; Emacs commands to move around in this buffer, and special Dired commands
;; to operate on the listed files. Dired works with both local and remote
;; directories.
;;
;; Helpful: https://emacs.stackexchange.com/questions/34567/dired-not-showing-recently-created-files-when-emacs-is-run-in-daemon-mode
;; 
;; Link: https://www.gnu.org/software/emacs/manual/html_node/emacs/Dired.html
(use-package dired
  :ensure nil ;; 'dired' is built-in
  :bind
  (:map dired-mode-map
	("i" . nil))
;;  :hook (dired-mode . auto-revert-mode)  ;; Need to test this...
  :config
  ;; Customize dired to show human-readable sizes and to auto-revert
  (setq dired-listing-switches "-alh"
        dired-dwim-target t
        dired-hide-details-hide-symlink-targets nil)
  
  (customize-set-value
   'auto-revert-verbose
   nil
   "Prevent any auto-revert messages from obscuring the minibuffer at crucial times!"))

;; File Browser - This loads after the ibuffer-sidebar to ensure that the command
;; 'ibuffer-sidebar-toggle-sidebar is available when called from the
;; '+sidebar-toggle command.
;;
;; Link: https://github.com/jojojames/dired-sidebar
(use-package dired-sidebar
  :after dired
  :commands (dired-sidebar-toggle-sidebar)
  :bind ("C-x C-n" . dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
	    (lambda ()
	      (unless (file-remote-p default-directory)
		(auto-revert-mode))))
  
  ;; (defun sidebar-toggle ()
    ;; Toggle both `dired-sidebar' and `ibuffer-sidebar'."
    ;;(interactive)
    ;; (ibuffer-sidebar-toggle-sidebar)
    ;; (dired-sidebar-toggle-sidebar))

  ;; (define-key global-map (kbd "C-x C-n") 'sidebar-toggle)
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)

  (setq dired-sidebar-subtree-line-prefix "  ")
  ;; (setq dired-sidebar-theme 'vscode)
  (setq dired-sidebar-theme 'none)
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t)
  :ensure t)

;; dired-k.el highlights dired buffer like k.
;;
;; Link: https://github.com/emacsorphanage/dired-k
(use-package dired-k
  :after dired
  :commands dired-k
  :bind
  (:map dired-mode-map
	("K" . dired-k))
  :hook ((dired-initial-position . dired-k)
	 (dired-after-readin . dired-k-no-revert))
  :init
  ;; (setq dired-listing-switches "-laGh1v") ;; --group-directories-first
  (setq dired-k-human-readable t)
  (setq dired-k-style "k.zsh")
  :ensure t)

;; This package implements useful features present in the ranger file manager which
;; are missing in dired.
;;
;; Link: https://github.com/Fuco1/dired-hacks
(use-package dired-ranger
  :after dired
  :ensure t)

;; This package adds more customizable highlighting for files in dired listings.
;; The group dired-faces provides only nine faces and isn't very fine-grained.
;;
;; Link: https://github.com/Fuco1/dired-hacks
(use-package dired-rainbow
  :after dired
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
;; NOTE: causes issues with dired sidebar
;; Link: https://github.com/Fuco1/dired-hacks
;;; (use-package dired-collapse
;;;  :after dired
;;;  :hook ((dired-mode . dired-collapse-mode))
;;;  (dired-sidebar-mode . dired-collapse-mode))
;;;  :ensure t)

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
  :after dired
  :commands dired-subtree-insert dired-subtre-remove dired-subtree-toggle
  :bind
  (:map dired-mode-map
	("i" . dired-subtree-toggle))
  :config
  ;; Customize appearance of subtree indentations
  (setq dired-subtree-line-prefix "  ")
  (setq dired-subtree-use-backgrounds nil)
  :ensure t)

;;; dired.el ends here
