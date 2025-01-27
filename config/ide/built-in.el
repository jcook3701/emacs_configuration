;;; built-in.el --- Summary
;;; Commentary:
;; Emacs built-in packages configurations
;;----------------------------------------------------------------------------------------------
;;; Code:

(use-package auth-source
  :ensure nil ; Built-in, so no need to install it
  :config
  ;; Your custom configuration for auth-source goes here
  (setq auth-sources '("~/.authinfo.gpg")))

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
  :ensure nil
  :bind
  (:map dired-mode-map
	("i" . nil))
;;  :hook (dired-mode . auto-revert-mode)  ;; Need to test this...
  :init
  (customize-set-value
   'auto-revert-verbose
   nil
   "Prevent any auto-revert messages from obscuring the minibuffer at crucial times!"))

;; Ibuffer is an advanced replacement for BufferMenu, which lets you operate
;; on buffers much in the same manner as Dired. The most important Ibuffer
;; features are highlighting and various alternate layouts. Ibuffer is part of
;; Emacs since version 22.
;;
;; Link: https://www.emacswiki.org/emacs/IbufferMode
(use-package ibuffer
  :ensure nil
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
  :ensure nil
  :defer t
  :config
  (setq tramp-verbose 1)
  (setq tramp-default-method "ssh"))

;; tree-sitter is an Emacs binding for Tree-sitter, an incremental parsing
;; system. It aims to be the foundation for a new breed of Emacs packages
;; that understand code structurally.
;;
;; Link: https://www.masteringemacs.org/article/how-to-get-started-tree-sitter
(use-package treesit
  :ensure nil
  :init
  (setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
  
  :config
  (setq treesit-debug t)
  (message "Tree-sitter available: %s" (treesit-available-p))

  ;; (setq treesit-extra-load-path '("/usr/local/lib"))
  ;; (global-tree-sitter-mode)
  ;; (require 'tree-sitter-langs)
;;;   (defvar python--treesit-settings
;;;    (treesit-font-lock-rules
;;;     :feature 'comment
;;;     :language 'python
;;;     '((comment) @font-lock-comment-face)
   
;;;     :feature 'string
;;;     :language 'python
;;;     '((string) @font-lock-string-face
;;;       (string) @contextual) ; Contextual special treatment.
     
;;;     :feature 'function-name
;;;     :language 'python
;;;     '((function_definition
;;;	name: (identifier) @font-lock-function-name-face))
     
;;;     :feature 'class-name
;;;     :language 'python
;;;     '((class_definition
;;;	name: (identifier) @font-lock-type-face))
;;;     ))
  )

;; “Flyspell enables on-the-fly spell checking in Emacs by the means of a minor mode.
;; It is called Flyspell. This facility is hardly intrusive. It requires no help.
;; Flyspell highlights incorrect words as soon as they are completed or as soon as
;; the TextCursor hits a new word.”
;;
;; Link: https://www.emacswiki.org/emacs/FlySpell
(use-package flyspell
  :ensure nil
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

;;; built-in.el ends here
