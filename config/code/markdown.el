;;; markdown.el --- Summary
;;; Commentary:
;; Emacs markdown packages configurations
;;----------------------------------------------------------------------------------------------
;;; Code:

;; markdown-mode is a major mode for editing Markdown-formatted text.
;; The latest stable version is markdown-mode 2.5, released on Feb 12, 2022.
;; See the release notes for details. markdown-mode is free software, licensed
;; under the GNU GPL, version 3 or later.
;;
;; Link: https://jblevins.org/projects/markdown-mode/
(use-package markdown-mode
  :commands (gfm-mode)
  :mode (("\\.md\\'"  . gfm-mode)
	 ("\\.markdown\\'" . gfm-mode))
  :hook ((gfm-mode . lsp-deferred)
	 (gfm-mode . visual-line-mode)
	 (gfm-mode . writegood-mode))
;;; :interpreter ("markdown" . gfm-mode)
  :init
  ;; (setq markdown-command "pandoc")    ; Use pandoc for Markdown processing if installed
  :config
  ;; 
;;;  (advice-add 'markdown-preview :around
;;;              (lambda (orig-fun &rest args)
;;;		(let ((default-directory temporary-file-directory))
;;;                  (apply orig-fun args))))

  ;; Enable syntax highlighting for code blocks
  (setq markdown-fontify-code-blocks-natively t)
  :ensure t)

;; NOTE: I'm really not using this right now.
;; Link: https://github.com/polymode/poly-markdown?tab=readme-ov-file
(use-package poly-markdown
  :ensure t)

;; Can be used to generate a Table of Contents within a Markdown file.
;;
;; Link: https://github.com/ardumont/markdown-toc
(use-package markdown-toc
  :after dash
  :commands markdown-toc-generate-toc
  :config
  (custom-set-variables '(markdown-toc-user-toc-structure-manipulation-fn
        (lambda (toc-structure)
	  (-filter (lambda (l) (let ((index (car l)))
				 (<= 1 index)))
		   toc-structure))))
  :ensure t)

;; Instant Github-flavored Markdown/Org preview using Grip (GitHub Readme Instant Preview).
;;
;; https://github.com/seagle0128/grip-mode
 
;;;(use-package grip-mode
;;;  :after auth-source
;;;  :hook ((gfm-mode org-mode) . grip-mode)
;;;  :custom
;;;  (grip-use-mdopen t) ;; to use `mdopen` instead of `grip`
;;;  ;;  (grip-mdopen-path "~/.emacs.d/mdopen-wrapper")
;;;  (grip--port nil)
;;;  ;; (setq grip-preview-host "localhost") ;; Preview hostname
;;;  (grip-update-after-change nil)  ;; after every text change
;;;  (grip-preview-use-webkit t)   ;; Use embedded webkit to preview
;;;  (grip-sleep-time 2)             ;; Sleep seconds to ensure the server starts
;;;  :config
;;;  ;; You can get the user name and password from ~/.authinfo like this.
;;;  ;; NOTE: This is not needed unless I swap off of mdopen which appears to work much better at the moment.
;;;  (let* ((auth-info (car (auth-source-search :host "api.github.com" :user "jcook3701^grip" :require '(:user :secret))))
;;;	 (user (plist-get auth-info :user))
;;;	 (password (funcall (plist-get auth-info :secret))))
;;;    (setq grip-github-user user)
;;;    (setq grip-github-password password))
;;;  (message "GitHub user: %s" grip-github-user)

;;;  ;; Override the grip--preview-md function to redirect tmp.md files
;;;  (defun my-grip--preview-md ()
;;;    "Render and preview markdown with grip, redirecting the tmp file."
;;;    (when grip-update-after-change
;;;      (add-hook 'after-change-functions #'grip-refresh-md nil t))
;;;    (add-hook 'after-save-hook #'grip-refresh-md nil t)
;;;    (add-hook 'after-revert-hook #'grip-refresh-md nil t)
;;;    ;; Redirect .tmp.md to a temp directory
;;;    (setq grip--preview-file
;;;          (concat temporary-file-directory
;;;                  (file-name-nondirectory
;;;                   (file-name-sans-extension buffer-file-name)) ".tmp.md"))
;;;    (grip-refresh-md)
;;;    (grip-start-process))

;;;  ;; Apply the override
;;;  (advice-add 'grip--preview-md :override #'my-grip--preview-md)
  
;;;  (setq grip-debug t)
;;;  :bind
;;;  ("C-c C-g g" . grip-mode)
;;;  ("C-c C-g p" . grip-start-preview)
;;;  ("C-c C-g s" . grip-stop-preview)
;;;  :ensure t)

(use-package mdopen-mode
  :hook ((markdown-mode . mdopen-mode))
  ;;	 (after-save-hook . mdopen-refresh))
  :bind
  (:map markdown-mode-command-map
	("C-c C-m" . mdopen-mode))
  :config
  ;; (setq browse-url-browser-function 'browse-url-generic)
  ;; (setq browse-url-generic-program "google-chrome")
  ;; (setq browse-url-generic-args '("--app=http://localhost:5032"))
  :ensure (:fetcher github :repo "jcook3701/mdopen-mode"))

;;; markdown.el ends here
