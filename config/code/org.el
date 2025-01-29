;;; org.el --- Summary
;;; Commentary:
;; Emacs org packages configurations
;;----------------------------------------------------------------------------------------------
;;; Code:

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
  :hook ((org-mode . visual-line-mode)
         (org-mode . org-indent-mode))
  :init
  ;; Allows Org mode to run bash scrips
  (org-babel-do-load-languages 'org-babel-load-languages
			       '(
				 (shell . t)
				 )
			       )

  (setq org-confirm-babel-evaluate nil)
  :config
  ;; Better agenda view
  (setq org-agenda-start-on-weekday 1      ;; Start agenda on Monday
        org-agenda-span 'week             ;; Show a week's agenda
        org-agenda-window-setup 'current-window)

  ;; Todo keywords with custom states
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

  ;; Inline images automatically
  (setq org-startup-with-inline-images t
	org-image-actual-width '(300)) ;; Limit image size for better readability
  
  ;; LaTeX previews
  (setq org-preview-latex-default-process 'dvisvgm) ;; Better quality previews
  
  ;; Task dependencies
  (setq org-enforce-todo-dependencies t)
  
  ;; Habit tracking
  (setq org-habit-graph-column 50)

  ;; Syntax highlighting in code blocks
  (setq org-src-fontify-natively t
	org-src-tab-acts-natively t
	org-edit-src-content-indentation 2)
  
  :ensure (:wait t))

;; Babel is about letting many different languages work together.
;; Programming languages live in code blocks inside natural language Org documents.
;;
;; Link: https://orgmode.org/worg/org-contrib/babel/intro.html
(use-package ob
  :ensure nil ;; 'ob' Built-in package
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (shell . t)
     (sql . t))) ;; Add more languages if needed
  (setq org-confirm-babel-evaluate nil)) ;; Disable confirmation for code execution


;; Prettify headings and plain lists in Org mode.
;; This package is a direct descendant of ‘org-bullets’,
;; with most of the code base completely rewritten.
;; Currently, this package supports:
;;
;; Link: https://github.com/integral-dw/org-superstar-mode
(use-package org-superstar
  :hook (org-mode . org-superstar-mode)
  :after org
  :config
  (setq org-superstar-headline-bullets-list '("◉" "○" "▶" "▷")
        org-superstar-item-bullet-alist '((?- . "•") (?+ . "◦")))
  :ensure t)


;; This is an implementation of dynamic virtual indentation.  It works
;; by adding text properties to a buffer to make sure lines are
;; indented according to outline structure.
;;
;; Link: https://github.com/tkf/org-mode/blob/master/lisp/org-indent.el
(use-package org-indent
  :after org
  :custom
  (org-indent-indentation-per-level 4)
  :ensure nil)

;; A carefully crafted Org exporter back-end for Hugo.
;; 'ox-hugo' is an Org exporter backend that exports Org
;; to Hugo-compatible Markdown (Blackfriday) and also generates
;; the front-matter (in TOML or YAML format).
;; 
;; Link: https://github.com/kaushalmodi/ox-hugo/tree/main
(use-package ox-hugo
  :after ox
  :config
  (setq org-hugo-auto-set-lastmod t))

;; Github Flavored Markdown exporter for Org Mode
;; This is a small exporter based on the Markdown exporter already existing
;; in Org mode.
;;
;; Link: https://github.com/larstvei/ox-gfm
(use-package ox-gfm
  :after org
  :ensure t)

;; This package implements a modern style for your Org
;; buffers using font locking and text properties. The
;; package styles headlines, keywords, tables and source
;; blocks. The styling is configurable, you can enable,
;; disable or modify the style of each syntax element
;; individually via the org-modern customization group.
;;
;; Link: https://github.com/minad/org-modern
(use-package org-modern
  :after org
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-star '("⦿" "⦾" "◉" "○")
        org-modern-table nil             ;; Disable table decorations for simplicity
        org-modern-list '((43 . "➤") (45 . "–") (42 . "•"))))

;; This adds very basic support for Pomodoro technique in Emacs' org-mode.
;;
;; Link: https://github.com/marcinkoziej/org-pomodoro
;; (use-package org-pomodoro
;;   :after org
;;  :bind ("C-c p" . org-pomodoro))

;;; org.el ends here
