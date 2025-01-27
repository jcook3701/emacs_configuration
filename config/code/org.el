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
  :ensure (:wait t))

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
;;;
;;; (eval-after-load "org"
;;;  '(require 'ox-gfm nil t))

;; This is a small exporter based on the Markdown exporter already existing
;; in Org mode.
;;
;; Link: https://github.com/larstvei/ox-gfm
(use-package ox-gfm
  :after org
  :ensure t)

;;; org.el ends here
