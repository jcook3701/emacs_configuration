;;; python.el --- Summary
;;; Commentary:
;; Emacs python packages configurations
;;----------------------------------------------------------------------------------------------
;;; Code:

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
  :commands (python-mode python-ts-mode)
  :mode ("\\.py\\'" . python-ts-mode)
  :interpreter ("python" . python-ts-mode)
  :hook
  (((python-ts-mode python-mode) .  (lambda () (eldoc-mode -1)))
   ((python-ts-mode python-mode) . dap-mode))
;;;  :init
  ;; Custom
;;;  (setenv "PYTHONIOENCODING" "utf-8")
;;;  (add-to-list 'process-coding-system-alist '("python" . (utf-8 . utf-8)))
  ;; (add-to-list 'process-coding-system-alist '("elpy" . (utf-8 . utf-8)))
;;;  (add-to-list 'process-coding-system-alist '("flake8" . (utf-8 . utf-8)))
;;;  (add-to-list 'process-coding-system-alist '("python" . (utf-8 . utf-8)))
;;;  :config
;;;  (setenv "FREECAD_MOD" "/usr/share/freecad/Mod/Web:/usr/share/freecad/Mod/Tux:/usr/share/freecad/Mod/Draft:/usr/share/freecad/Mod/OpenSCAD:/usr/share/freecad/Mod/Import:/usr/share/freecad/Mod/Path:/usr/share/freecad/Mod/Drawing:/usr/share/freecad/Mod/Part:/usr/share/freecad/Mod/Material:/usr/share/freecad/Mod/Points:/usr/share/freecad/Mod/Test:/usr/share/freecad/Mod/Arch:/usr/share/freecad/Mod/Image:/usr/share/freecad/Mod/Robot:/usr/share/freecad/Mod/AddonManager:/usr/share/freecad/Mod/Start:/usr/share/freecad/Mod/Inspection:/usr/share/freecad/Mod/PartDesign:/usr/share/freecad/Mod/ReverseEngineering:/usr/share/freecad/Mod/Fem:/usr/share/freecad/Mod/Surface:/usr/share/freecad/Mod/Sketcher:/usr/share/freecad/Mod/Measure:/usr/share/freecad/Mod/TechDraw:/usr/share/freecad/Mod/Show:/usr/share/freecad/Mod/Spreadsheet:/usr/share/freecad/Mod/Raytracing:/usr/share/freecad/Mod/MeshPart:/usr/share/freecad/Mod/Mesh:/usr/share/freecad/Mod/Idf:/usr/share/freecad/Mod:/usr/lib/freecad/Mod")
;;;  (setenv "FREECAD_LIB" "/usr/lib/freecad/lib:/usr/lib/freecad-python3/lib")
;;;  (setenv "FREECAD_EXT" "/usr/lib/freecad/Ext")
;;;  (setenv "FREECAD_BIN" "/usr/lib/freecad/bin")
;;;  (setenv "FREECAD_MACRO" "/home/jcook/.FreeCAD/Macro:/usr/lib/freecad/Macro")
;;; (setenv "FREECAD_STUBS" "/home/jcook/Documents/git_repo/freecad-stubs/out")
;;;  (setenv "ROS_LIB" "/opt/ros/melodic/lib/python2.7/dist-packages")
;;;  (setenv "PYTHONPATH" (concat (getenv "FREECAD_MOD")
;;;			       ":"
;;;			       (getenv "FREECAD_LIB")
;;;			       ":"
;;;			       (getenv "FREECAD_EXT")
;;;			       ":"
;;;			       ;;(getenv "FREECAD_STUBS")
;;;			       ;;":"
;;;			       (getenv "FREECAD_BIN")
;;;			       ":"
;;;			       (getenv "FREECAD_MACRO")
;;;			       ":"
;;;			       (getenv "ROS_LIB")
;;;			       ":"
;;;			       (getenv "PYTHONPATH")))
  ;; (define-key python-mode-map (kbd "C-c C-c") 'python-shell-r)
  ;; (require 'dap-mode)
  :ensure t)

;;;(use-package pyvenv
;;;  :demand t
;;;  :config
  ;; (setenv "JAVA_HOME" "~/Documents/python_virtual_envs/nodejs/")
;;;  (setenv "WORKON_HOME" "~/Documents/python_virtual_envs/python3")

;;;  (pyvenv-tracking-mode 1)
;;;  (setq pyvenv-mode-line-indicator '(pyvenv-virtual-env-name ("[venv:" pyvenv-virtual-env-name "] ")))
;;;  (pyvenv-mode t)

  ;; Set correct Python interpreter
;;;  (setq pyvenv-post-activate-hooks
;;;        (list (lambda ()
;;;                (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python3"))
;;;		(lsp))))
;;;  (setq pyvenv-post-deactivate-hooks
;;;        (list (lambda ()
;;;                (setq python-shell-interpreter "python3")
;;;		(lsp-shutdown-workspace))))

;;;  :ensure t)

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

;;;(use-package lsp-pyright
;;  :hook ((python-ts-mode python-mode) . (lambda ()
;;					  (require 'lsp-pyright)))
  ;; (lsp)))  ; or lsp-deferre
;;  :config
;;  (setq lsp-pyright-python-executable-cmd "python3")
;;  (setq lsp-pyright-log-level "trace")
;;  (setq lsp-pyright-stub-path "/home/jcook/.python3_stubs/FreeCAD")
;;  (setq lsp-pyright-extra-paths
;;	(vconcat lsp-pyright-extra-paths
;;		 ["/usr/share/freecad/Mod/Web",
;;		  "/usr/share/freecad/Mod/Tux",
;;		  "/usr/share/freecad/Mod/Draft",
;;		  "/usr/share/freecad/Mod/OpenSCAD",
;;		  "/usr/share/freecad/Mod/Import",
;;		  "/usr/share/freecad/Mod/Path",
;;		  "/usr/share/freecad/Mod/Drawing",
;;		  "/usr/share/freecad/Mod/Part",
;;		  "/usr/share/freecad/Mod/Material",
;;		  "/usr/share/freecad/Mod/Points",
;;		  "/usr/share/freecad/Mod/Test",
;;		  "/usr/share/freecad/Mod/Arch",
;;		  "/usr/share/freecad/Mod/Image",
;;		  "/usr/share/freecad/Mod/Robot",
;;		  "/usr/share/freecad/Mod/AddonManager",
;;		  "/usr/share/freecad/Mod/Start",
;;		  "/usr/share/freecad/Mod/Inspection",
;;		  "/usr/share/freecad/Mod/PartDesign",
;;		  "/usr/share/freecad/Mod/ReverseEngineering",
;;		  "/usr/share/freecad/Mod/Fem",
;;		  "/usr/share/freecad/Mod/Surface",
;;		  "/usr/share/freecad/Mod/Sketcher",
;;		  "/usr/share/freecad/Mod/Measure",
;;		  "/usr/share/freecad/Mod/TechDraw",
;;		  "/usr/share/freecad/Mod/Show",
;;		  "/usr/share/freecad/Mod/Spreadsheet",
;;		  "/usr/share/freecad/Mod/Raytracing",
;;		  "/usr/share/freecad/Mod/MeshPart",
;;		  "/usr/share/freecad/Mod/Mesh",
;;		  "/usr/share/freecad/Mod/Idf",
;;		  "/usr/share/freecad/Mod",
;;		  "/usr/lib/freecad/Mod",
;;		  "/usr/lib/freecad-python3/lib",
;;		  "/usr/lib/freecad/lib",
;;		  "/usr/lib/freecad/Ext",		  
;;		  "/usr/lib/freecad/bin",
;;		  "/home/jcook/.FreeCAD/Macro",
;;		  "/usr/lib/freecad/Macro"]))
;;;  :ensure t)

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

;;; python.el ends here
