;;; ansible.el --- Summary
;;; Commentary:
;; Emacs ansible packages configurations
;;----------------------------------------------------------------------------------------------
;;; Code:

;; Ansible minor mode designed to be used for modifying Ansible files.
;; 
;; Requirement
;; * yasnippet
;; *auto-complete
;;
;; Link: https://github.com/k1LoW/emacs-ansible
;; NOTE: The hook to ansible-mode might need to be removed when working on non-ansible projects
;;       This was needed to enable ls-ansible within lsp.  Otherwise only yamlls server starts.
(use-package ansible
  :after yaml
  :hook
  (((yaml-ts-mode yaml-mode) . ansible-mode)
   (ansible-mode . lsp-deferred))
	 ;;(ansible-mode . my-yaml-with-jinja2-highlighting))
  :interpreter ("ansible" . ansible-mode)
  :config
  (defun my-yaml-with-jinja2-highlighting ()
    "Add Jinja2 syntax highlighting to yaml-mode."
    (message "Add Jinja2 syntax highlighting to yaml-mode.")
    (font-lock-add-keywords nil
			    `(
			      ;; First, match the name: field and value
			      ("^ *- \\(name\\):\\([^#\n]*\\)"
			       (1 font-lock-builtin-face t)
			       (2 ansible-task-label-face t))
			      ;; Highlight {{ ... }} expressions in Jinja2
			      ("\\({{\\)\\([^}]+\\)\\(}}\\)"
			       (1 font-lock-builtin-face t)
			       (2 font-lock-function-name-face t)
			       (3 font-lock-builtin-face t))
			      ;; Highlight {{ ... }} expressions in Jinja2
			      ("\\(({\\)\\([^})]+\\)\\(})\\)"
			       (1 font-lock-builtin-face t)
			       (2 font-lock-function-name-face t)
			       (3 font-lock-builtin-face t))
			      ;; Highlight {% ... %} logic blocks in Jinja2
			      ("\\({%\\)\\([^}]+\\)\\(%}\\)"
			       (1 font-lock-builtin-face t)
			       (2 font-lock-function-name-face t)
			       (3 font-lock-builtin-face t))
			      ;; Highlight {# ... #} comment blocks in Jinja2
			      ("\\({#\\)\\([^}]+\\)\\(#}\\)"
			       (1 font-lock-comment-delimiter-face t)
			       (2 font-lock-comment-face t)
			       (3 font-lock-comment-delimiter-face t))
			      ))
    "Font lock definitions for Jinja2 syntax in Ansible playbooks.")
  :ensure t)

;; Ansible documentation lookup for GNU Emacs:
;;
;; Link: https://github.com/emacsorphanage/ansible-doc
(use-package ansible-doc
  :after ansible
  :hook (ansible-mode . ansible-doc-mode)
  :ensure t)

;; Minor mode for in place manipulation of ansible-vault.
;;
;; Link: https://github.com/zellio/ansible-vault-mode
(use-package ansible-vault
  :hook
  ((yaml-ts-mode yaml-mode) . ansible-vault-mode-maybe)
  :init
  (defun ansible-vault-mode-maybe ()
    (when (ansible-vault--is-encrypted-vault-file)
      (ansible-vault-mode 1)))
  ;; :config
  ;; (setq ansible-vault-password-file "~/vault-pass")
  :ensure t)

;;; ansible.el ends here
