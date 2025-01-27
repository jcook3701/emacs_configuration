;;; yaml.el --- Summary
;;; Commentary:
;; Emacs yaml packages configurations
;;----------------------------------------------------------------------------------------------
;;; Code:


;; yaml.el is a YAML parser written in Emacs List without any external dependencies.
;; It provides an interface similar to the Emacs JSON parsing utility. The functions
;; provided are as follows:
;;
;; Link: https://github.com/zkry/yaml.el
(use-package yaml
  :ensure t)

;; Simple major mode to edit YAML file for emacs
;; 
;; Link: https://github.com/yoshiki/yaml-mode
(use-package yaml-mode
  :mode ("\\.ya?ml\\'" . yaml-mode)
  :interpreter ("yaml" . yaml-mode)
  :config
  (setq yaml-indent-offset 2) ;; Set the indentation width to 2 spaces
  :ensure t)

;; yaml-pro is a package that provides conveniences for editing yaml.
;;
;; Link: https://github.com/zkry/yaml-pro
(use-package yaml-pro
  :hook ((yaml-ts-mode yaml-mode) . yaml-pro-mode) ;; Enable yaml-pro-mode automatically in yaml-mode
  :config
  ;; Optional: Set up keybindings for yaml-pro
  (define-key yaml-pro-mode-map (kbd "C-c C-n") 'yaml-pro-forward-node)
  (define-key yaml-pro-mode-map (kbd "C-c C-p") 'yaml-pro-backward-node)
  (define-key yaml-pro-mode-map (kbd "C-c C-d") 'yaml-pro-delete-node)
  (define-key yaml-pro-mode-map (kbd "C-c C-a") 'yaml-pro-add-node)
  (define-key yaml-pro-mode-map (kbd "C-c C-r") 'yaml-pro-rename-node)
  :ensure t)

;;; yaml.el ends here
