;;; docker.el --- Summary
;;; Commentary:
;; Emacs docker packages configurations
;;----------------------------------------------------------------------------------------------
;;; Code:

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
  :hook ((dockerfile-ts-mode dockerfile-mode) . lsp-deferred)
  :mode ("\\Dockerfile\\'" . dockerfile-mode)
  :interpreter ("dockerfile" . dockerfile-mode)
  :ensure t)

;; Major mode for editing docker-compose files, providing context-aware completion of docker-compose
;; keys through completion-at-point-functions.
;; 
;; Link: https://github.com/meqif/docker-compose-mode
(use-package docker-compose-mode
  :ensure t)

;;; docker.el ends here
