;;; git.el --- Summary
;;; Commentary:
;; Emacs git packages configurations
;;----------------------------------------------------------------------------------------------
;;; Code:

;; Transient is the library used to implement the keyboard-driven “menus”
;; in Magit. It is distributed as a separate package, so that it can be used
;; to implement similar menus in other packages.
;; 
;; Link: https://github.com/magit/transient
(use-package transient
  :ensure (:fetcher github :repo "magit/transient"))

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
;; TODO: [emacs-authinfo](https://www.gnu.org/software/emacs/manual/html_node/emacs/Authentication.html)
(use-package magit
  :after auth-source
  :bind
  (("C-x g" . magit-status)) ;; Bind Magit status to C-x g
  :ensure t)

;; Work with Git forges, such as Github and Gitlab, from the comfort of Magit and the rest of Emacs.
;;
;; Link: https://github.com/magit/forge
(use-package forge
  :after magit
  :config
  (let* ((auth-info (car (auth-source-search :host "api.github.com" :user "jcook3701^forge" :require '(:user :secret))))
	 (user (plist-get auth-info :user))
	 (password (funcall (plist-get auth-info :secret))))
    (setq forge-github-user user)
    (setq forge-github-password password))
  (message "GitHub user: %s" forge-github-user)
  :ensure t)

;;; git.el ends here
