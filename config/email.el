;;; email.el --- Summary
;;; Commentary:
;; Emacs email packages configurations
;;----------------------------------------------------------------------------------------------
;;; Code:

(use-package mu4e
  :defer 20 ; Defer loading to speed up startup
  :config
  ;; Set the location of your Maildir
  (setq mu4e-maildir "~/Maildir") ; Replace with your Maildir path

  ;; Basic settings
  (setq mu4e-get-mail-command "mbsync -a" ; Use mbsync to sync mail
        mu4e-update-interval 300          ; Update mail every 5 minutes
        mu4e-view-show-images t
        mu4e-view-show-addresses t
        mu4e-sent-folder "/[Gmail]/Sent Mail"
        mu4e-drafts-folder "/[Gmail]/Drafts"
        mu4e-trash-folder "/[Gmail]/Trash")

  ;; Use `authinfo` for credentials
  (setq smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 465
        smtpmail-stream-type 'ssl
        smtpmail-auth-credentials (expand-file-name "~/.authinfo"))

  ;; Sending mail
  (setq user-mail-address "your-email@gmail.com"
        user-full-name "Your Name"
        message-send-mail-function 'smtpmail-send-it)

  ;; Automatically start the mu4e main view
  (global-set-key (kbd "C-x m") 'mu4e))

;; Install and configure `mbsync` for IMAP syncing
(use-package mbsync
  :elpaca nil
  :config
  ;; Sync Gmail mailboxes using mbsync
  (setq mbsync-command "mbsync -a"))


;; https://www.reddit.com/r/emacs/comments/z0bopp/is_there_a_complete_guide_for_setting_up/
;; https://macowners.club/posts/email-emacs-mu4e-macos/

;;; email.el ends here
