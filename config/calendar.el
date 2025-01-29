;;; calendar.el --- Summary
;;; Commentary:
;; Emacs calendar settings configuration
;;----------------------------------------------------------------------------------------------
;;; Code:

;; This program displays a calendar view in the Emacs buffer.
;;
;; Link: https://github.com/kiwanami/emacs-calfw
(use-package calfw
  :defer t
  :config
  (use-package calfw-org
    :elpaca t
    :after org
    :config
    (setq cfw:org-overwrite-default-keybinding t)
    ;; Example function to open the calendar
    (defun my/open-calendar ()
      "Open the calendar with org agenda integration."
      (interactive)
      (cfw:open-org-calendar)))
  :ensure t)

;; Caldav sync for Emacs Orgmode
;;
;; Link: https://github.com/dengste/org-caldav
(use-package org-caldav
  :defer t
  :config
  ;; Set up org-caldav configuration
  (setq ;; org-caldav-url "https://apidata.googleusercontent.com/caldav/v2/"
        ;; org-caldav-calendar-id "your-calendar-id"
        ;; org-caldav-inbox "~/.emacs.d/org/org-caldav-inbox.org"
        ;; org-caldav-files '("~/.emacs.d/org/club-wpt-schedule.org")
        org-caldav-sync-direction 'both ;; Sync both ways
	org-icalendar-timezone "America/Los_Angeles")

  ;; Function to synchronize with CalDAV
  (defun my/org-caldav-sync ()
    "Synchronize org files with CalDAV."
    (interactive)
    (org-caldav-sync))

  ;; Hook to sync after saving org files
  (add-hook 'after-save-hook
            (lambda ()
              (when (member buffer-file-name org-caldav-files)
                (my/org-caldav-sync))))
    :ensure t)

;;; calendar.el ends here
