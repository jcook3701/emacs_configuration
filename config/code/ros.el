;;; ros.el --- Summary
;;; Commentary:
;; Emacs ros package configuration
;;----------------------------------------------------------------------------------------------
;;; Code:

;; ros.el is an emacs package which should ease the interaction with ROS nodes
;; as well as help developing software for ROS systems. ROS is the
;; Robot Operating System and focuses on exchanging messages between nodes.
;; Since the systems can become quite complex, why not use the best editor
;; in the word to interact with it? The main advantage of this package over
;; using the shell commands is using completing-read functions for almost anything
;; which allows for fuzzy matching using packages like helm or ivy.
;;
;; Link: https://github.com/mbeutelspacher/ros.el

;; (use-package ros
;;  :config
;;  (setq ros-workspaces
;;      (list
;;       (ros-dump-workspace :tramp-prefix nil :workspace "~/main_ws" :extends '("/opt/ros/noetic/"))
;;       (ros-dump-workspace :tramp-prefix nil :workspace "~/overlay_ws" :extends '("/opt/ros/noetic/" "~/main_ws/install"))))
    
;;  :ensure t)

;; ROS Emacs
;;; (add-to-list 'load-path' "/opt/ros/melodic/share/emacs/site-lisp")
;;; (require 'rosemacs-config)
;;; (require 'helm-ros)

;;; ros.el ends here
