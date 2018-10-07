---
layout: default
---

# Building Emacs
> Platform: Mac OS High Sierra  
> Version: 25.3  
 
## with-ns
[Emacs 25.3 with-ns](./tutorials/emacs-25-with-ns.html).

## with-xwidget
[Emacs 25.3 with-xwidget](./tutorials/emacs-25-with-xwidget.html).

# Init File Breakdown

### Bash Configuration
#### Auto Complete 

> This allows for alias in the bashrc file to be auto completed inside of Emacs shell.  

```elisp
;; bash-completion
(autoload 'bash-completion-dynamic-complete 
  "bash-completion"
  "BASH completion hook")
(add-hook 'shell-dynamic-complete-functions
	  'bash-completion-dynamic-complete)
```

#### emacsclient
```elisp
;; Shell Feature - Allows ability to open emacs inside of emacs bash
(server-start)
(setq server-socket-dir "~/tmp/emacs1000/server")
```

This below bash function is added to my bashrc file and allows the user to send an already open Emacs window a new buffer. It works from install Emacs "shell" and from a regular bash terminal.
```bash
function run_emacs
{
    emacsclient "$1" &
}
```

### Python Configuration
```elisp
;; ------- Python Packages for Emacs ------- ;;
;; virtualenvwrapper
;;(require 'virtualenvwrapper)
;;(setq venv-location "/Users/jared3701/Documents/College/5th_year/seniorProject/gitHub/supreme_bot")

;; elpy
(elpy-enable)
(setq elpy-rpc-backend "jedi")

;;-------------------------------;;
;; Custom Setup for python shell ;;
;;-------------------------------;;
;;Run python and pop-up its shell.
;; Kill process to solve the reload modules problem.
(defun my-python-shell-run ()
  (interactive)
  (when (get-buffer-process "*Python*")
    (set-process-query-on-exit-flag (get-buffer-process "*Python*") nil)
    (kill-process (get-buffer-process "*Python*"))
    ;; if you want to clean the buffer too.
    ;; (kill-buffer "*Python*")
    ;; Not so fast!
    (sleep-for 0.5))
  (run-python (python-shell-parse-command) nil nil)
  (elpy-shell-send-buffer t)
  ;; Pop a new window only if shell isn't visible
  ;; in any frame.
  (unless (get-buffer-window "*Python*" t)
    (elpy-shell-switch-to-shell)))

(defun my-python-shell-run-region ()
  (interactive)
  (python-shell-send-region (region-beginning) (region-end))
  (python-shell-switch-to-shell))

(eval-after-load 'elpy
  `(progn
     (message "python cfg has finished loading")
     (define-key elpy-mode-map (kbd "C-c C-c") 'my-python-shell-run)
     (define-key elpy-mode-map (kbd "C-c C-r") 'my-python-shell-run-region)
     (define-key elpy-mode-map (kbd "C-c f") 'python-eldoc-at-point)))
```

### xwidget-webkit-browse-url Configuration
```elisp
;; Live Testing for Emacs run on XQartz
;; Version: Emacs 25.3
;; Built with:
;;
;;     ./autogen.sh all
;;     ./configure --with-xwidgets --without-ns --with-gnutls --with-imagemagick --without-dbus --with-x
;;     make install
;;
;; This is actively running on Mac OS High Sierra
;; Version: 10.13.6

;; make these keys behave like normal browser
(define-key xwidget-webkit-mode-map [mouse-4] 'xwidget-webkit-scroll-down)
(define-key xwidget-webkit-mode-map [mouse-5] 'xwidget-webkit-scroll-up)
(define-key xwidget-webkit-mode-map (kbd "<up>") 'xwidget-webkit-scroll-down)
(define-key xwidget-webkit-mode-map (kbd "<down>") 'xwidget-webkit-scroll-up)
(define-key xwidget-webkit-mode-map (kbd "M-w") 'xwidget-webkit-copy-selection-as-kill)
(define-key xwidget-webkit-mode-map (kbd "C-c") 'xwidget-webkit-copy-selection-as-kill)

;; adapt webkit according to window configuration chagne automatically
;; without this hook, every time you change your window configuration,
;; you must press 'a' to adapt webkit content to new window size
(add-hook 'window-configuration-change-hook (lambda ()
               (when (equal major-mode 'xwidget-webkit-mode)
                 (xwidget-webkit-adjust-size-dispatch))))

;; by default, xwidget reuses previous xwidget window,
;; thus overriding your current website, unless a prefix argument
;; is supplied
;;
;; This function always opens a new website in a new window
(defun xwidget-browse-url-no-reuse (url &optional sessoin)
  (interactive (progn
                 (require 'browse-url)
                 (browse-url-interactive-arg "xwidget-webkit URL: "
                                             )))
  (xwidget-webkit-browse-url url t))

;; make xwidget default browser
(setq browse-url-browser-function (lambda (url session)
                    (other-window 1)
                    (xwidget-browse-url-no-reuse url)))
```
