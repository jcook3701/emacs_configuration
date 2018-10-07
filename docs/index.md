---
layout: default
---

# Building Emacs
> Platform: Mac OS High Sierra
>
> Version: 25.3
 
## with-ns
[Emacs 25.3 with-ns](./tutorials/emacs-25-with-ns.html).

## with-xwidget
[Emacs 25.3 with-xwidget](./tutorials/emacs-25-with-xwidget.html).

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
