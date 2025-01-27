;;; chatgpt.el --- Summary
;;; Commentary:
;; Emacs chatgpt packages configurations
;;----------------------------------------------------------------------------------------------
;;; Code:

;; Load OpenAI Key
(let ((api-key-file "~/.emacs.d/openai-api-key.el"))
  (when (file-exists-p api-key-file)
    (message "Loading OpenAI API key...")
    (load api-key-file)))

;; Link: https://github.com/emacs-openai/openai#-usage
(use-package openai
  :config
  ;; Set the API key (already loaded from the external file)
  (setq openai-key openai-api-key)
  :ensure (:host github
		 :repo "emacs-openai/openai"))

;; Link: https://github.com/emacs-openai/chatgpt?tab=readme-ov-file
(use-package chatgpt
  :bind
  (("C-c C-a" . chatgpt)  ;; Bind 'chatgpt' to "C-c g"
   ("C-c C-q" . chatgpt-shell)) ;; Bind 'chatgpt-query' to "C-c q"
  :after openai
  :ensure (:host github
		 :repo "emacs-openai/chatgpt"))

;; Link: https://github.com/xenodium/chatgpt-shell
(use-package chatgpt-shell
;;   :bind
;;   ("" . )
  :custom
  (chatgpt-shell-openai-key openai-api-key)
  :ensure t)

;; NOTE: https://www.reddit.com/r/LocalLLaMA/comments/1dadaq6/lspai_an_open_source_language_server_brining/
;; TODO: Look more into setting this up with a local llm
;; https://github.com/jart/emacs-copilot
;; https://github.com/huggingface/llm-ls
;; https://github.com/ggerganov/llama.cpp

;;; chatgpt.el ends here
