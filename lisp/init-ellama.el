;;; init-ellama.el

(use-package ellama
  :init
  (setopt ellama-keymap-prefix "C-c e")

  (require 'init-private)
  (require 'llm-openai)

  ;; TODO: issue with connection
  (setopt ellama-naming-provider
	  (make-llm-openai-compatible
	   :key groq-api-key
	   :url "https://api.groq.com/openai/v1/"
	   :chat-model "llama-3.1-70b-versatile"))

  (setopt ellama-naming-scheme 'ellama-generate-name-by-llm)

  (setopt llm-warn-on-nonfree nil))

(provide 'init-ellama)
;;; init.ellama.el ends here
