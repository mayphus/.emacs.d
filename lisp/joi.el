;;; joi.el --- Chat with AI

(require 'gptel)

(defun joi-chat (user-query)
  "Start a chat with AI using USER-QUERY as the initial prompt."
  (interactive "sChat with AI: ")
  (gptel-request user-query))

(global-set-key (kbd "C-c j") 'joi-chat)

(provide 'joi)
;;; joi.el ends here