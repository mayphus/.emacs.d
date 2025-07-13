;;; claude-code.el --- Claude Code integration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun claude-code ()
  "Open eshell buffer and run claude CLI."
  (interactive)
  (let ((buffer-name "*claude-code*"))
    (if (get-buffer buffer-name)
        (switch-to-buffer buffer-name)
      (let ((eshell-buffer-name buffer-name))
        (eshell)
        (insert "claude")
        (eshell-send-input)))))

(provide 'claude-code)
;;; claude-code.el ends here
