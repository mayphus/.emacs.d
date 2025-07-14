;;; claude-code.el --- Claude Code integration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;;(require 'eshell)
(require 'em-banner)
(require 'em-prompt)

(defun claude-code ()
  "Open eshell buffer and run claude CLI."
  (interactive)
  (let ((buffer-name "*claude-code*"))
    (if (get-buffer buffer-name)
        (switch-to-buffer buffer-name)
      (let ((eshell-banner-message "")
            (eshell-prompt-function (lambda () "")))
        (setq eshell-buffer-name buffer-name)
        (eshell)
        (insert "claude")
        (eshell-send-input)
        (run-with-timer 0.1 nil
                        (lambda ()
                          (with-current-buffer "*claude-code*"
                            (goto-char (point-min))
                            (delete-region (point-min) (progn (forward-line 1) (point))))))))))

(provide 'claude-code)
;;; claude-code.el ends here
