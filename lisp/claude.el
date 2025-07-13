;;; claude.el --- Claude Code -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: Mayphus Tang
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: ai, claude, cli

;;; Code:

(defcustom claude-executable "claude"
  "Path to the claude executable."
  :type 'string
  :group 'claude)

;;;###autoload
(defun claude-query (prompt)
  "Send PROMPT to Claude and display result in a buffer."
  (interactive "sPrompt: ")
  (let ((buffer (get-buffer-create "*claude*"))
        (command (format "%s -p %s" claude-executable (shell-quote-argument prompt))))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (format "> %s\n" prompt)))
    (display-buffer buffer)
    (start-process "claude-process" buffer shell-file-name "-c" command)))

;;;###autoload
(defun claude ()
  "Send a prompt to Claude."
  (interactive)
  (call-interactively 'claude-query))

(provide 'claude)

;;; claude.el ends here
