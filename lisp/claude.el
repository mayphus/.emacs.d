;;; claude.el --- Claude Code CLI Integration -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: Claude Code
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: ai, claude, cli

;;; Code:

(defcustom claude-executable "claude"
  "Path to the claude executable."
  :type 'string
  :group 'claude)

;;;###autoload
(defun claude-send (prompt)
  "Send PROMPT to Claude and display result in a buffer."
  (interactive "sPrompt: ")
  (let ((buffer (get-buffer-create "*claude*"))
        (command (format "%s %s" claude-executable (shell-quote-argument prompt))))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (format "> %s\n" prompt))
      (call-process-shell-command command nil buffer t))
    (display-buffer buffer)))

;;;###autoload
(defun claude ()
  "Send a prompt to Claude."
  (interactive)
  (call-interactively 'claude-send))

(provide 'claude)

;;; claude.el ends here
