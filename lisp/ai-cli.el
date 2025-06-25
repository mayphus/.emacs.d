;;; -*- lexical-binding: t; -*-
;;; ai-cli.el --- Minimal AI CLI wrappers

(require 'vterm)
(require 'project)

(defun ai-cli--find-project-root ()
  "Find the current project root directory."
  (when-let ((project (project-current)))
    (project-root project)))

(defun ai-cli--select-project-directory ()
  "Prompt user to select a project directory."
  (read-directory-name "Select project directory: " 
                       (or (ai-cli--find-project-root) 
                           default-directory) 
                       nil t))

(defun ai-cli-start (command)
  "Start AI CLI with project detection, or prompt for directory if no project found."
  (let ((project-root (ai-cli--find-project-root)))
    (if project-root
        (ai-cli command project-root)
      (ai-cli command (ai-cli--select-project-directory)))))

(defun ai-cli-start-in-directory (command)
  "Start AI CLI in a selected directory."
  (ai-cli command (ai-cli--select-project-directory)))

(defun ai-cli-send (command)
  "Send region or buffer to AI CLI with project detection, or prompt if no project."
  (let ((project-root (ai-cli--find-project-root)))
    (if project-root
        (ai-send-text command project-root)
      (ai-send-text command (ai-cli--select-project-directory)))))

(defun ai-cli (command &optional directory)
  "Start AI CLI in vterm buffer, optionally in DIRECTORY."
  (let ((buffer-name (format "*%s*" command))
        (default-directory (or directory default-directory)))
    (if (get-buffer buffer-name)
        (pop-to-buffer buffer-name)
      (let ((buffer (get-buffer-create buffer-name)))
        (with-current-buffer buffer
          (vterm-mode)
          (vterm-send-string (format "%s\n" command)))
        (pop-to-buffer buffer)))))

(defun gemini ()
  "Start Gemini CLI in project root, or prompt for directory if no project found."
  (interactive)
  (ai-cli-start "gemini"))

(defun claude ()
  "Start Claude CLI in project root, or prompt for directory if no project found."
  (interactive)
  (ai-cli-start "claude"))

(defun claude-in-directory ()
  "Start Claude CLI in a selected directory."
  (interactive)
  (ai-cli-start-in-directory "claude"))

(defun gemini-in-directory ()
  "Start Gemini CLI in a selected directory."
  (interactive)
  (ai-cli-start-in-directory "gemini"))

(defun codex-in-directory ()
  "Start Codex CLI in a selected directory."
  (interactive)
  (ai-cli-start-in-directory "codex"))

(defun codex ()
  "Start Codex CLI in project root, or prompt for directory if no project found."
  (interactive)
  (ai-cli-start "codex"))

(defun ai-send-text (command &optional directory)
  "Send region if selected, otherwise send entire buffer to AI CLI."
  (let ((text (if (use-region-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (buffer-substring-no-properties (point-min) (point-max))))
        (buffer-name (format "*%s*" command)))
    (ai-cli command directory)
    (with-current-buffer buffer-name
      (vterm-send-string text)
      (vterm-send-return))))

(defun gemini-send ()
  "Send region or buffer to Gemini CLI in project root, or prompt if no project."
  (interactive)
  (ai-cli-send "gemini"))

(defun claude-send ()
  "Send region or buffer to Claude CLI in project root, or prompt if no project."
  (interactive)
  (ai-cli-send "claude"))

(defun codex-send ()
  "Send region or buffer to Codex CLI in project root, or prompt if no project."
  (interactive)
  (ai-cli-send "codex"))

(provide 'ai-cli)

;;; ai-cli.el ends here
