;;; -*- lexical-binding: t; -*-
;;; ai-cli.el --- Simple Claude CLI wrapper with transient UI

(require 'project)
(require 'transient)

;; Optional vterm requirement with fallback
(condition-case nil
    (require 'vterm)
  (error 
   (message "vterm not available - AI CLI functionality will be limited")
   (defun vterm-mode () (error "vterm not available"))
   (defun vterm-send-string (_text) (error "vterm not available"))
   (defun vterm-send-return () (error "vterm not available"))))

;; Compatibility for older Emacs versions
(unless (boundp 'overriding-text-conversion-style)
  (defvar overriding-text-conversion-style nil
    "Compatibility variable for older Emacs versions."))

;; Configuration constants
(defcustom ai-cli-side-window-width 0.33
  "Width ratio for AI CLI side window."
  :type 'float
  :group 'ai-cli)

(defcustom ai-cli-debounce-delay 0.5
  "Delay in seconds for debouncing project switches."
  :type 'float
  :group 'ai-cli)


(defun ai-cli--find-project-root ()
  "Find the current project root directory."
  (when-let* ((project (project-current)))
    (project-root project)))

(defun ai-cli--select-directory ()
  "Prompt user to select a directory."
  (read-directory-name "Directory: " 
                       (or (ai-cli--find-project-root) 
                           default-directory) 
                       nil t))

(defvar ai-cli-side-window nil
  "Reference to the AI CLI side window.")

(defun ai-cli--get-buffer-for-project ()
  "Get the appropriate AI CLI buffer for current project."
  (let* ((project-root (ai-cli--find-project-root))
         (buffer-name (if project-root
                          (format "*claude-%s*" (file-name-nondirectory (directory-file-name project-root)))
                        "*claude*")))
    buffer-name))

(defun ai-cli-show-side-window ()
  "Show AI CLI in a side window on the right with 1/3 width."
  (interactive)
  (let* ((buffer-name (ai-cli--get-buffer-for-project))
         (buffer (get-buffer buffer-name)))
    ;; Create buffer if it doesn't exist
    (unless buffer
      (claude))
    ;; Get the buffer again after creation
    (setq buffer (get-buffer buffer-name))
    (when buffer
      ;; Delete existing side window if it exists
      (when (and ai-cli-side-window (window-live-p ai-cli-side-window))
        (delete-window ai-cli-side-window))
      ;; Create new side window
      (setq ai-cli-side-window
            (display-buffer-in-side-window 
             buffer
             `((side . right)
               (window-width . ,ai-cli-side-window-width)
               (slot . 0)
               (window-parameters . ((no-delete-other-windows . t)))))))))

(defun ai-cli-hide-side-window ()
  "Hide the AI CLI side window."
  (interactive)
  (when (and ai-cli-side-window (window-live-p ai-cli-side-window))
    (delete-window ai-cli-side-window)
    (setq ai-cli-side-window nil)))

(defun ai-cli-toggle-side-window ()
  "Toggle AI CLI side window visibility."
  (interactive)
  (if (and ai-cli-side-window (window-live-p ai-cli-side-window))
      (ai-cli-hide-side-window)
    (ai-cli-show-side-window)))

(defun ai-cli-switch-project-buffer ()
  "Switch AI CLI side window to current project's buffer."
  (interactive)
  (when (and ai-cli-side-window (window-live-p ai-cli-side-window))
    (let* ((buffer-name (ai-cli--get-buffer-for-project))
           (buffer (get-buffer buffer-name)))
      ;; Create buffer if it doesn't exist
      (unless buffer
        (claude)
        (setq buffer (get-buffer buffer-name)))
      ;; Switch window to the correct buffer
      (when buffer
        (set-window-buffer ai-cli-side-window buffer)))))

;; Debounced auto-switch with timer to avoid performance issues
(defvar ai-cli--switch-timer nil
  "Timer for debounced project switching.")

(defun ai-cli--auto-switch-on-file-change ()
  "Automatically switch AI CLI buffer when changing files in different projects."
  (when ai-cli--switch-timer
    (cancel-timer ai-cli--switch-timer))
  (setq ai-cli--switch-timer
        (run-with-idle-timer 0.5 nil
                             (lambda ()
                               (when (and ai-cli-side-window (window-live-p ai-cli-side-window))
                                 (ai-cli-switch-project-buffer))))))

;; Use window selection hook instead of buffer-list-update for better performance
(add-hook 'window-selection-change-functions
          (lambda (_frame) (ai-cli--auto-switch-on-file-change)))

(defun ai-cli (command &optional directory)
  "Start AI CLI in vterm buffer, optionally in DIRECTORY."
  (condition-case err
      (let ((buffer-name (format "*%s*" command))
            (default-directory (or directory default-directory)))
        (if (get-buffer buffer-name)
            (pop-to-buffer buffer-name)
          (let ((buffer (get-buffer-create buffer-name)))
            (with-current-buffer buffer
              (vterm-mode)
              (vterm-send-string (format "%s\n" command)))
            (pop-to-buffer buffer))))
    (error
     (message "Failed to start %s: %s" command (error-message-string err))
     nil)))

;; Main Claude functions
(defun claude ()
  "Start Claude CLI in project root, or prompt to select directory."
  (interactive)
  (ai-cli "claude" (or (ai-cli--find-project-root) 
                       (ai-cli--select-directory))))

(defun claude-directory ()
  "Start Claude CLI in selected directory."
  (interactive)
  (ai-cli "claude" (ai-cli--select-directory)))

(defun claude-send ()
  "Send region or buffer to Claude CLI."
  (interactive)
  (let ((text (if (use-region-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (buffer-substring-no-properties (point-min) (point-max))))
        (buffer-name (ai-cli--get-buffer-for-project)))
    (when (string-empty-p (string-trim text))
      (user-error "No text to send"))
    (unless (get-buffer buffer-name)
      (claude))
    (if-let* ((buffer (get-buffer buffer-name)))
        (with-current-buffer buffer
          (vterm-send-string text)
          (vterm-send-return))
      (error "Failed to create or find Claude buffer"))))

;; Backup AI functions
(defun gemini ()
  "Start Gemini CLI in project root, or prompt to select directory."
  (interactive)
  (ai-cli "gemini" (or (ai-cli--find-project-root) 
                       (ai-cli--select-directory))))

(defun codex ()
  "Start Codex CLI in project root, or prompt to select directory."
  (interactive)
  (ai-cli "codex" (or (ai-cli--find-project-root) 
                      (ai-cli--select-directory))))

;; Simple transient menu
(transient-define-prefix ai-cli-menu ()
  "AI CLI Menu - Claude focused"
  ["Claude"
   ("c" "Start" claude)
   ("d" "Directory" claude-directory)
   ("s" "Send" claude-send)]
  ["Side Window"
   ("w" "Toggle" ai-cli-toggle-side-window)
   ("W" "Show" ai-cli-show-side-window)
   ("h" "Hide" ai-cli-hide-side-window)
   ("p" "Switch Project" ai-cli-switch-project-buffer)]
  ["Exit"
   ("q" "Quit" transient-quit-one)])

(provide 'ai-cli)

;;; ai-cli.el ends here
