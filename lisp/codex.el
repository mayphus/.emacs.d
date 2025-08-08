;;; codex.el --- OpenAI Codex CLI integration for Emacs  -*- lexical-binding: t; -*-

;; Author: Emacs User
;; Version: 1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, ai, openai, codex
;; URL: https://github.com/openai/openai-codex

;;; Commentary:
;;
;; This package provides integration with OpenAI Codex CLI, similar to how
;; claude-code.el integrates with Claude Code CLI.
;;
;; Features:
;; - Start and manage Codex CLI sessions
;; - Send regions, buffers, and commands to Codex
;; - Toggle Codex window visibility
;; - Fix errors at point using Codex
;; - Support for multiple terminal backends (eat, vterm)
;;
;; Setup:
;; 1. Install OpenAI Codex CLI
;; 2. Set your OpenAI API key in environment or via auth-source
;; 3. Optionally configure keybindings:
;;    (global-set-key (kbd "C-c x") #'codex)
;;    (global-set-key (kbd "C-c X") #'codex-send-region)
;;
;;; Code:

(require 'project)
(require 'cl-generic)

(defgroup codex nil
  "OpenAI Codex CLI integration."
  :group 'tools
  :prefix "codex-")

(defcustom codex-cli-executable "openai"
  "Executable for the OpenAI CLI."
  :type 'string
  :group 'codex)

(defcustom codex-api-key-var "OPENAI_API_KEY"
  "Environment variable containing OpenAI API key."
  :type 'string
  :group 'codex)

(defcustom codex-terminal-backend 'eat
  "Terminal backend to use for Codex sessions."
  :type '(choice (const :tag "Eat terminal" eat)
                 (const :tag "Vterm" vterm))
  :group 'codex)

(defcustom codex-window-side 'right
  "Side of the frame to display the Codex window."
  :type '(choice (const left) (const right) (const bottom) (const top))
  :group 'codex)

(defcustom codex-window-width 80
  "Width of the Codex window when displayed on the side."
  :type 'integer
  :group 'codex)

(defcustom codex-window-height 20
  "Height of the Codex window when displayed at top/bottom."
  :type 'integer
  :group 'codex)

(defcustom codex-auto-scroll t
  "Whether to auto-scroll Codex terminal to bottom."
  :type 'boolean
  :group 'codex)

(defcustom codex-context-lines 10
  "Number of lines of context to include around point."
  :type 'integer
  :group 'codex)

(defcustom codex-max-context-chars 2000
  "Maximum characters to include in context."
  :type 'integer
  :group 'codex)

(defcustom codex-include-project-context t
  "Whether to include project information in prompts."
  :type 'boolean
  :group 'codex)

(defcustom codex-model "gpt-4"
  "Default model to use for Codex sessions.
Recommended models: gpt-4, gpt-4-turbo, gpt-3.5-turbo."
  :type '(choice (const "gpt-4")
                 (const "gpt-4-turbo")
                 (const "gpt-3.5-turbo")
                 (string :tag "Custom model"))
  :group 'codex)

(defvar codex-sessions (make-hash-table :test 'equal)
  "Hash table mapping project roots to Codex session data.")

(defvar codex-current-session nil
  "Currently active Codex session.")

(defvar codex-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c c c") #'codex-chat)
    (define-key map (kbd "C-c c e") #'codex-explain-code)
    (define-key map (kbd "C-c c f") #'codex-fix-error-at-point)
    (define-key map (kbd "C-c c o") #'codex-optimize-code)
    (define-key map (kbd "C-c c d") #'codex-generate-docstring)
    (define-key map (kbd "C-c c t") #'codex-generate-tests)
    (define-key map (kbd "C-c c s") #'codex-send-region)
    (define-key map (kbd "C-c c b") #'codex-send-buffer)
    (define-key map (kbd "C-c c F") #'codex-send-file)
    (define-key map (kbd "C-c c w") #'codex-toggle-window)
    (define-key map (kbd "C-c c q") #'codex-quit)
    (define-key map (kbd "C-c c r") #'codex-restart)
    (define-key map (kbd "C-c c m") #'codex-set-model)
    (define-key map (kbd "C-c c R") #'codex-refactor)
    (define-key map (kbd "C-c c v") #'codex-code-review)
    (define-key map (kbd "C-c c p") #'codex-ask-about-project)
    map)
  "Keymap for Codex commands.")

;;;###autoload
(define-minor-mode codex-mode
  "Minor mode for Codex integration."
  :lighter " Codex"
  :keymap codex-mode-map
  :global t)

(cl-defstruct codex-session
  "Structure representing a Codex session."
  buffer
  process
  project-root
  model
  terminal-backend)

(cl-defgeneric codex-terminal-start (backend buffer-name directory &rest args)
  "Start terminal with BACKEND in BUFFER-NAME at DIRECTORY with ARGS.")

(cl-defgeneric codex-terminal-send-command (backend process command)
  "Send COMMAND to PROCESS using BACKEND.")

(cl-defgeneric codex-terminal-get-process (backend buffer)
  "Get terminal process from BUFFER using BACKEND.")

(cl-defmethod codex-terminal-start ((backend (eql eat)) buffer-name directory &rest args)
  "Start eat terminal.

BACKEND is the terminal backend type.
BUFFER-NAME is the name of the buffer to create.
DIRECTORY is the working directory.
ARGS are additional arguments (unused)."
  (require 'eat)
  (let ((default-directory directory))
    (eat buffer-name)
    (get-buffer buffer-name)))

(cl-defmethod codex-terminal-start ((backend (eql vterm)) buffer-name directory &rest args)
  "Start vterm terminal.

BACKEND is the terminal backend type.
BUFFER-NAME is the name of the buffer to create.
DIRECTORY is the working directory.
ARGS are additional arguments (unused)."
  (require 'vterm)
  (let ((default-directory directory))
    (vterm buffer-name)
    (get-buffer buffer-name)))

(cl-defmethod codex-terminal-send-command ((backend (eql eat)) process command)
  "Send command to eat terminal.

BACKEND is the terminal backend type.
PROCESS is the terminal process.
COMMAND is the command string to send."
  (condition-case err
      (when (and process (process-live-p process))
        (process-send-string process (concat command "\n"))
        t)
    (error
     (message "Error sending command to eat terminal: %s" (error-message-string err))
     nil)))

(cl-defmethod codex-terminal-send-command ((backend (eql vterm)) process command)
  "Send command to vterm terminal.

BACKEND is the terminal backend type.
PROCESS is the terminal process.
COMMAND is the command string to send."
  (condition-case err
      (when (and process (process-live-p process))
        (with-current-buffer (process-buffer process)
          (vterm-send-string command)
          (vterm-send-return)
          t))
    (error
     (message "Error sending command to vterm terminal: %s" (error-message-string err))
     nil)))

(cl-defmethod codex-terminal-get-process ((backend (eql eat)) buffer)
  "Get eat terminal process.

BACKEND is the terminal backend type.
BUFFER is the terminal buffer."
  (buffer-local-value 'eat-terminal-process buffer))

(cl-defmethod codex-terminal-get-process ((backend (eql vterm)) buffer)
  "Get vterm terminal process.

BACKEND is the terminal backend type.
BUFFER is the terminal buffer."
  (get-buffer-process buffer))

(defun codex--get-project-root ()
  "Get the current project root directory."
  (or (when-let ((project (project-current)))
        (project-root project))
      default-directory))

(defun codex--session-buffer-name (project-root)
  "Generate buffer name for Codex session in PROJECT-ROOT."
  (format "*codex-%s*" (file-name-nondirectory (directory-file-name project-root))))

(defun codex--get-or-create-session (&optional project-root)
  "Get or create a Codex session for PROJECT-ROOT."
  (let* ((root (or project-root (codex--get-project-root)))
         (session (gethash root codex-sessions)))
    (if (and session
             (codex-session-buffer session)
             (buffer-live-p (codex-session-buffer session))
             (codex-session-process session)
             (process-live-p (codex-session-process session)))
        session
      ;; Clean up dead session if exists
      (when session
        (remhash root codex-sessions))
      (codex--create-session root))))

(defun codex--create-session (project-root)
  "Create a new Codex session for PROJECT-ROOT."
  (condition-case err
      (let* ((buffer-name (codex--session-buffer-name project-root))
             (backend codex-terminal-backend)
             (buffer (codex-terminal-start backend buffer-name project-root))
             (process (codex-terminal-get-process backend buffer)))
        (unless process
          (error "Failed to start terminal process"))
        (let ((session (make-codex-session
                       :buffer buffer
                       :process process
                       :project-root project-root
                       :model codex-model
                       :terminal-backend backend)))
          (puthash project-root session codex-sessions)
          (with-current-buffer buffer
            (setq-local codex-current-session session))
          ;; Initialize session with environment check
          (codex--initialize-session session)
          session))
    (error
     (message "Error creating Codex session: %s" (error-message-string err))
     nil)))

(defun codex--initialize-session (session)
  "Initialize a Codex SESSION with proper setup."
  (let* ((backend (codex-session-terminal-backend session))
         (process (codex-session-process session))
         (model (codex-session-model session)))
    ;; Check CLI availability
    (codex-terminal-send-command backend process (format "which %s || echo 'ERROR: %s not found in PATH'" codex-cli-executable codex-cli-executable))
    ;; Check API key
    (codex-terminal-send-command backend process (format "if [ -z \"$%s\" ]; then echo 'WARNING: %s environment variable not set'; fi" codex-api-key-var codex-api-key-var))
    ;; Welcome message
    (codex-terminal-send-command backend process (format "echo 'Codex session ready - Model: %s'" model))))

(defun codex--display-buffer (buffer)
  "Display Codex BUFFER according to window settings."
  (let ((display-buffer-alist
         `((,(regexp-quote (buffer-name buffer))
            (display-buffer-in-side-window)
            (side . ,codex-window-side)
            (window-width . ,codex-window-width)
            (window-height . ,codex-window-height)
            (slot . 0)))))
    (display-buffer buffer)))

(defun codex--scroll-to-bottom (buffer)
  "Scroll BUFFER to bottom if auto-scroll is enabled."
  (when (and codex-auto-scroll (buffer-live-p buffer))
    (condition-case nil
        (with-current-buffer buffer
          (goto-char (point-max))
          (when-let ((window (get-buffer-window buffer)))
            (set-window-point window (point-max))))
      (error nil))))

;;;###autoload
(defun codex (&optional project-root)
  "Start or switch to Codex session for PROJECT-ROOT."
  (interactive)
  (if-let ((session (codex--get-or-create-session project-root)))
      (let ((buffer (codex-session-buffer session)))
        (codex--display-buffer buffer)
        (codex--scroll-to-bottom buffer)
        (select-window (get-buffer-window buffer))
        session)
    (user-error "Failed to create or access Codex session")))

;;;###autoload
(defun codex-send-region (start end)
  "Send region from START to END to Codex."
  (interactive "r")
  (let* ((text (buffer-substring-no-properties start end))
         (session (codex--get-or-create-session))
         (process (codex-session-process session))
         (backend (codex-session-terminal-backend session)))
    (codex-terminal-send-command backend process text)
    (codex--display-buffer (codex-session-buffer session))
    (codex--scroll-to-bottom (codex-session-buffer session))))

;;;###autoload
(defun codex-send-buffer ()
  "Send entire current buffer to Codex."
  (interactive)
  (codex-send-region (point-min) (point-max)))

;;;###autoload
(defun codex-send-command (command)
  "Send COMMAND to Codex."
  (interactive "sCodex command: ")
  (let* ((session (codex--get-or-create-session))
         (process (codex-session-process session))
         (backend (codex-session-terminal-backend session))
         (model (codex-session-model session))
         ;; Escape single quotes in command for shell safety
         (escaped-command (replace-regexp-in-string "'" "'\"'\"'" command))
         (formatted-command (format "%s api chat.completions.create -m %s -g user '%s'"
                                   codex-cli-executable model escaped-command)))
    (codex-terminal-send-command backend process formatted-command)
    (codex--display-buffer (codex-session-buffer session))
    (codex--scroll-to-bottom (codex-session-buffer session))))

;;;###autoload
(defun codex-chat (prompt)
  "Send a chat PROMPT to Codex using proper API format."
  (interactive "sChat with Codex: ")
  (codex-send-command prompt))

;;;###autoload
(defun codex-explain-code ()
  "Ask Codex to explain the current region or function at point."
  (interactive)
  (let* ((bounds (if (use-region-p)
                    (cons (region-beginning) (region-end))
                  (bounds-of-thing-at-point 'defun)))
         (code (if bounds
                  (buffer-substring-no-properties (car bounds) (cdr bounds))
                (user-error "No code selected or function at point")))
         (context (codex--get-context-info))
         (prompt (format "Please explain this code:\n\n%s```%s\n%s\n```"
                        context
                        (or (file-name-extension (buffer-file-name)) "")
                        code)))
    (codex-chat prompt)))

;;;###autoload
(defun codex-toggle-window ()
  "Toggle visibility of Codex window."
  (interactive)
  (let* ((session (codex--get-or-create-session))
         (buffer (codex-session-buffer session))
         (window (get-buffer-window buffer)))
    (if window
        (delete-window window)
      (codex--display-buffer buffer))))

;;;###autoload
(defun codex-fix-error-at-point ()
  "Ask Codex to fix the error at point."
  (interactive)
  (let* ((error-msg (or (get-text-property (point) 'help-echo)
                        (thing-at-point 'line t)))
         (context (buffer-substring-no-properties
                   (max (point-min) (- (point) 500))
                   (min (point-max) (+ (point) 500))))
         (filename (file-name-nondirectory (or (buffer-file-name) "unknown")))
         (prompt (format "I have an error in %s:\n\nError: %s\n\nContext:\n```\n%s\n```\n\nPlease suggest a fix." 
                        filename error-msg context)))
    (codex-chat prompt)))

;;;###autoload
(defun codex-optimize-code ()
  "Ask Codex to optimize the selected code or function at point."
  (interactive)
  (let* ((bounds (if (use-region-p)
                    (cons (region-beginning) (region-end))
                  (bounds-of-thing-at-point 'defun)))
         (code (if bounds
                  (buffer-substring-no-properties (car bounds) (cdr bounds))
                (user-error "No code selected or function at point")))
         (context (codex--get-context-info))
         (prompt (format "Please optimize this code for better performance and readability:\n\n%s```%s\n%s\n```" 
                        context
                        (or (file-name-extension (buffer-file-name)) "")
                        code)))
    (codex-chat prompt)))

;;;###autoload
(defun codex-quit ()
  "Quit the current Codex session."
  (interactive)
  (let ((project-root (codex--get-project-root)))
    (when-let ((session (gethash project-root codex-sessions)))
      (let ((buffer (codex-session-buffer session))
            (process (codex-session-process session)))
        ;; Clean up process
        (when (and process (process-live-p process))
          (delete-process process))
        ;; Kill buffer
        (when (and buffer (buffer-live-p buffer))
          (kill-buffer buffer))
        ;; Remove from sessions
        (remhash project-root codex-sessions)
        (message "Codex session terminated")))))

;;;###autoload
(defun codex-restart ()
  "Restart the current Codex session."
  (interactive)
  (let ((project-root (codex--get-project-root)))
    (codex-quit)
    (codex project-root)))

(defun codex-set-model (model)
  "Set the Codex MODEL for the current session."
  (interactive (list (completing-read "Model: " 
                                     '("gpt-4" "gpt-4-turbo" "gpt-3.5-turbo") 
                                     nil nil codex-model)))
  (setq codex-model model)
  (when-let ((session (gethash (codex--get-project-root) codex-sessions)))
    (setf (codex-session-model session) model)
    (message "Codex model set to: %s" model)))

;;;###autoload
(defun codex-send-file ()
  "Send current file content to Codex with context."
  (interactive)
  (let* ((filename (file-name-nondirectory (or (buffer-file-name) "untitled")))
         (content (buffer-substring-no-properties (point-min) (point-max)))
         (context (codex--get-context-info))
         (prompt (format "Here's the content of %s:\n\n%s```%s\n%s\n```\n\nPlease review and provide feedback." 
                        filename context
                        (or (file-name-extension (buffer-file-name)) "")
                        content)))
    (codex-chat prompt)))

;;;###autoload
(defun codex-generate-docstring ()
  "Generate documentation for the function at point."
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'defun))
         (func (if bounds
                  (buffer-substring-no-properties (car bounds) (cdr bounds))
                (user-error "No function at point")))
         (context (codex--get-context-info))
         (prompt (format "Generate proper documentation/docstring for this function:\n\n%s```%s\n%s\n```" 
                        context
                        (or (file-name-extension (buffer-file-name)) "")
                        func)))
    (codex-chat prompt)))

;;;###autoload
(defun codex-generate-tests ()
  "Generate test for the selected code or function at point."
  (interactive)
  (let* ((bounds (if (use-region-p)
                    (cons (region-beginning) (region-end))
                  (bounds-of-thing-at-point 'defun)))
         (code (if bounds
                  (buffer-substring-no-properties (car bounds) (cdr bounds))
                (user-error "No code selected or function at point")))
         (context (codex--get-context-info))
         (prompt (format "Generate comprehensive tests for this code:\n\n%s```%s\n%s\n```"
                        context
                        (or (file-name-extension (buffer-file-name)) "")
                        code)))
    (codex-chat prompt)))

(defun codex--get-context-info ()
  "Get context information about current buffer and project."
  (let* ((filename (when (buffer-file-name)
                     (file-name-nondirectory (buffer-file-name))))
         (project-name (when-let ((project (project-current)))
                         (file-name-nondirectory 
                          (directory-file-name (project-root project)))))
         (language (or (file-name-extension (or (buffer-file-name) ""))
                      (symbol-name major-mode)))
         (context-parts '()))
    (when project-name
      (push (format "Project: %s" project-name) context-parts))
    (when filename
      (push (format "File: %s" filename) context-parts))
    (when language
      (push (format "Language: %s" language) context-parts))
    (if context-parts
        (concat (mapconcat #'identity (reverse context-parts) ", ") "\n\n")
      "")))

;;;###autoload
(defun codex-refactor ()
  "Ask Codex to refactor the selected code or function."
  (interactive)
  (let* ((bounds (if (use-region-p)
                    (cons (region-beginning) (region-end))
                  (bounds-of-thing-at-point 'defun)))
         (code (if bounds
                  (buffer-substring-no-properties (car bounds) (cdr bounds))
                (user-error "No code selected or function at point")))
         (context (codex--get-context-info))
         (prompt (format "Please refactor this code to improve readability, maintainability, and follow best practices:\n\n%s```%s\n%s\n```" 
                        context
                        (or (file-name-extension (buffer-file-name)) "")
                        code)))
    (codex-chat prompt)))

;;;###autoload
(defun codex-code-review ()
  "Ask Codex to perform a code review of current selection or function."
  (interactive)
  (let* ((bounds (if (use-region-p)
                    (cons (region-beginning) (region-end))
                  (bounds-of-thing-at-point 'defun)))
         (code (if bounds
                  (buffer-substring-no-properties (car bounds) (cdr bounds))
                (user-error "No code selected or function at point")))
         (context (codex--get-context-info))
         (prompt (format "Please perform a thorough code review of this code, checking for bugs, performance issues, security concerns, and best practices:\n\n%s```%s\n%s\n```" 
                        context
                        (or (file-name-extension (buffer-file-name)) "")
                        code)))
    (codex-chat prompt)))

;;;###autoload
(defun codex-ask-about-project ()
  "Ask Codex a question about the current project."
  (interactive)
  (let* ((project-root (codex--get-project-root))
         (project-name (file-name-nondirectory (directory-file-name project-root)))
         (question (read-string "Ask about project: "))
         (context (format "Project: %s\nWorking directory: %s\n\nQuestion: %s" 
                         project-name project-root question)))
    (codex-chat context)))

(provide 'codex)

;;; codex.el ends here

