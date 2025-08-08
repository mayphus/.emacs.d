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

(defcustom codex-model "gpt-4"
  "Default model to use for Codex sessions."
  :type 'string
  :group 'codex)

(defvar codex-sessions (make-hash-table :test 'equal)
  "Hash table mapping project roots to Codex session data.")

(defvar codex-current-session nil
  "Currently active Codex session.")

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
  "Start eat terminal."
  (require 'eat)
  (let ((default-directory directory))
    (eat buffer-name)
    (get-buffer buffer-name)))

(cl-defmethod codex-terminal-start ((backend (eql vterm)) buffer-name directory &rest args)
  "Start vterm terminal."
  (require 'vterm)
  (let ((default-directory directory))
    (vterm buffer-name)
    (get-buffer buffer-name)))

(cl-defmethod codex-terminal-send-command ((backend (eql eat)) process command)
  "Send command to eat terminal."
  (when (and process (process-live-p process))
    (process-send-string process (concat command "\n"))))

(cl-defmethod codex-terminal-send-command ((backend (eql vterm)) process command)
  "Send command to vterm terminal."
  (with-current-buffer (process-buffer process)
    (vterm-send-string command)
    (vterm-send-return)))

(cl-defmethod codex-terminal-get-process ((backend (eql eat)) buffer)
  "Get eat terminal process."
  (buffer-local-value 'eat-terminal-process buffer))

(cl-defmethod codex-terminal-get-process ((backend (eql vterm)) buffer)
  "Get vterm terminal process."
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
             (buffer-live-p (codex-session-buffer session)))
        session
      (codex--create-session root))))

(defun codex--create-session (project-root)
  "Create a new Codex session for PROJECT-ROOT."
  (let* ((buffer-name (codex--session-buffer-name project-root))
         (backend codex-terminal-backend)
         (buffer (codex-terminal-start backend buffer-name project-root))
         (process (codex-terminal-get-process backend buffer))
         (session (make-codex-session
                   :buffer buffer
                   :process process
                   :project-root project-root
                   :model codex-model
                   :terminal-backend backend)))
    (puthash project-root session codex-sessions)
    (with-current-buffer buffer
      (setq-local codex-current-session session))
    (codex-terminal-send-command backend process (format "%s api chat --model %s" codex-cli-executable codex-model))
    session))

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
  (when codex-auto-scroll
    (with-current-buffer buffer
      (goto-char (point-max)))))

;;;###autoload
(defun codex (&optional project-root)
  "Start or switch to Codex session for PROJECT-ROOT."
  (interactive)
  (let* ((session (codex--get-or-create-session project-root))
         (buffer (codex-session-buffer session)))
    (codex--display-buffer buffer)
    (codex--scroll-to-bottom buffer)
    (select-window (get-buffer-window buffer))
    session))

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
         (backend (codex-session-terminal-backend session)))
    (codex-terminal-send-command backend process command)
    (codex--display-buffer (codex-session-buffer session))
    (codex--scroll-to-bottom (codex-session-buffer session))))

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
         (prompt (format "Fix this error:\n%s\n\nContext:\n%s" error-msg context)))
    (codex-send-command prompt)))

;;;###autoload
(defun codex-quit ()
  "Quit the current Codex session."
  (interactive)
  (when-let* ((session (codex--get-or-create-session))
              (buffer (codex-session-buffer session)))
    (when (buffer-live-p buffer)
      (kill-buffer buffer))
    (remhash (codex-session-project-root session) codex-sessions)))

;;;###autoload
(defun codex-restart ()
  "Restart the current Codex session."
  (interactive)
  (let ((project-root (codex--get-project-root)))
    (codex-quit)
    (codex project-root)))

(defun codex-set-model (model)
  "Set the Codex MODEL for the current session."
  (interactive (list (read-string "Model: " codex-model)))
  (setq codex-model model)
  (when-let ((session (gethash (codex--get-project-root) codex-sessions)))
    (setf (codex-session-model session) model)
    (codex-send-command (format "Use model %s" model))))

(provide 'codex)

;;; codex.el ends here

