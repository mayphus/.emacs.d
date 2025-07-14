;;; init-ai.el --- AI integration configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Comprehensive AI integration configuration including Claude, GPT, Copilot,
;; and various AI-powered tools for Emacs.

;;; Code:

(use-package gptel
  :ensure t
  :defer t
  :commands (gptel gptel-send gptel-menu)
  :bind (("C-c g" . gptel)
         ("C-c G" . gptel-menu))
  :custom
  (gptel-default-mode 'org-mode)
  (gptel-model "claude-3-5-sonnet-20241022")
  (gptel-backend (gptel-make-anthropic "Claude"
                                       :stream t
                                       :key 'gptel-api-key)))

(use-package org-ai
  :ensure t
  :defer t
  :commands (org-ai-mode org-ai-global-mode)
  :init
  (add-hook 'org-mode-hook #'org-ai-mode)
  :custom
  (org-ai-default-chat-model "claude-3-5-sonnet-20241022")
  (org-ai-default-chat-system-prompt "You are a helpful assistant.")
  :config
  (org-ai-global-mode))

(use-package claude-code-ide
  :bind (("C-c i i" . claude-code-ide)
         ("C-c i r" . claude-code-ide-resume)
         ("C-c i s" . claude-code-ide-check-status)
         ("C-c i k" . claude-code-ide-stop)
         ("C-c i b" . claude-code-ide-switch-to-buffer)
         ("C-c i l" . claude-code-ide-list-sessions)
         ("C-c i d" . claude-code-ide-select-folder))
  :config
  (defun claude-code-ide--get-working-directory ()
    "Get the current working directory (project root or Emacs config directory)."
    (if-let* ((project (project-current)))
        (expand-file-name (project-root project))
      (if (string= (expand-file-name default-directory) (expand-file-name "~/"))
          (expand-file-name user-emacs-directory)
        (expand-file-name default-directory))))

  (defun claude-code-ide-select-folder ()
    "Select a folder as the working directory for a new Claude Code session.
This allows you to choose any directory as the working directory instead of
using the current project root or default directory."
    (interactive)
    (let* ((selected-dir (read-directory-name "Select working directory for Claude Code: "
                                              default-directory nil t))
           (expanded-dir (expand-file-name selected-dir))
           (buffer-name (claude-code-ide--get-buffer-name expanded-dir))
           (existing-buffer (get-buffer buffer-name))
           (existing-process (claude-code-ide--get-process expanded-dir)))

      (unless (claude-code-ide--ensure-cli)
        (user-error "Claude Code CLI not available.  Please install it and ensure it's in PATH"))

      (unless (fboundp 'vterm)
        (user-error "The package vterm is not installed.  Please install the vterm package"))

      (require 'vterm nil t)
      (claude-code-ide--cleanup-dead-processes)

      ;; If buffer exists and process is alive, toggle the window
      (if (and existing-buffer
               (buffer-live-p existing-buffer)
               existing-process)
          (claude-code-ide--toggle-existing-window existing-buffer expanded-dir)
        ;; Start MCP server with selected directory
        (let ((port (claude-code-ide-mcp-start expanded-dir)))
          ;; Create new vterm session
          (let* ((buffer-and-process (claude-code-ide--create-vterm-session
                                      buffer-name expanded-dir port nil))
                 (buffer (car buffer-and-process))
                 (process (cdr buffer-and-process)))
            (claude-code-ide--set-process process expanded-dir)
            ;; Set up process sentinel to clean up when Claude exits
            (set-process-sentinel process
                                  (lambda (_proc event)
                                    (when (or (string-match "finished" event)
                                              (string-match "exited" event)
                                              (string-match "killed" event)
                                              (string-match "terminated" event))
                                      (claude-code-ide--cleanup-on-exit expanded-dir))))
            ;; Also add buffer kill hook as a backup
            (with-current-buffer buffer
              (add-hook 'kill-buffer-hook
                        (lambda ()
                          (claude-code-ide--cleanup-on-exit expanded-dir))
                        nil t)
              ;; Add vterm exit hook to ensure buffer is killed when process exits
              (add-hook 'vterm-exit-functions
                        (lambda (&rest _)
                          (when (buffer-live-p buffer)
                            (kill-buffer buffer)))
                        nil t))
            ;; Display the buffer in a side window
            (claude-code-ide--display-buffer-in-side-window buffer)
            (claude-code-ide-log "Claude Code started in %s with MCP on port %d%s"
                                 (file-name-nondirectory (directory-file-name expanded-dir))
                                 port
                                 (if claude-code-ide-cli-debug " (debug mode enabled)" ""))))))))

(use-package claude-code
  :bind ("C-c c" . claude-code))

(provide 'init-ai)
;;; init-ai.el ends here
