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

;; my custom AI tools
(use-package claude-code
  :bind ("C-c c" . claude-code))

(provide 'init-ai)
;;; init-ai.el ends here
