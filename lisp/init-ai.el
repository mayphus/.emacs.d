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

(use-package claude-code-ide
  :ensure t
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :bind ("C-c C-'" . claude-code-ide-menu)
  :config
  (claude-code-ide-emacs-tools-setup))

(provide 'init-ai)
;;; init-ai.el ends here
