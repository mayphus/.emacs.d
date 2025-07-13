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

(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el" :rev :newest)
  :defer t
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . copilot-accept-completion)
              ("TAB" . copilot-accept-completion)
              ("C-TAB" . copilot-accept-completion-by-word)
              ("C-<tab>" . copilot-accept-completion-by-word))
  :bind (("C-c M-c" . copilot-mode)
         ("C-c M-n" . copilot-next-completion)
         ("C-c M-p" . copilot-previous-completion)
         ("C-c M-f" . copilot-accept-completion-by-line))
  :custom
  (copilot-max-char -1)
  (copilot-indent-offset-warning-disable t)
  :config
  ;; Most mode mappings are automatic, only add if needed
  (add-to-list 'copilot-major-mode-alist '("elisp" . "emacs-lisp")))


(use-package claude-code-ide
  ;; claude code IDE integration for Emacs.
  ;; different from claude-code.el, this package provides a more IDE-like experience,
  ;; which just like claude code inside vscode experience."
  :ensure t
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :defer t
  :commands (claude-code-ide claude-code-ide-resume claude-code-ide-stop claude-code-ide-list-sessions)
  :bind (("C-c i i" . claude-code-ide)
         ("C-c i r" . claude-code-ide-resume)
         ("C-c i s" . claude-code-ide-stop)
         ("C-c i l" . claude-code-ide-list-sessions)
         :map vterm-mode-map
         ("M-<return>" . claude-code-ide-insert-newline))
  :config
  (defun my/claude-code-ide-default-to-emacs-config (orig-fun &rest args)
    "Use Emacs config folder when not in a project."
    (let ((default-directory
           (or (when-let* ((project (project-current)))
                 (project-root project))
               user-emacs-directory)))
      (apply orig-fun args)))

  (advice-add 'claude-code-ide :around #'my/claude-code-ide-default-to-emacs-config))

(provide 'init-ai)
;;; init-ai.el ends here
