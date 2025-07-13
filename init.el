;;; init.el --- Clean, minimal Emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Modern Emacs configuration focused on enhanced completion, git integration,
;; and AI assistance.  Optimized for development workflows and note-taking.

;;; Code:

;; Core Settings
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Backup and Auto-save Configuration
(let ((backup-dir (expand-file-name "backups/backups/" user-emacs-directory))
      (auto-save-dir (expand-file-name "backups/auto-saves/" user-emacs-directory)))
  (dolist (dir (list backup-dir auto-save-dir))
    (unless (file-exists-p dir)
      (make-directory dir t)))
  (setq backup-directory-alist `((".*" . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,auto-save-dir t))
        auto-save-list-file-prefix (expand-file-name ".saves-" auto-save-dir)))

;; Basic Behavior
(defalias 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode 1)
(savehist-mode 1)
(save-place-mode 1)
(global-visual-line-mode)

;; Disable audible bell
(setq ring-bell-function 'ignore)

;; UI and messaging settings
(setq auto-revert-verbose nil
      dired-auto-revert-buffer t
      warning-minimum-level :error
      byte-compile-warnings '(not docstrings))

;; Coding Standards
(setq-default tab-width 2
              indent-tabs-mode nil)
(setq js-indent-level 2)

;; Auto-formatting for JSON
(add-hook 'json-mode-hook
  (lambda ()
    (when (< (buffer-size) 50000)
      (add-hook 'before-save-hook 'json-pretty-print-buffer nil t))))

;; Clean up whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Scrolling
(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1))

;; Key Bindings
(global-set-key (kbd "C-s-f") 'toggle-frame-fullscreen)
(global-set-key (kbd "C-c e") 'eshell)

;; Server
(require 'server)
(unless (server-running-p)
  (server-start))

;; Themes

(use-package standard-themes
  :ensure t
  :defer t)
(use-package ef-themes
  :ensure t
  :defer t)

(use-package init-themes
  :config
  (my/setup-themes))

;; Environment Variables
(use-package exec-path-from-shell
  :ensure t
  :custom
  (exec-path-from-shell-warn-duration-millis 2000)
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-copy-envs
     '("OPENAI_API_KEY"
       "GEMINI_API_KEY"
       "DEEPSEEK_API_KEY"
       "HF_TOKEN"
       "CLOUDFLARE_API_TOKEN"))
    (exec-path-from-shell-initialize)))

;; Version Control

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

(use-package forge
  :ensure t
  :after magit)

(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode))

;; Completion System

(use-package vertico
  :ensure t
  :init (vertico-mode)
  :custom
  (vertico-cycle t)
  (vertico-resize t)
  (vertico-count 99))

(use-package marginalia
  :ensure t
  :init (marginalia-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  (orderless-matching-styles '(orderless-literal orderless-regexp))
  (orderless-component-separator "[ &]"))

(use-package consult
  :ensure t
  :bind (("C-s" . consult-line)
         ("C-x b" . consult-buffer)
         ("C-x C-b" . consult-buffer)
         ("C-x C-r" . consult-recent-file)
         ("M-y" . consult-yank-pop)
         ("M-g g" . consult-goto-line)
         ("M-g i" . consult-imenu)
         ("M-g o" . consult-outline)
         ("M-s d" . consult-find)
         ("M-s g" . consult-grep)
         ("M-s r" . consult-ripgrep)))

(use-package consult-notes
  :ensure t
  :commands (consult-notes consult-notes-search-in-all-notes)
  :custom
  (consult-notes-file-dir-sources
   `(("Notes" ?n "~/workspace/notes/")
     ("Journal" ?j "~/workspace/notes/journal/")))
  (consult-notes-use-find-command t)
  :config
  ;; Initialize the cache
  (when (fboundp 'consult-notes-cache-setup)
    (consult-notes-cache-setup))

  ;; Disable org-headings-mode to avoid cache issues
  ;; Re-enable once org-element-cache issues are resolved
  ;; (consult-notes-org-headings-mode)
  )

(use-package embark
  :ensure t
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package corfu
  :ensure t
  :init (global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-cycle t)
  (corfu-preview-current 'insert)
  (corfu-quit-no-match 'separator)
  (corfu-popupinfo-delay '(0.25 . 0.1))
  (corfu-popupinfo-hide nil)
  :config
  (corfu-popupinfo-mode))

(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block))

(use-package which-key
  :ensure t
  :init (which-key-mode))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

;; Development

(use-package flymake
  :hook (prog-mode . flymake-mode)
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)))

(use-package dap-mode
  :ensure t
  :defer t
  :bind ("C-c d" . dap-debug))

(use-package eglot
  :ensure t
  :defer t
  :hook ((python-mode js-mode typescript-mode typescript-ts-mode go-mode rust-mode) . eglot-ensure))

(use-package treesit-auto
  :ensure t
  :defer t
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; Terminal

(use-package vterm
  :ensure t
  :defer t
  :commands vterm)

(use-package eat
  :ensure t
  :defer t
  :hook (eshell-load . eat-eshell-mode)
  :config
  (setq eat-enable-mouse t))

(use-package eshell
  :defer t
  :init
  (add-hook 'eshell-mode-hook
            (lambda ()
              (define-key eshell-mode-map (kbd "C-r") 'consult-history)
              (define-key eshell-mode-map (kbd "C-p") 'eshell-previous-input)
              (define-key eshell-mode-map (kbd "C-n") 'eshell-next-input))))

;; Dired
(when (executable-find "gls")
  (setq insert-directory-program "gls"))

;; Web
(use-package xwidget
  :when (featurep 'xwidget-internal)
  :defer t
  :bind (("C-c x" . xwidget-webkit-browse-url)
         :map xwidget-webkit-mode-map
         ("h" . xwidget-webkit-back)
         ("l" . xwidget-webkit-forward)
         ("r" . xwidget-webkit-reload)
         ("q" . quit-window)
         ("g" . xwidget-webkit-browse-url)
         ("+" . xwidget-webkit-zoom-in)
         ("-" . xwidget-webkit-zoom-out)
         ("=" . xwidget-webkit-zoom-out))
  :custom
  (xwidget-webkit-enable-plugins t))

;; Meow
(use-package init-meow)

;; AI
(use-package init-ai)

;; Org, Notes
(use-package init-org)

;;; init.el ends here
