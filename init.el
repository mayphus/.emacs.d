;;; init.el --- Clean, minimal Emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Emacs configuration focused on enhanced completion and AI assistance.
;; Optimized for development workflows and note-taking.

;;; Code:

(let ((backup-dir (expand-file-name "backups/backups/" user-emacs-directory))
      (auto-save-dir (expand-file-name "backups/auto-saves/" user-emacs-directory)))
  (dolist (dir (list backup-dir auto-save-dir))
    (unless (file-exists-p dir)
      (make-directory dir t)))
  (setq backup-directory-alist `((".*" . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,auto-save-dir t))
        auto-save-list-file-prefix (expand-file-name ".saves-" auto-save-dir)))

(use-package emacs
  :config
  ;; Simplify yes/no prompts
  (defalias 'yes-or-no-p 'y-or-n-p)
  ;; Auto-revert files when changed on disk
  (global-auto-revert-mode 1)
  ;; Better text wrapping handled per-mode via hooks
  ;; Silence bell
  (setq ring-bell-function 'ignore)

  (defun my/json-pretty-print-buffer ()
    "Pretty print the current JSON buffer on demand."
    (interactive)
    (require 'json)
    (if (<= (buffer-size) 50000)
        (json-pretty-print-buffer)
      (user-error "JSON buffer too large to pretty-print automatically")))

  (defun my/json-setup-json-formatting ()
    "Install handy JSON formatting helpers for the current buffer."
    (local-set-key (kbd "C-c C-f") #'my/json-pretty-print-buffer))
  :custom
  ;; Auto-revert settings
  (auto-revert-verbose nil)
  (dired-auto-revert-buffer t)
  ;; Warning levels
  (warning-minimum-level :warning)
  (byte-compile-warnings '(not docstrings))
  ;; Indentation
  (tab-width 2)
  (indent-tabs-mode nil)
  (js-indent-level 2)
  ;; Use GNU ls if available for better dired functionality
  (insert-directory-program (when (executable-find "gls") "gls"))
  :bind (("C-s-f" . toggle-frame-fullscreen)
         ("C-c e" . eshell))
  :hook ((before-save . delete-trailing-whitespace)
         (text-mode . visual-line-mode)
         (json-mode . my/json-setup-json-formatting)))

(use-package custom
  :config
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file)))

(use-package savehist
  :config
  (savehist-mode 1))

(use-package saveplace
  :config
  (save-place-mode 1))

(use-package pixel-scroll
  :if (fboundp 'pixel-scroll-precision-mode)
  :config
  (pixel-scroll-precision-mode 1))

(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

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

(use-package codex
  :load-path "lisp/codex.el"
  :commands (codex)
  :bind (("C-c c" . codex)))

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

(use-package vertico
  :ensure t
  :init (vertico-mode)
  :custom
  (vertico-cycle t)
  (vertico-resize t))

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

(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window)
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package dap-mode
  :ensure t
  :defer t
  :bind ("C-c d" . dap-debug))

(use-package eglot
  :ensure t
  :defer t
  :hook ((python-mode js-mode typescript-mode typescript-ts-mode go-mode rust-mode) . eglot-ensure))

(use-package flymake
  :ensure t
  :hook (prog-mode . flymake-mode)
  :custom
  (flymake-no-changes-timeout 0.5)
  (flymake-start-on-flymake-mode t)
  (flymake-start-on-save-buffer t)
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)
              ("C-c ! l" . flymake-show-buffer-diagnostics)
              ("C-c ! p" . flymake-show-project-diagnostics)))

(use-package treesit-auto
  :ensure t
  :demand t
  :custom
  (treesit-auto-add-to-auto-mode-alist t)
  :config
  (global-treesit-auto-mode))

(use-package vterm
  :ensure t
  :defer t
  :bind ("C-c t" . vterm)
  :hook (vterm-mode . goto-address-mode)
  :custom
  (vterm-max-scrollback 10000)
  (vterm-kill-buffer-on-exit t))

(use-package eat
  :ensure t
  :defer t
  :hook (eshell-load . eat-eshell-mode)
  :config
  (setq eat-enable-mouse t))

(use-package eshell
  :defer t
  :hook ((eshell-mode . goto-address-mode)
         (eshell-mode . (lambda ()
                          (define-key eshell-mode-map (kbd "C-r") 'consult-history)
                          (define-key eshell-mode-map (kbd "C-p") 'eshell-previous-input)
                          (define-key eshell-mode-map (kbd "C-n") 'eshell-next-input)))))


;; Org, Notes
(use-package init-org)

;;; init.el ends here
