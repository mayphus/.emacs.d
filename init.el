;;; init.el --- Clean, minimal Emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Modern Emacs configuration focused on enhanced completion, git integration,
;; and AI assistance. Optimized for development workflows and note-taking.

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
(setq scroll-step 1
      scroll-margin 3
      scroll-conservatively 100000
      auto-window-vscroll nil
      fast-but-imprecise-scrolling t
      scroll-preserve-screen-position t)

(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1))

;; Key Bindings
(global-set-key (kbd "C-s-f") 'toggle-frame-fullscreen)
(global-set-key (kbd "C-c e") 'eshell)

;; Server
(require 'server)
(unless (server-running-p)
  (server-start))

;; Appearance
(defun my/apple-theme (appearance)
  "Set ns-appearance and modus theme based on system APPEARANCE."
  (when (eq system-type 'darwin)
    (pcase appearance
      ('light (set-frame-parameter nil 'ns-appearance 'light)
              (load-theme 'modus-operandi t)
              (let ((bg (face-background 'default)))
                (when (and bg (not (string= bg "unspecified-bg")))
                  (set-face-background 'fringe bg))))
      ('dark (set-frame-parameter nil 'ns-appearance 'dark)
             (load-theme 'modus-vivendi t)
             (let ((bg (face-background 'default)))
               (when (and bg (not (string= bg "unspecified-bg")))
                 (set-face-background 'fringe bg)))))))

(when (and (eq system-type 'darwin)
           (boundp 'ns-system-appearance-change-functions))
  (add-hook 'ns-system-appearance-change-functions #'my/apple-theme))

;; Environment Variables
(use-package exec-path-from-shell
  :ensure t
  :custom
  (exec-path-from-shell-warn-duration-millis 2000)
  :init
  (when (memq window-system '(mac ns x))
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
  :init (which-key-mode)
  :custom
  (which-key-idle-delay 0.3))

;; Development Tools
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

(use-package markdown-mode
  :ensure t
  :defer t
  :mode ("\\.md\\'" . markdown-mode)
  :custom
  (markdown-fontify-code-blocks-natively t))

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

;; File Management
(use-package dired-sidebar
  :ensure t
  :defer t
  :bind ("C-c d" . dired-sidebar-toggle-sidebar))

;; Web Browser
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

;; AI Assistance
(use-package claude-code
  :ensure t
  :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
  :defer t
  :config
  (claude-code-mode)
  (defun claude-code-frame-at-default-size-p ()
    "Return t if frame is at default size, nil otherwise."
    (let ((default-width 80)
          (default-height 35)
          (current-width (frame-width))
          (current-height (frame-height)))
      (and (<= (abs (- current-width default-width)) 5)
           (<= (abs (- current-height default-height)) 5))))

  (defun claude-code-auto-display ()
    "Auto-display Claude buffer only when frame is default size."
    (when (claude-code-frame-at-default-size-p)
      (delete-other-windows)
      (switch-to-buffer (current-buffer))))

  (advice-add 'display-buffer :after
              (lambda (buffer &rest _)
                (when (and (buffer-live-p buffer)
                           (string-match-p "\\*claude.*\\*" (buffer-name buffer)))
                  (with-current-buffer buffer
                    (claude-code-auto-display)))))

  (defun claude-code-toggle-sidebar ()
    "Toggle Claude Code sidebar visibility."
    (interactive)
    (let ((claude-window (get-buffer-window (current-buffer))))
      (if (and claude-window (string-match-p "\\*claude.*\\*" (buffer-name)))
          (if (window-parameter claude-window 'window-side)
              (delete-window claude-window)
            (display-buffer (current-buffer)
                           '((display-buffer-in-side-window)
                             (side . right)
                             (window-width . 0.33)
                             (window-parameters . ((no-delete-other-windows . t))))))
        (let ((claude-buf (seq-find (lambda (buf)
                                     (string-match-p "\\*claude.*\\*" (buffer-name buf)))
                                   (buffer-list))))
          (when claude-buf
            (let ((existing-window (get-buffer-window claude-buf)))
              (if existing-window
                  (delete-window existing-window)
                (display-buffer claude-buf
                               '((display-buffer-in-side-window)
                                 (side . right)
                                 (window-width . 0.33)
                                 (window-parameters . ((no-delete-other-windows . t))))))))))))

  :bind-keymap ("C-c c" . claude-code-command-map)
  :bind ("C-c C-t" . claude-code-toggle-sidebar)
  :config
  (defun claude-code--directory-advice (orig-fun &rest args)
    "Advice to default to ~/.emacs.d when no project or file."
    (let ((result (apply orig-fun args)))
      (if (string= result "~/")
          user-emacs-directory
        result)))
  
  (advice-add 'claude-code--directory :around #'claude-code--directory-advice))

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
  (add-to-list 'copilot-major-mode-alist '("elisp" . "emacs-lisp"))
  (add-to-list 'copilot-major-mode-alist '("js" . "javascript"))
  (add-to-list 'copilot-major-mode-alist '("ts" . "typescript")))

;; Note-taking and Organization
(use-package org
  :defer t
  :mode ("\\.org\\'" . org-mode)
  :custom
  (org-startup-indented t)
  (org-pretty-entities t)
  (org-hide-emphasis-markers t)
  (org-startup-with-inline-images t))

(use-package denote
  :ensure t
  :defer t
  :custom
  (denote-directory (expand-file-name "~/workspaces/org/"))
  (denote-known-keywords '("emacs" "programming" "electronics" "article" "project" "journal"))
  (denote-infer-keywords t)
  (denote-sort-keywords t)
  (denote-file-type nil)
  (denote-prompts '(title keywords))
  :bind (("C-c n n" . denote)
         ("C-c n c" . denote-region)
         ("C-c n N" . denote-type)
         ("C-c n d" . denote-date)
         ("C-c n z" . denote-signature)
         ("C-c n s" . denote-subdirectory)
         ("C-c n t" . denote-template)
         ("C-c n i" . denote-link)
         ("C-c n I" . denote-add-links)
         ("C-c n b" . denote-backlinks)
         ("C-c n f f" . denote-find-link)
         ("C-c n f b" . denote-find-backlink)
         ("C-c n r" . denote-rename-file)
         ("C-c n R" . denote-rename-file-using-front-matter)
         ("C-c n o" . denote-org-capture))
  :config
  (denote-fontify-links-mode-maybe)
  (setq denote-dired-directories (list denote-directory))
  (add-hook 'dired-mode-hook #'denote-dired-mode)
  (setq denote-templates '((journal . "* Daily Notes\n\n** Tasks\n\n** Notes\n\n"))))

;;; init.el ends here
