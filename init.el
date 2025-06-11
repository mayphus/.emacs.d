;;; init.el --- Emacs Configuration -*- lexical-binding: t; -*-
;;; Commentary: Clean, minimal config with completion, git, and AI assistance
;;; Code:

;; Package repositories
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Add lisp directory and all subdirectories to load-path
(let ((lisp-dir (expand-file-name "lisp" user-emacs-directory)))
  (when (file-directory-p lisp-dir)
    ;; Add the main lisp directory
    (add-to-list 'load-path lisp-dir)
    ;; Add all subdirectories recursively
    (dolist (subdir (directory-files lisp-dir t "^[^.]"))
      (when (file-directory-p subdir)
        (add-to-list 'load-path subdir)))))

;; Core Settings

;; Custom file configuration
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Save command history
(savehist-mode 1)

;; Tab and indentation settings
(setq-default tab-width 2
              indent-tabs-mode nil)  ; Use spaces instead of tabs

;; JSON indentation and auto-formatting
(setq js-indent-level 2)
(add-hook 'json-mode-hook
  (lambda ()
    (when (< (buffer-size) 50000) ; Only format files < 50KB
      (add-hook 'before-save-hook 'json-pretty-print-buffer nil t))))

;; Auto-revert files when changed externally
(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)

;; Remember cursor position in files
(save-place-mode 1)

;; Use y/n instead of yes/no for confirmations
(fset 'yes-or-no-p 'y-or-n-p)

;; Automatic whitespace cleanup
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Suppress startup buffer
(setq inhibit-startup-message t)

;; Start Emacs server for external connections (emacsclient)
(require 'server)
(unless (server-running-p)
  (server-start))

;; Version Control

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

;; Completion System

;; Modern minibuffer completion
(use-package vertico
  :ensure t
  :init (vertico-mode))

(use-package marginalia
  :ensure t
  :init (marginalia-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Enhanced commands with live preview
(use-package consult
  :ensure t
  :bind (("C-s" . consult-line)
         ("C-x b" . consult-buffer)
         ("C-x C-r" . consult-recent-file)
         ("M-g g" . consult-goto-line)
         ("M-s d" . consult-find)
         ("M-s g" . consult-grep)
         ("M-s r" . consult-ripgrep)))

;; Context actions on completion candidates
(use-package embark
  :ensure t
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)))

(use-package embark-consult
  :ensure t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; In-buffer completion UI
(use-package corfu
  :ensure t
  :init (global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2))

;; Development Tools

;; Treesitter for better syntax highlighting
(use-package treesit-auto
  :ensure t
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; LSP support
(use-package eglot
  :ensure t
  :hook ((python-mode js-mode typescript-mode go-mode rust-mode) . eglot-ensure))

;; Syntax checking
;; (use-package flymake
;;   :hook (prog-mode . flymake-mode))


;; Markdown mode for better markdown display
(use-package markdown-mode
  :ensure t
  :defer t
  :mode ("\\.md\\'" . markdown-mode)
  :custom
  (markdown-fontify-code-blocks-natively t))

;; Environment

;; Import environment variables from shell
(use-package exec-path-from-shell
  :ensure t
  :custom
  (exec-path-from-shell-warn-duration-millis 2000)
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-copy-envs
     '("OPENAI_API_KEY"
       "GEMINI_API_KEY"
       "DEEPSEEK_API_KEY"))
    (exec-path-from-shell-initialize)))

;; AI Assistance

(use-package gptel
  :ensure t
  :config
  ;; Configure DeepSeek backend (default)
  (setq gptel-backend-deepseek
        (gptel-make-openai "DeepSeek"
          :host "api.deepseek.com"
          :endpoint "/chat/completions"
          :stream t
          :key (lambda () (getenv "DEEPSEEK_API_KEY"))
          :models '("deepseek-chat")))

  ;; Configure Gemini backend
  (setq gptel-backend-gemini
        (gptel-make-gemini "Gemini"
          :key (lambda () (getenv "GEMINI_API_KEY"))
          :stream t))

  ;; Set Gemini as default
  (setq gptel-model 'gemini-2.0-flash
        gptel-backend gptel-backend-gemini)

  ;; Key bindings for quick backend switching
  :bind
  (("C-c a d" . (lambda () (interactive)
                  (setq gptel-backend gptel-backend-deepseek
                        gptel-model 'deepseek-chat)
                  (message "Switched to DeepSeek backend")))
   ("C-c a g" . (lambda () (interactive)
                  (setq gptel-backend gptel-backend-gemini
                        gptel-model 'gemini-2.0-flash)
                  (message "Switched to Gemini backend")))
   ("C-c a c" . gptel-send)
   ("C-c a m" . gptel-menu)))

(use-package claude-code
  :load-path "lisp/claude-code.el"
  :config
  (claude-code-mode)

  ;; Auto-switch to claude-code buffer when it starts
  (defun claude-code-auto-switch-focus (&optional arg)
    "Switch to claude-code buffer and focus on it."
    (when-let* ((buffer (get-buffer "*claude*")))
      (let ((window (get-buffer-window buffer)))
        (if window
            (select-window window)
          (switch-to-buffer-other-window buffer)))))

  (advice-add 'claude-code :after 'claude-code-auto-switch-focus)

  :bind-keymap ("C-c c" . claude-code-command-map))



;; User Interface

;; Standard themes - elegant light/dark theme pair
(use-package standard-themes
  :ensure t
  :config
  ;; Track current theme state
  (defvar current-theme-dark nil
    "Track whether the current theme is dark.")

  (defun auto-switch-theme ()
    "Switch between standard-light
and standard-dark themes based on system appearance."
    (let ((dark-mode-p
           (string-match-p
            "Dark"
            (shell-command-to-string
             "defaults read -g AppleInterfaceStyle 2>/dev/null || echo Light"))))
      (if dark-mode-p
          (progn
            (load-theme 'standard-dark t)
            (setq current-theme-dark t))
        (progn
          (load-theme 'standard-light t)
          (setq current-theme-dark nil)))))

  (defun toggle-theme ()
    "Toggle between standard-light and standard-dark themes."
    (interactive)
    (if current-theme-dark
        (progn
          (load-theme 'standard-light t)
          (setq current-theme-dark nil)
          (message "Switched to standard-light theme"))
      (progn
        (load-theme 'standard-dark t)
        (setq current-theme-dark t)
        (message "Switched to standard-dark theme"))))

  ;; Apply system theme on startup and check periodically
  (auto-switch-theme)
  (run-with-timer 0 60 'auto-switch-theme)

  ;; Key binding for manual theme toggle
  (global-set-key (kbd "<f5>") 'toggle-theme))

;; Additional key bindings
(global-set-key (kbd "M-/") 'comment-region)

;; Key binding discovery
(use-package which-key
  :ensure t
  :defer 1
  :init (which-key-mode)
  :custom
  (which-key-idle-delay 0.3))

;; Org Mode

;; Enhanced org-mode appearance
(use-package org
  :defer t
  :custom
  (org-startup-indented t)              ; Indent text according to outline structure
  (org-pretty-entities t)               ; Show UTF8 characters for entities
  (org-hide-emphasis-markers t)         ; Hide markup characters
  (org-startup-with-inline-images t))   ; Show images by default

;; Denote - Note-taking system
(use-package denote
  :ensure t
  :custom
  (denote-directory (expand-file-name "~/workspaces/org/"))
  (denote-known-keywords '("emacs" "programming" "electronics" "article" "project" "journal"))
  (denote-infer-keywords t)
  (denote-sort-keywords t)
  (denote-file-type nil)                ; Use default .org extension
  (denote-prompts '(title keywords))    ; Prompt for title and keywords
  :bind
  (("C-c n n" . denote)
   ("C-c n c" . denote-region)          ; Create note from region
   ("C-c n N" . denote-type)
   ("C-c n d" . denote-date)
   ("C-c n z" . denote-signature)       ; Add signature to file name
   ("C-c n s" . denote-subdirectory)
   ("C-c n t" . denote-template)
   ("C-c n i" . denote-link)            ; Insert link to other note
   ("C-c n I" . denote-add-links)
   ("C-c n b" . denote-backlinks)
   ("C-c n f f" . denote-find-link)
   ("C-c n f b" . denote-find-backlink)
   ("C-c n r" . denote-rename-file)
   ("C-c n R" . denote-rename-file-using-front-matter)
   ("C-c n o" . denote-org-capture))
  :config
  ;; Enable fontification for denote links
  (denote-fontify-links-mode-maybe)
  ;; Configure silo directories
  (setq denote-dired-directories (list denote-directory))
  ;; Journal template
  (setq denote-templates '((journal . "* Daily Notes\n\n** Tasks\n\n** Notes\n\n"))))

;;; init.el ends here
