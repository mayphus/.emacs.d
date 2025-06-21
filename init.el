;;; init.el --- Emacs Configuration -*- lexical-binding: t; -*-
;;; Commentary: Clean, minimal config with completion, git, and AI assistance
;;; Code:

;; Package repositories
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Add lisp directory and subdirectories to load-path with safety limits
(let ((lisp-dir (expand-file-name "lisp" user-emacs-directory)))
  (when (file-directory-p lisp-dir)
    ;; Add the main lisp directory
    (add-to-list 'load-path lisp-dir)
    ;; Add subdirectories with depth limit and symlink protection
    (condition-case err
        (let ((max-depth 3)
              (visited-dirs (make-hash-table :test 'equal)))
          (defun add-lisp-subdirs (dir depth)
            "Add subdirectories to load-path with depth limit and symlink protection."
            (when (and (< depth max-depth)
                       (not (gethash (file-truename dir) visited-dirs)))
              (puthash (file-truename dir) t visited-dirs)
              (dolist (subdir (ignore-errors (directory-files dir t "^[^.]")))
                (when (and subdir
                           (file-directory-p subdir)
                           (not (file-symlink-p subdir)))
                  (add-to-list 'load-path subdir)
                  (add-lisp-subdirs subdir (1+ depth))))))
          (add-lisp-subdirs lisp-dir 0))
      (error
       (message "Warning: Failed to load some lisp subdirectories: %s"
                (error-message-string err))))))

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

;; Automatic whitespace cleanup
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Suppress startup buffer
(setq inhibit-startup-message t)

;; Smooth scrolling configuration
(setq scroll-step 1
      scroll-margin 3
      scroll-conservatively 100000
      auto-window-vscroll nil
      fast-but-imprecise-scrolling t
      scroll-preserve-screen-position t)

;; Pixel-level smooth scrolling (Emacs 29+)
(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1))

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
  :hook ((python-mode js-mode typescript-mode typescript-ts-mode go-mode rust-mode) . eglot-ensure))


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
       "DEEPSEEK_API_KEY"
       "CLOUDFLARE_API_TOKEN"))
    (exec-path-from-shell-initialize)))

;; AI Assistance

(use-package gptel
  :ensure t
  :defer t
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
  (setq gptel-model 'gemini-2.0-flash-exp
        gptel-backend gptel-backend-gemini)

  ;; Key bindings for quick backend switching
  :bind
  (("C-c a d" . (lambda () (interactive)
                  (setq gptel-backend gptel-backend-deepseek
                        gptel-model 'deepseek-chat)
                  (message "Switched to DeepSeek backend")))
   ("C-c a g" . (lambda () (interactive)
                  (setq gptel-backend gptel-backend-gemini
                        gptel-model 'gemini-2.0-flash-exp)
                  (message "Switched to Gemini backend")))
   ("C-c a c" . gptel-send)
   ("C-c a m" . gptel-menu)))

;; GitHub Copilot integration
(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el" :rev :newest)
  :defer t
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word))
  :custom
  (copilot-max-char -1)  ; No character limit
  (copilot-indent-offset-warning-disable t)
  :config
  ;; Enable copilot in various modes
  (add-to-list 'copilot-major-mode-alist '("elisp" . "emacs-lisp"))
  (add-to-list 'copilot-major-mode-alist '("js" . "javascript"))
  (add-to-list 'copilot-major-mode-alist '("ts" . "typescript"))

  ;; Auto-login reminder
  (unless (file-exists-p (expand-file-name "copilot" user-emacs-directory))
    (message "Copilot installed! Run M-x copilot-login to authenticate with GitHub"))

  ;; Bind global keys for copilot
  :bind (("C-c M-c" . copilot-mode)
         ("C-c M-n" . copilot-next-completion)
         ("C-c M-p" . copilot-previous-completion)
         ("C-c M-f" . copilot-accept-completion-by-line)))

;; Claude Code Integration
(use-package claude-code :ensure t
  :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
  :config (claude-code-mode)
  :bind-keymap ("C-c c" . claude-code-command-map))

;; User Interface

;; Standard themes - elegant light/dark theme pair
(use-package standard-themes
  :ensure t
  :config
  ;; Track current theme state
  (defvar current-theme-dark nil
    "Track whether the current theme is dark.")

  (defun get-system-appearance ()
    "Get system appearance (dark/light) cross-platform."
    (condition-case nil
      (cond
       ;; macOS
       ((eq system-type 'darwin)
        (string-match-p "Dark"
                       (shell-command-to-string
                        "defaults read -g AppleInterfaceStyle 2>/dev/null || echo Light")))
       ;; Linux with GNOME
       ((and (eq system-type 'gnu/linux)
             (executable-find "gsettings"))
        (string-match-p "dark"
                       (shell-command-to-string
                        "gsettings get org.gnome.desktop.interface gtk-theme 2>/dev/null || echo light")))
       ;; Linux with KDE
       ((and (eq system-type 'gnu/linux)
             (getenv "KDE_SESSION_VERSION"))
        (string-match-p "dark"
                       (shell-command-to-string
                        "kreadconfig5 --group General --key ColorScheme 2>/dev/null || echo light")))
       ;; Windows (basic time-based fallback)
       ((eq system-type 'windows-nt)
        (let ((hour (string-to-number (format-time-string "%H"))))
          (or (< hour 7) (> hour 19))))
       ;; Default fallback
       (t nil))
      (error nil)))

  (defun auto-switch-theme ()
    "Switch between standard-light and standard-dark themes based on system appearance."
    (condition-case err
        (let ((dark-mode-p (get-system-appearance)))
          (if dark-mode-p
              (unless current-theme-dark
                (load-theme 'standard-dark t)
                (setq current-theme-dark t))
            (when current-theme-dark
              (load-theme 'standard-light t)
              (setq current-theme-dark nil))))
      (error
       (message "Theme switching failed: %s" (error-message-string err)))))

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

  ;; Apply system theme on startup and check periodically (every 60 seconds)
  (condition-case nil
      (auto-switch-theme)
    (error (message "Initial theme setup failed, using default")))
  (run-with-timer 0 60 'auto-switch-theme)  ; Check every 60 seconds

  ;; Key binding for manual theme toggle
  (global-set-key (kbd "<f5>") 'toggle-theme))

;; Additional key bindings
(global-set-key (kbd "C-c /") 'comment-region)

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
  (add-hook 'dired-mode-hook #'denote-dired-mode)
  ;; Journal template
  (setq denote-templates '((journal . "* Daily Notes\n\n** Tasks\n\n** Notes\n\n"))))

;;; init.el ends here
