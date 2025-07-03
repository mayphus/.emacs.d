;;; init.el --- Emacs Configuration -*- lexical-binding: t; -*-
;;; Commentary: Clean, minimal config with completion, git, and AI assistance
;;; Code:

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(let ((lisp-dir (expand-file-name "lisp" user-emacs-directory)))
  (when (file-directory-p lisp-dir)
    (add-to-list 'load-path lisp-dir)
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

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(savehist-mode 1)

(setq-default tab-width 2
              indent-tabs-mode nil)

(setq js-indent-level 2)
(add-hook 'json-mode-hook
  (lambda ()
    (when (< (buffer-size) 50000)
      (add-hook 'before-save-hook 'json-pretty-print-buffer nil t))))

(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)

(save-place-mode 1)

(defalias 'yes-or-no-p 'y-or-n-p)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq inhibit-startup-message t)

(setq scroll-step 1
      scroll-margin 3
      scroll-conservatively 100000
      auto-window-vscroll nil
      fast-but-imprecise-scrolling t
      scroll-preserve-screen-position t)

(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1))

(require 'server)
(unless (server-running-p)
  (server-start))

;; Version Control

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

;; Completion System

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

(use-package consult
  :ensure t
  :bind (("C-s" . consult-line)
         ("C-x b" . consult-buffer)
         ("C-x C-r" . consult-recent-file)
         ("M-g g" . consult-goto-line)
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
  (corfu-auto-delay 0.2))

(use-package which-key
  :ensure t
  :init (which-key-mode)
  :custom
  (which-key-idle-delay 0.3))


;; Development Tools

(use-package vterm
  :ensure t)

(use-package eat
  :ensure t
  :hook (eshell-load . eat-eshell-mode)
  :config
  (setq eat-enable-mouse t))

(use-package treesit-auto
  :ensure t
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package eglot
  :ensure t
  :hook ((python-mode js-mode typescript-mode typescript-ts-mode go-mode rust-mode) . eglot-ensure))


(use-package markdown-mode
  :ensure t
  :defer t
  :mode ("\\.md\\'" . markdown-mode)
  :custom
  (markdown-fontify-code-blocks-natively t))

;; Environment

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
  :defer t
  :config
  (setq gptel-model "gemini-1.5-flash")
  (when (getenv "GEMINI_API_KEY")
    (setq gptel-api-key (getenv "GEMINI_API_KEY"))
    (setq gptel-backend
          (gptel-make-gemini "Gemini"
            :key gptel-api-key
            :models '("gemini-1.5-flash" "gemini-1.5-pro")
            :stream t)))
  (when (getenv "DEEPSEEK_API_KEY")
    (setq gptel-deepseek
          (gptel-make-openai "DeepSeek"
            :host "api.deepseek.com"
            :key (getenv "DEEPSEEK_API_KEY")
            :models '("deepseek-chat" "deepseek-coder")
            :stream t))))

;; install claude-code.el
(use-package claude-code
  :ensure t
  :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
  :config (claude-code-mode)
  :bind-keymap ("C-c c" . claude-code-command-map))

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
  (add-to-list 'copilot-major-mode-alist '("elisp" . "emacs-lisp"))
  (add-to-list 'copilot-major-mode-alist '("js" . "javascript"))
  (add-to-list 'copilot-major-mode-alist '("ts" . "typescript"))

  :bind (("C-c M-c" . copilot-mode)
         ("C-c M-n" . copilot-next-completion)
         ("C-c M-p" . copilot-previous-completion)
         ("C-c M-f" . copilot-accept-completion-by-line)))

;; User Interface

(use-package standard-themes
  :ensure t
  :config
  (defvar current-theme-dark nil)

  (defun get-system-appearance ()
    "Get system dark mode preference."
    (condition-case nil
        (cond
         ((eq system-type 'darwin)
          (string-match-p "Dark" (shell-command-to-string
                                 "defaults read -g AppleInterfaceStyle 2>/dev/null || echo Light")))
         ((and (eq system-type 'gnu/linux) (executable-find "gsettings"))
          (string-match-p "dark" (shell-command-to-string
                                 "gsettings get org.gnome.desktop.interface gtk-theme 2>/dev/null || echo light")))
         (t nil))
      (error nil)))

  (defun auto-switch-theme ()
    "Auto-switch theme based on system appearance."
    (let ((dark-p (get-system-appearance)))
      (when (not (eq dark-p current-theme-dark))
        (load-theme (if dark-p 'standard-dark 'standard-light) t)
        (setq current-theme-dark dark-p))))

  (defun toggle-theme ()
    "Toggle between light and dark themes."
    (interactive)
    (load-theme (if current-theme-dark 'standard-light 'standard-dark) t)
    (setq current-theme-dark (not current-theme-dark)))

  (auto-switch-theme)
  (run-with-timer 0 60 'auto-switch-theme)
  (global-set-key (kbd "<f5>") 'toggle-theme))

(global-set-key (kbd "C-c /") 'comment-region)


;; Org Mode

(use-package org
  :defer t
  :custom
  (org-startup-indented t)
  (org-pretty-entities t)
  (org-hide-emphasis-markers t)
  (org-startup-with-inline-images t))

(use-package denote
  :ensure t
  :custom
  (denote-directory (expand-file-name "~/workspaces/org/"))
  (denote-known-keywords '("emacs" "programming" "electronics" "article" "project" "journal"))
  (denote-infer-keywords t)
  (denote-sort-keywords t)
  (denote-file-type nil)
  (denote-prompts '(title keywords))
  :bind
  (("C-c n n" . denote)
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
