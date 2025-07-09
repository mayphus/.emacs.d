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
        auto-save-list-file-prefix (expand-file-name ".saves-" auto-save-dir))

  ;; Backup retention: keep only last 30 days
  (defun my/cleanup-old-backups ()
    "Remove backup files older than 30 days."
    (let ((cutoff-time (- (float-time) (* 30 24 60 60))))
      (dolist (dir (list backup-dir auto-save-dir))
        (when (file-directory-p dir)
          (dolist (file (directory-files dir t "^[^.]"))
            (when (and (file-regular-p file)
                       (< (float-time (nth 5 (file-attributes file))) cutoff-time))
              (delete-file file)))))))

  (run-with-timer 3600 3600 'my/cleanup-old-backups))

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

;; Theme
(use-package standard-themes
  :ensure t
  :config
  (defun my/apple-theme (appearance)
    "Set ns-appearance and theme based on system APPEARANCE."
    (when (eq system-type 'darwin)
      (pcase appearance
        ('light (set-frame-parameter nil 'ns-appearance 'light)
                (load-theme 'standard-light t)
                (let ((bg (face-background 'default)))
                  (when (and bg (not (string= bg "unspecified-bg")))
                    (set-face-background 'fringe bg)))
                (when (executable-find "claude")
                  (start-process "claude-theme" nil "claude" "config" "set" "-g" "theme" "light")))
        ('dark (set-frame-parameter nil 'ns-appearance 'dark)
               (load-theme 'standard-dark t)
               (let ((bg (face-background 'default)))
                 (when (and bg (not (string= bg "unspecified-bg")))
                   (set-face-background 'fringe bg)))
               (when (executable-find "claude")
                 (start-process "claude-theme" nil "claude" "config" "set" "-g" "theme" "dark"))))))

  (when (and (eq system-type 'darwin)
             (boundp 'ns-system-appearance-change-functions))
    (add-hook 'ns-system-appearance-change-functions #'my/apple-theme))

  ;; Set initial theme based on system appearance
  (when (eq system-type 'darwin)
    (my/apple-theme (if (string-match-p "Dark" (or (getenv "APPEARANCE") "")) 'dark 'light))))

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

;; AI

(use-package claude-code
  :ensure t
  :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
  :defer t
  :config (claude-code-mode)
  :bind-keymap ("C-c c" . claude-code-command-map)
  :custom-face
  (claude-code-repl-face ((t (:family "JuliaMono"))))
  :config
  (defun claude-code--directory-advice (orig-fun &rest args)
    "Advice to default to ~/.emacs.d when no project or file."
    (let ((result (apply orig-fun args)))
      (if (string= result "~/")
          user-emacs-directory
        result))))

(use-package claude-code-ide
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
           (or (when-let ((project (project-current)))
                 (project-root project))
               user-emacs-directory)))
      (apply orig-fun args)))

  (advice-add 'claude-code-ide :around #'my/claude-code-ide-default-to-emacs-config))

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

;; Note-taking and Organization
;; Create a proper keymap for notes
(defvar my/notes-map (make-sparse-keymap)
  "Keymap for note-taking commands.")

(define-key my/notes-map (kbd "n") 'org-capture)
(define-key my/notes-map (kbd "c") 'consult-notes)
(define-key my/notes-map (kbd "s") 'consult-notes-search-in-all-notes)
(define-key my/notes-map (kbd "f") (lambda () (interactive) (consult-find "~/workspace/notes/")))
(define-key my/notes-map (kbd "r") (lambda () (interactive) (consult-ripgrep "~/workspace/notes/")))
(define-key my/notes-map (kbd "d") 'deft)

(global-set-key (kbd "C-c n") my/notes-map)

(use-package org
  :defer t
  :mode ("\\.org\\'" . org-mode)
  :custom
  (org-startup-indented t)
  (org-pretty-entities t)
  (org-hide-emphasis-markers t)
  (org-startup-with-inline-images t)
  (org-agenda-files '("~/workspace/notes/"))
  (org-capture-templates
   '(("n" "Note" plain
      (file (lambda ()
              (let* ((title (read-string "Title: "))
                     (slug (downcase
                            (replace-regexp-in-string
                             "[^a-z0-9]+" "-"
                             (replace-regexp-in-string
                              "^-\\|-$" ""
                              (replace-regexp-in-string
                               "[^[:alnum:][:space:]]" ""
                               title))))))
                (setq org-capture-current-title title)
                (expand-file-name (concat slug ".org") "~/workspace/notes/"))))
      "#+title:      %(or org-capture-current-title \"\")\n#+date:       %U\n#+filetags:   %(my/org-capture-process-tags)\n\n%?")))
  :config
  ;; Configure org-element cache for better performance
  (setq org-element-use-cache t)
  (setq org-element-cache-persistent t)

  (defun my/org-capture-process-tags ()
    "Process tags input for org capture."
    (let* ((tags-input (read-string "Tags: "))
           (tags (when (not (string-empty-p tags-input))
                   (mapcar (lambda (tag)
                             (downcase (string-trim tag)))
                           (split-string tags-input "[, ]+" t)))))
      (if tags
          (concat ":" (mapconcat 'identity tags ":") ":")
        ""))))

;; Deft for alternative note browsing
(use-package deft
  :ensure t
  :defer t
  :commands deft
  :custom
  (deft-directory "~/workspace/notes/")
  (deft-recursive t)
  (deft-extensions '("org" "txt" "md"))
  (deft-default-extension "org")
  (deft-text-mode 'org-mode)
  (deft-use-filename-as-title t)
  (deft-use-filter-string-for-filename t)
  (deft-auto-save-interval 0)
  ;; Optimize for performance
  (deft-strip-summary-regexp "\\(\\*+\\|#+\\w+:.*\\)")
  ;; Ignore dot files, hidden files, CLAUDE.md, and README.org files
  (deft-ignore-file-regexp "\\(?:\\.\\|#\\|~\\|CLAUDE\\.md\\|README\\.org\\)"))

;;; init.el ends here
