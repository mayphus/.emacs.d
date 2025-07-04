;;; init.el --- Emacs Configuration -*- lexical-binding: t; -*-
;;; Commentary: Clean, minimal config with completion, git, and AI assistance

;;; Code:

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
(setq dired-auto-revert-buffer t)

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

(set-face-background 'fringe (face-background 'default))

;; Version Control

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

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

(use-package which-key
  :ensure t
  :init (which-key-mode)
  :custom
  (which-key-idle-delay 0.3))

(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-symbol))

;; Development Tools

(use-package vterm
  :ensure t)

(use-package eat
  :ensure t
  :hook (eshell-load . eat-eshell-mode)
  :config
  (setq eat-enable-mouse t))

;; (use-package treesit-auto
;;   :ensure t
;;   :config
;;   (treesit-auto-add-to-auto-mode-alist 'all)
;;   (global-treesit-auto-mode))

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
    (exec-path-from-shell-initialize)))

;; AI Assistance

(use-package claude-code
  :ensure t
  :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
  :config
  (claude-code-mode)
  (custom-set-faces
   '(claude-code-repl-face ((t (:family "JuliaMono")))))

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
