;;; init.el --- Mayphus Tang's Emacs config -*- lexical-binding: t -*-

;; Author: Mayphus Tang
;; Maintainer: Mayphus Tang
;; Version: 0.0.1
;; Package-Requires:
;; Homepage: https://github.com/mayphus/.emacs.d
;; Keywords: mayphus emacs init

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the MIT License.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; MIT License for more details.

;; For a full copy of the MIT License
;; see <https://opensource.org/licenses/MIT>

;;; Commentary:
;; Felix's Emacs Config

;;; Code:

;; Add folder and subdirs to the `load-path'
(defun my/add-folder-to-load-path (folder)
  "Add FOLDER and its subdirectories to the `load-path'."
  (let ((default-directory folder))
    (normal-top-level-add-to-load-path '("."))
    (normal-top-level-add-subdirs-to-load-path)))

(my/add-folder-to-load-path (expand-file-name "lisp" user-emacs-directory))

;; Toggle fullscreen with Ctrl + Command + F
(global-set-key (kbd "C-s-f") #'toggle-frame-fullscreen)

;; Optimize garbage collection during startup
(defun my/set-gc-threshold ()
  "Set `gc-cons-threshold' to a more reasonable value after startup."
  (setq gc-cons-threshold 16777216 ; 16MB
        gc-cons-percentage 0.1))

(let ((gc-cons-threshold most-positive-fixnum)
      (gc-cons-percentage 0.6))
  (add-hook 'emacs-startup-hook #'my/set-gc-threshold))

;; Increase file-name-handler-alist performance
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist default-file-name-handler-alist)))

;; Initial settings
(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(setq ring-bell-function 'ignore)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

;; Set async-shell-command-buffer to 'new-buffer
(setq async-shell-command-buffer 'new-buffer)

;; Set workspace directory
(setq workspace-directory (expand-file-name "workspaces" "~"))

;; Disable menu bar in terminal frames
(defun my/disable-menu-bar-for-terminal-frames (frame)
  "Disable the menu bar in terminal frames."
  (unless (display-graphic-p frame)
    (set-frame-parameter frame 'menu-bar-lines 0)))
(add-hook 'after-make-frame-functions 'my/disable-menu-bar-for-terminal-frames)

;; Disable tool bar and scroll bar
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Enable visual line mode
(global-visual-line-mode)
(midnight-mode)

;; Set dired to use 'gls' if available
(when (executable-find "gls")
  (setq insert-directory-program "gls" dired-use-ls-dired t))

;; Load private settings if they exist
(when (file-exists-p
       (expand-file-name "init-private.el" user-emacs-directory))
  (load-file (expand-file-name
	      "init-private.el" user-emacs-directory)))

;; Start server if not already running
(require 'server)
(unless (server-running-p)
  (server-start))

;; Now, Load
;; essential packages and configurations
(require 'init-package)  ; Package management
(require 'init-path)     ; Path configuration

;; Core functionality and UI enhancements
(require 'init-theme)    ; Theme configuration
(require 'init-window)   ; Window management
(require 'init-meow)     ; Modal editing
(require 'init-which-key); Key binding help
;; (require 'init-rainbow-delimiters) ; Delimiter highlighting

;; Completion and search
(setq completion-styles '(orderless))
(marginalia-mode)
(require 'init-vertico)  ; Vertical completion UI
(require 'init-embark)   ; Context-aware commands
(require 'init-consult)  ; Search and navigation commands

;; Text editing and navigation
;; (require 'init-smart-hungry-delete) ; Intelligent backspace
;; (require 'init-expand-region)       ; Expand selection

;; Snippet system
(require 'init-yasnippet)           ; Snippet system

;; Version control and project management
(require 'init-magit)    ; Git interface

;; Org-mode and note-taking
(require 'init-org)      ; Org-mode configuration

;; Development tools
(require 'init-tramp)    ; Remote file editing
(require 'init-epa)      ; Encryption and decryption
(require 'init-geiser)   ; Scheme development

;; AI assistance
(require 'init-ellama)   ; AI language model integration

;; Database interaction
(require 'edbi)          ; Database interface

;; Terminal emulation
(require 'init-vterm)

;; Load custom settings
(load-file custom-file)

(provide 'init)

;;; init.el ends here
