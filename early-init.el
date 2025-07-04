;;; early-init.el --- Early initialization -*- lexical-binding: t; -*-

;;; Commentary:
;; Early initialization settings that need to be applied before the first frame
;; is created.  This includes UI element removal, frame appearance, native
;; compilation optimization, automatic theme switching, and backup configuration.

;;; Code:

(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (setq native-comp-async-report-warnings-errors nil
        native-comp-deferred-compilation t
        native-comp-speed 3
        native-comp-async-jobs 4))

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

(when (eq system-type 'darwin)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (setq ns-use-native-fullscreen t
        ns-use-thin-smoothing t))

(global-set-key (kbd "C-s-f") 'toggle-frame-fullscreen)

(setq inhibit-startup-message t
      scroll-step 1
      scroll-margin 3
      scroll-conservatively 100000
      auto-window-vscroll nil
      fast-but-imprecise-scrolling t
      scroll-preserve-screen-position t)

(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1))

(defalias 'yes-or-no-p 'y-or-n-p)

;;(set-face-background 'fringe (face-background 'default))

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

(let ((lisp-dir (expand-file-name "lisp" user-emacs-directory)))
  (when (file-directory-p lisp-dir)
    (add-to-list 'load-path lisp-dir)))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(use-package mini-echo
  :ensure t
  :config
  (mini-echo-mode 1))

(let ((backup-dir (expand-file-name "backups/backups/" user-emacs-directory))
      (auto-save-dir (expand-file-name "backups/auto-saves/" user-emacs-directory)))
  (unless (file-exists-p backup-dir)
    (make-directory backup-dir t))
  (unless (file-exists-p auto-save-dir)
    (make-directory auto-save-dir t))
  (setq backup-directory-alist `((".*" . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,auto-save-dir t))
        auto-save-list-file-prefix (expand-file-name ".saves-" auto-save-dir)))

;;; early-init.el ends here
