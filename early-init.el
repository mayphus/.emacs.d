;;; early-init.el --- Early initialization -*- lexical-binding: t; -*-

;;; Commentary:
;; Early initialization settings that need to be applied before the first frame
;; is created.  This includes UI element removal and frame appearance.

;;; Code:

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

(when (eq system-type 'darwin)
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (setq ns-use-native-fullscreen t
        ns-use-thin-smoothing t))

(global-set-key (kbd "C-s-f") 'toggle-frame-fullscreen)

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
