;;; early-init.el --- Early initialization -*- lexical-binding: t; -*-

;;; Commentary:
;; Early initialization settings that need to be applied before the first frame
;; is created.  This includes UI element removal and frame appearance.

;;; Code:

(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (setq native-comp-async-report-warnings-errors nil
        native-comp-deferred-compilation t
        native-comp-speed 2
        native-comp-async-jobs 4)
  (add-to-list 'native-comp-eln-load-path
               (expand-file-name "eln-cache/" user-emacs-directory)))

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

(when (eq system-type 'darwin)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (setq ns-use-native-fullscreen t
        ns-use-thin-smoothing t))

(global-set-key (kbd "C-s-f") 'toggle-frame-fullscreen)

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
