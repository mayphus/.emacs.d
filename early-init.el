;;; early-init.el --- Early initialization -*- lexical-binding: t; -*-

;;; Commentary:
;; Early initialization settings that need to be applied before the first frame
;; is created.  This includes UI element removal and frame appearance.

;;; Code:

;; Hide UI elements before frame creation
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Toggle fullscreen with C-s-f
(global-set-key (kbd "C-s-f") 'toggle-frame-fullscreen)

;; Configure backup and auto-save directories
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
