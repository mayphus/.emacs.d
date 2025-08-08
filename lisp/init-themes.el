;;; init-themes.el --- Standard dark and light theme configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Simple dark and light theme configuration that syncs with system appearance.

;;; Code:

(use-package standard-themes
  :ensure t)
  ;; :config
  ;; (setq standard-themes-bold-constructs nil
  ;;       standard-themes-italic-constructs t))

(defmacro my/set-appearance-theme (appearance theme-name)
  "Set macOS APPEARANCE, THEME-NAME, and sync Claude theme."
  `(progn
     (set-frame-parameter nil 'ns-appearance ',appearance)
     (load-theme ',theme-name t)
     (when (executable-find "claude")
       (start-process "claude-theme" nil "claude" "config" "set" "-g" "theme" ,(symbol-name appearance)))))

(defun my/handle-appearance-change (appearance)
  "Set ns-appearance and theme based on system APPEARANCE."
  (when (eq system-type 'darwin)
    (pcase appearance
      ('light (my/set-appearance-theme light standard-light))
      ('dark (my/set-appearance-theme dark standard-dark)))))

(defun my/setup-themes ()
  "Setup theme packages and appearance handling."
  ;; Setup appearance change handling
  (when (and (eq system-type 'darwin)
             (boundp 'ns-system-appearance-change-functions))
    (add-hook 'ns-system-appearance-change-functions #'my/handle-appearance-change))

  (when (eq system-type 'darwin)
    (my/handle-appearance-change
     (if (string-match-p "Dark" (or (getenv "APPEARANCE") "")) 'dark 'light))))

(provide 'init-themes)
;;; init-themes.el ends here
