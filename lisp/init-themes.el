;;; init-themes.el --- Theme configuration with daily rotation -*- lexical-binding: t; -*-

;;; Commentary:
;; Theme configuration with ef-themes daily rotation and system appearance sync.

;;; Code:

;; Daily theme rotation lists
(defvar my/ef-light-themes
  '(ef-arbutus ef-cyprus ef-day ef-deuteranopia-light ef-duo-light
    ef-eagle ef-elea-light ef-frost ef-kassio ef-light
    ef-maris-light ef-melissa-light ef-reverie ef-spring
    ef-summer ef-trio-light ef-tritanopia-light)
  "List of ef-themes light variants for daily rotation.")

(defvar my/ef-dark-themes
  '(ef-autumn ef-bio ef-cherie ef-dark ef-deuteranopia-dark
    ef-dream ef-duo-dark ef-elea-dark ef-maris-dark ef-melissa-dark
    ef-night ef-owl ef-rosa ef-symbiosis ef-trio-dark
    ef-tritanopia-dark ef-winter)
  "List of ef-themes dark variants for daily rotation.")

(defun my/get-daily-theme (theme-list)
  "Get a theme from THEME-LIST based on the current day of the year."
  (let ((day-of-year (string-to-number (format-time-string "%j"))))
    (nth (mod day-of-year (length theme-list)) theme-list)))

(defmacro my/set-appearance-theme (appearance theme-name)
  "Set macOS APPEARANCE, THEME-NAME, and sync Claude theme."
  `(progn
     (set-frame-parameter nil 'ns-appearance ',appearance)
     (load-theme ',theme-name t)
     (when (executable-find "claude")
       (start-process "claude-theme" nil "claude" "config" "set" "-g" "theme" ,(symbol-name appearance)))))

(defun my/handle-appearance-change (appearance)
  "Set ns-appearance and theme based on system APPEARANCE with daily rotation."
  (when (eq system-type 'darwin)
    (pcase appearance
      ('light (let ((daily-theme (my/get-daily-theme my/ef-light-themes)))
                (eval `(my/set-appearance-theme light ,daily-theme))))
      ('dark (let ((daily-theme (my/get-daily-theme my/ef-dark-themes)))
               (eval `(my/set-appearance-theme dark ,daily-theme)))))))

(defun my/setup-themes ()
  "Setup theme packages and appearance handling."
  ;; Setup appearance change handling
  (when (and (eq system-type 'darwin)
             (boundp 'ns-system-appearance-change-functions))
    (add-hook 'ns-system-appearance-change-functions #'my/handle-appearance-change))
  
  ;; Set initial theme based on system appearance
  (when (eq system-type 'darwin)
    (my/handle-appearance-change 
     (if (string-match-p "Dark" (or (getenv "APPEARANCE") "")) 'dark 'light))))

(provide 'init-themes)
;;; init-themes.el ends here