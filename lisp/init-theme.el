;;; init-theme.el --- Theme configuration -*- lexical-binding: t; -*-

;;; Code

(use-package auto-dark
  :ensure t
  :custom
  (auto-dark-themes '((modus-vivendi) (modus-operandi)))
  (aut-dark-allow-osascript t)
  :init (auto-dark-mode))

(provide 'init-theme)
;;; init-theme.el ends here
