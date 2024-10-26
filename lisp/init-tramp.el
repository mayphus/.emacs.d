;;; init-tramp.el --- Tramp integration for Init -*- lexical-binding: t; -*-

;;; Code:

(use-package tramp
  :ensure t
  :custom
  (tramp-remote-path (append tramp-remote-path
                             '(tramp-own-remote-path))))

(provide 'init-tramp)
;;; init-tramp.el ends here
