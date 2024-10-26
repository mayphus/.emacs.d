;;; init-vertico.el --- Vertico configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(vertico-mode t)
;;(vertico-grid-mode t)

(keymap-set vertico-map "DEL" #'vertico-directory-delete-word)

(provide 'init-vertico)

;;; init-vertico.el ends here
