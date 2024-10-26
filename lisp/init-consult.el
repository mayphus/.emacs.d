;;; init-consult.el --- Consult configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package consult
  :bind (("C-s" . consult-line)
         ("C-x /" . consult-ripgrep))
  :config
  ;; Uncomment and adjust the following line if needed:
  ;; (setq consult-notes-dir denote-directory)
  )

(provide 'init-consult)

;;; init-consult.el ends here
