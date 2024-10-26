;;; init-window.el --- Window configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;; Code:

(use-package emacs
  :config
  (defun balance-windows-after-split (&rest _)
    "Balance windows after splitting."
    (balance-windows))

  (advice-add 'split-window-right :after #'balance-windows-after-split)
  (advice-add 'split-window-below :after #'balance-windows-after-split))

(provide 'init-window)
;;; init-window.el ends here
