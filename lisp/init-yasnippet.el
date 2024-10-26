;;; init-yasnippet.el --- Yasnippet configuration -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package yasnippet
  :ensure t
  :init
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

(use-package yatemplate
  :ensure t
  :after yasnippet)

(provide 'init-yasnippet)

;;; init-yasnippet.el ends here
