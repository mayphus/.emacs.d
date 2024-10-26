;;; init-path.el --- Set up load path -*- lexical-binding: t; -*-

(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns x))
  :custom
  (exec-path-from-shell-warn-duration-millis 12000)
  :config
  (condition-case nil
      (exec-path-from-shell-initialize)
    (error (message "Failed to initialize exec-path-from-shell"))))

(provide 'init-path)

;;; init-path.el ends here
