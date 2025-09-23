;;; init-org.el --- Minimal Org-mode configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Lean Org configuration focused on quick capture and agenda access.

;;; Code:

(defconst my/org-notes-directory (expand-file-name "~/workspace/notes/")
  "Primary directory for Org notes.")

(use-package org
  :ensure nil
  :mode ("\\.org\\'" . org-mode)
  :init
  (unless (file-directory-p my/org-notes-directory)
    (make-directory my/org-notes-directory t))
  :bind (("C-c n" . org-capture)
         ("C-c a" . org-agenda))
  :custom
  (org-directory my/org-notes-directory)
  (org-default-notes-file (expand-file-name "inbox.org" my/org-notes-directory))
  (org-agenda-files (list my/org-notes-directory))
  (org-startup-with-inline-images t)
  :config
  (setq org-capture-templates
        '(("n" "Quick note" entry
           (file org-default-notes-file)
           "* %?\n%U\n"))))

(provide 'init-org)
;;; init-org.el ends here
