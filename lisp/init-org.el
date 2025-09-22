;;; init-org.el --- Org-mode configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Comprehensive org-mode configuration including note-taking, capture templates,
;; and integration with consult-notes and deft.

;;; Code:

;; Create a proper keymap for notes
(defvar my/notes-map (make-sparse-keymap)
  "Keymap for note-taking commands.")

(define-key my/notes-map (kbd "n") 'org-capture)
(define-key my/notes-map (kbd "c") 'consult-notes)
(define-key my/notes-map (kbd "s") 'consult-notes-search-in-all-notes)
(define-key my/notes-map (kbd "f") (lambda () (interactive) (consult-find "~/workspace/notes/")))
(define-key my/notes-map (kbd "r") (lambda () (interactive) (consult-ripgrep "~/workspace/notes/")))
(define-key my/notes-map (kbd "d") 'deft)

(global-set-key (kbd "C-c n") my/notes-map)

(use-package org
  :defer t
  :mode ("\\.org\\'" . org-mode)
  :custom
  (org-startup-indented t)
  (org-pretty-entities t)
  (org-hide-emphasis-markers t)
  (org-startup-with-inline-images t)
  (org-agenda-files '("~/workspace/notes/"))
  (org-capture-templates
   '(("n" "Note" plain
      (file (lambda ()
              (let* ((title (read-string "Title: "))
                     (slug (downcase
                            (replace-regexp-in-string
                             "[^a-z0-9]+" "-"
                             (replace-regexp-in-string
                              "^-\\|-$" ""
                              (replace-regexp-in-string
                               "[^[:alnum:][:space:]]" ""
                               title))))))
                (setq org-capture-current-title title)
                (expand-file-name (concat slug ".org") "~/workspace/notes/"))))
      "#+title:      %(or org-capture-current-title \"\")\n#+filetags:   %(my/org-capture-process-tags)\n\n%?")))
  :config
  ;; Configure org-element cache for better performance
  (setq org-element-use-cache t)
  (setq org-element-cache-persistent t)

  (defun my/org-capture-process-tags ()
    "Process tags input for org capture."
    (let* ((tags-input (read-string "Tags: "))
           (tags (when (not (string-empty-p tags-input))
                   (mapcar (lambda (tag)
                             (downcase (string-trim tag)))
                           (split-string tags-input "[, ]+" t)))))
      (if tags
          (concat ":" (mapconcat 'identity tags ":") ":")
        ""))))

;; Deft for alternative note browsing
(use-package deft
  :ensure t
  :defer t
  :commands deft
  :custom
  (deft-directory "~/workspace/notes/")
  (deft-recursive t)
  (deft-extensions '("org" "txt" "md"))
  (deft-default-extension "org")
  (deft-text-mode 'org-mode)
  (deft-use-filename-as-title t)
  (deft-use-filter-string-for-filename t)
  (deft-auto-save-interval 0)
  ;; Optimize for performance
  (deft-strip-summary-regexp "\\(\\*+\\|#+\\w+:.*\\)")
  ;; Ignore dot files, hidden files, CLAUDE.md, and README.org files
  (deft-ignore-file-regexp "\\(?:\\.\\|#\\|~\\|CLAUDE\\.md\\|README\\.org\\)"))

(provide 'init-org)
;;; init-org.el ends here
