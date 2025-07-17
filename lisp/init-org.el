;;; init-org.el --- Org-mode configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Comprehensive org-mode configuration including note-taking, capture templates,
;; and integration with consult-notes and deft.

;;; Code:

(defvar my/notes-map (make-sparse-keymap)
  "Keymap for note-taking commands.")

(defvar my/notes-directory "~/workspace/notes/"
  "Directory for storing notes.")

(defun my/org-capture-process-tags ()
  "Process tags input for org capture."
  (let* ((tags-input (read-string "Tags: "))
         (tags (when (not (string-empty-p tags-input))
                 (mapcar (lambda (tag)
                           (downcase (string-trim tag)))
                         (split-string tags-input "[, ]+" t)))))
    (if tags
        (concat ":" (mapconcat 'identity tags ":") ":")
      "")))

(defun my/org-capture-generate-filename ()
  "Generate filename for org capture based on title."
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
    (expand-file-name (concat slug ".org") my/notes-directory)))

(defun my/set-org-heading-heights ()
  "Set different heights for org heading levels."
  (custom-set-faces
   '(org-level-1 ((t (:height 1.5))))
   '(org-level-2 ((t (:height 1.4))))
   '(org-level-3 ((t (:height 1.3))))
   '(org-level-4 ((t (:height 1.2))))
   '(org-level-5 ((t (:height 1.1))))
   '(org-level-6 ((t (:height 1.0))))
   '(org-level-7 ((t (:height 1.0))))
   '(org-level-8 ((t (:height 1.0))))))

(define-key my/notes-map (kbd "n") 'org-capture)
(define-key my/notes-map (kbd "c") 'consult-notes)
(define-key my/notes-map (kbd "s") 'consult-notes-search-in-all-notes)
(define-key my/notes-map (kbd "f") (lambda () (interactive) (consult-find my/notes-directory)))
(define-key my/notes-map (kbd "r") (lambda () (interactive) (consult-ripgrep my/notes-directory)))
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
  (org-agenda-files (list my/notes-directory))
  (org-capture-templates
   '(("n" "Note" plain
      (file my/org-capture-generate-filename)
      "#+title:      %(or org-capture-current-title \"\")\\n#+filetags:   %(my/org-capture-process-tags)\\n\\n%?")))
  :config
  ;; Performance optimizations
  (setq org-element-use-cache t)
  (setq org-element-cache-persistent t)
  ;; Appearance configuration
  (add-hook 'after-load-theme-hook 'my/set-org-heading-heights)
  (my/set-org-heading-heights))

;;; Deft Configuration
(use-package deft
  :ensure t
  :defer t
  :commands deft
  :custom
  (deft-directory my/notes-directory)
  (deft-recursive t)
  (deft-extensions '("org" "txt" "md"))
  (deft-default-extension "org")
  (deft-text-mode 'org-mode)
  (deft-use-filename-as-title t)
  (deft-use-filter-string-for-filename t)
  (deft-auto-save-interval 0)
  (deft-strip-summary-regexp "\\(\\*+\\|#+\\w+:.*\\)")
  (deft-ignore-file-regexp "\\(?:\\.\\|#\\|~\\|CLAUDE\\.md\\|README\\.org\\)"))

(provide 'init-org)
;;; init-org.el ends here
