;;; init-org.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq org-directory (expand-file-name "org" workspace-directory))
(setq denote-directory (expand-file-name "notes" org-directory))

(setq org-startup-indented t)
(setq org-hide-emphasis-markers t)

(setq org-confirm-babel-evaluate nil)

;; (setq ob-ditaa-jar-path "/usr/local/Cellar/ditaa/0.11.0_1/libexec/ditaa-0.11.0-standalone.jar"
;;       org-ditaa-jar-path "/usr/local/Cellar/ditaa/0.11.0_1/libexec/ditaa-0.11.0-standalone.jar")

(setq org-babel-python-command "python3")

(setq org-babel-clojure-backend 'cider)

(add-hook 'before-save-hook 'org-update-all-dblocks)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (C . t)
   (awk . t)
   (shell . t)
   (go . t)
   (lisp . t)
   (http . t)
   (clojure . t)
   (sql . t)
   (org . t)
   (js . t)
   (gnuplot . t)
   (ditaa . t)
   (dot . t)
   (elixir . t)
   (julia . t)
   (R . t)
   (perl . t)
   (octave . t)
   (groovy . t)
   (java . t)
   (lua . t)
   (haskell . t)
   (fortran . t)
   (ruby . t)
   (rust . t)))

(setq org-todo-keywords
      '((sequence "TODO(t)" "DOING(i)" "WAITING(w@/!)" "|" "DONE(x!)" "CANCELED(c@)")
        (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
        (sequence "BACKLOG(l)" "READY(y)" "ACTIVE(a)" "|" "COMPLETED(d)")))

(setq org-log-done 'time)

(setq org-capture-templates
      '(("g" "Gettting Things Done" entry (file+headline "~/org/daily/gtd.org" "Tasks")
         "* TODO %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t))\n  %i  %a\n")

        ("i" "Inbox" entry (file+olp+datetree "~/org/daily/inbox.org")
         "* %?\n  %i  %a"
         :tree-type week)
	
	("n" "Note" entry (file+olp+datetree "~/org/daily/inbox.org")
         "* %?\n  %i  %a"
         :tree-type week)
	
        ("j" "Journal" entry (file+olp+datetree "~/org/daily/journal.org")
         "* %?\nEntered on %U\n  %i  %a"
         :tree-type week)
	))

(add-to-list 'org-agenda-files (expand-file-name "gtd.org" org-directory))

(setq org-latex-pdf-process '("xelatex -interaction nonstopmode %f" "xelatex -interaction nonstopmode %f"))

(setq org-export-global-macros
      '(("timestamp" . "@@html:<span class=\"timestamp\">[$1]</span>@@")))

(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

(use-package denote
  :ensure t
  :config
  (setq denote-dired-directories-include-subdirectories t)
  (setq denote-dired-directories
      (list denote-directory
            (expand-file-name "articles" org-directory)
	    (expand-file-name "journal" org-directory)))

  :hook
  (dired-mode . denote-dired-mode-in-directories)

  :bind
  (("C-c n n" . denote)
   ("C-c n l" . denote-link)
   ("C-c n r" . denote-rename-file)))

(defun my/goto-denote-directory ()
  "Go to the directory where denote notes are stored."
  (interactive)
  (dired denote-directory))

(global-set-key (kbd "C-c n d") 'my/goto-denote-directory)

(provide 'init-org)

;;; init-org ends here
