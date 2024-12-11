;;; mayphus.el --- Custom features for Emacs by Mayphus

;; Author: Mayphus Tang
;; Version: 1.0
;; Keywords: custom, features, emacs, lisp

(require 's)

(defun title-to-filename (title)
  "Convert TITLE to a suitable filename using s.el."
  (->> title
       (s-downcase)
       (s-replace-regexp "[[:punct:]]" "")
       (s-trim)
       (s-replace " " "-")))

(defun mayphus/goto ()
  "Quickly open folders inside org-directory's articles, notes, journals, life, and kid.
  Defaults to org-directory if no selection is made."
  (interactive)
  (let ((dir (completing-read "Select directory (default: org-directory): "
                               '("articles" "notes" "journals" "life" "kid" "diet")
                               nil nil nil nil "")))
    (dired (expand-file-name
            (if (string= dir "") "." dir)
            org-directory))))

(global-set-key (kbd "C-c t g") 'mayphus/goto)

(provide 'mayphus)

;;; mayphus.el ends here


