;;; english-words.el --- English frequency word list viewer -*- lexical-binding: t; -*-

;;; Commentary:
;; A compact viewer for the English word frequency list.

;;; Code:

(defvar english-words--data nil
  "Cached list of English words from frequency list.")

(defvar english-words--buffer-name "*English Words*"
  "Name of the English words buffer.")

(defvar english-words--min-column-width 26
  "Minimum width of each column to fit longest words.")

(defvar english-words--columns nil
  "Number of columns to display words in (calculated dynamically).")

(defvar english-words--column-width nil
  "Width of each column (calculated dynamically).")

(defun english-words--parse-csv ()
  "Parse the English frequency CSV file and return list of words."
  (let ((csv-file (expand-file-name "english_frequency_list.csv" 
                                    (file-name-directory (or load-file-name buffer-file-name default-directory))))
        words)
    (when (file-exists-p csv-file)
      (with-temp-buffer
        (insert-file-contents csv-file)
        (goto-char (point-min))
        (forward-line 1) ; Skip header
        (while (not (eobp))
          (when (looking-at "^[0-9]+,\\(.+\\)$")
            (push (match-string 1) words))
          (forward-line 1))))
    (reverse words)))

(defun english-words--calculate-layout ()
  "Calculate optimal columns and width based on window size."
  (let* ((window-width (max 80 (window-body-width)))
         (cols (max 1 (/ window-width english-words--min-column-width)))
         (col-width (/ window-width cols)))
    (setq english-words--columns cols
          english-words--column-width col-width)))

(defun english-words--load-data ()
  "Load English words data if not already cached."
  (unless english-words--data
    (setq english-words--data (english-words--parse-csv))))

(defun english-words--format-words (words)
  "Format WORDS into multi-column layout."
  (let ((result "")
        (col 0)
        (format-str (format "%%-%ds" english-words--column-width)))
    (dolist (word words)
      (setq result (concat result (format format-str word)))
      (setq col (1+ col))
      (when (>= col english-words--columns)
        (setq result (concat result "\n"))
        (setq col 0)))
    (when (> col 0)
      (setq result (concat result "\n")))
    result))

(defun english-words--display ()
  "Display the English words in a buffer."
  (english-words--load-data)
  (let ((buffer (get-buffer-create english-words--buffer-name)))
    (with-current-buffer buffer
      (english-words-mode)
      (english-words--calculate-layout)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (english-words--format-words english-words--data))
        (goto-char (point-min))))
    (pop-to-buffer buffer)))

(defun english-words-search (query)
  "Search for QUERY in the word list."
  (interactive "sSearch word: ")
  (english-words--load-data)
  (let ((case-fold-search t)
        (found nil))
    (with-current-buffer (get-buffer english-words--buffer-name)
      (goto-char (point-min))
      (when (search-forward query nil t)
        (setq found t)
        (beginning-of-line)
        (recenter)))
    (unless found
      (message "Word '%s' not found" query))))

(defun english-words-goto-position (pos)
  "Jump to word at position POS."
  (interactive "nGoto position: ")
  (english-words--load-data)
  (when (and (> pos 0) (<= pos (length english-words--data)))
    (let ((row (/ (1- pos) english-words--columns))
          (col (mod (1- pos) english-words--columns)))
      (with-current-buffer (get-buffer english-words--buffer-name)
        (goto-char (point-min))
        (forward-line row)
        (move-to-column (* col english-words--column-width))))))

(defun english-words-refresh ()
  "Refresh the word list display."
  (interactive)
  (with-current-buffer (get-buffer english-words--buffer-name)
    (english-words--calculate-layout)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (english-words--format-words english-words--data))
      (goto-char (point-min)))))

(defun english-words-quit ()
  "Quit the English words buffer."
  (interactive)
  (quit-window t))

(defvar english-words-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'english-words-quit)
    (define-key map (kbd "s") #'english-words-search)
    (define-key map (kbd "g") #'english-words-goto-position)
    (define-key map (kbd "r") #'english-words-refresh)
    (define-key map (kbd "n") #'next-line)
    (define-key map (kbd "p") #'previous-line)
    (define-key map (kbd "f") #'forward-char)
    (define-key map (kbd "b") #'backward-char)
    map)
  "Keymap for English words mode.")

(define-derived-mode english-words-mode special-mode "English-Words"
  "Major mode for viewing English word frequency list.

\\{english-words-mode-map}"
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (add-hook 'window-size-change-functions 
            (lambda (_frame) 
              (when (eq major-mode 'english-words-mode)
                (english-words-refresh))) 
            nil t))

;;;###autoload
(defun english-words ()
  "Display English word frequency list in compact view."
  (interactive)
  (english-words--display))

(provide 'english-words)
;;; english-words.el ends here