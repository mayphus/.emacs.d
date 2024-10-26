;;; init-package --- Packages -*- lexical-binding: t; -*-

(require 'package)

(setq package-archives '(("org"   . "https://orgmode.org/elpa/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("gnu"   . "https://elpa.gnu.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(defvar my-packages
  '(
    ;; Editing and Navigation
    company corfu expand-region meow smart-hungry-delete yasnippet yasnippet-snippets

    ;; Completion and Search
    consult consult-notes embark embark-consult marginalia orderless vertico

    ;; Version Control and Project Management
    editorconfig forge gist magit

    ;; Programming Languages and Modes
    auctex elixir-mode erlang ess geiser go-mode haskell-mode rust-mode slime terraform-mode toml-mode yaml-mode

    ;; Development Tools
    eglot flycheck format-all

    ;; Org Mode and Related
    denote
	ob-async ob-browser ob-dart ob-elixir ob-go ob-graphql ob-http ob-ipython ob-mongo ob-restclient ob-rust ob-tmux ob-typescript ob-uart
	org-ai org-bullets org-contrib org-ref org-transclusion org-tree-slide ox-pandoc

    ;; Terminal and System
    exec-path-from-shell pass pcache pinentry ssh-config-mode vterm

    ;; UI and Theming
    auto-dark circadian rainbow-delimiters which-key

    ;; Miscellaneous
    codespaces edbi ein elisp-demos ellama elpl elpy esqlite gnuplot helpful robot-mode tblui tsc undo-tree

    ;; AI and Machine Learning
    company-tabnine

    ;; Custom Templates
    yatemplate
	))

(dolist (package my-packages)
  (unless (package-installed-p package)
    (package-install package)))

(provide 'init-package)

;;; init-package ends here
