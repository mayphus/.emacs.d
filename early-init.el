;;; early-init.el --- Elite startup sequence -*- lexical-binding: t; -*-

;;; Commentary:
;; Hyper-optimized pre-initialization for the discerning hacker.
;; Executes before package loading and frame creation to maximize performance.
;; Every microsecond counts in the relentless pursuit of startup efficiency.

;;; Code:

;; GC threshold manipulation: delay garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.8)

;; Restore sanity after initialization
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 16777216  ; 16MB
                  gc-cons-percentage 0.1)))

;; Native compilation: embrace the machine
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (setq native-comp-async-report-warnings-errors nil  ; silence the noise
        native-comp-deferred-compilation t            ; lazy compilation
        native-comp-speed 3                           ; maximum optimization
        native-comp-async-jobs (/ (num-processors) 2) ; half cores for async
        native-comp-jit-compilation t))               ; JIT when possible

;; UI purification: remove the training wheels
(setq-default inhibit-startup-screen t
              inhibit-startup-echo-area-message t
              inhibit-default-init t
              initial-major-mode 'fundamental-mode
              initial-scratch-message nil)

;; Frame aesthetics: minimal and purposeful
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)

;; Darwin-specific optimizations for the enlightened macOS user
(when (eq system-type 'darwin)
  (push '(ns-transparent-titlebar . t) default-frame-alist)
  (push '(ns-appearance . dark) default-frame-alist)
  (setq ns-use-native-fullscreen t
        ns-use-thin-smoothing t
        ns-pop-up-frames nil
        ns-use-proxy-icon nil))

;; File I/O optimizations: reduce system calls
(setq read-process-output-max (* 1024 1024)  ; 1MB
      process-adaptive-read-buffering nil)

;; Load path enhancement: prepare for custom elisp greatness
(let ((lisp-dir (expand-file-name "lisp" user-emacs-directory)))
  (when (file-directory-p lisp-dir)
    (add-to-list 'load-path lisp-dir t)))

;; Package system: the gateway to infinite possibilities
(setq package-enable-at-startup nil
      package-quickstart t
      package-native-compile t)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu-elpa" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
(package-initialize)

;; Suppression of frivolous warnings for the focused mind
(setq warning-suppress-types '((comp)))

;;; early-init.el ends here
