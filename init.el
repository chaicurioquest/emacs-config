;;; init.el --- Bootstrap Emacs config.org
;; Minimal bootstrapper for Emacs 28.1 configuration.
;; Loads config.el (tangled from config.org) or tangles config.org if needed.
;; Sets early optimizations (GC, package.el) for faster startup.
;; Synced via https://github.com/chaicurioquest/emacs-config.

;; Minimal bootstrapper for Emacs 28.1 configuration.

;; Speed up startup by increasing garbage collection threshold.
(setq gc-cons-threshold (* 50 1000 1000))
(setq package-enable-at-startup nil)

;; Configure package.el as a fallback for straight.el.
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))

;; Bootstrap straight.el for reproducible package management.
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; 👉 Now use straight to install org before requiring it
(straight-use-package 'use-package)
(straight-use-package 'org)

;; Ensure org is loaded for tangling
(require 'org)

;; Define paths
(defvar my-config-org (expand-file-name "config.org" user-emacs-directory))
(defvar my-config-el (expand-file-name "config.el" user-emacs-directory))

;; Tangle config.org if needed
(condition-case err
    (when (or (not (file-exists-p my-config-el))
              (file-newer-than-file-p my-config-org my-config-el))
      (message "Tangling %s to %s..." my-config-org my-config-el)
      (org-babel-tangle-file my-config-org my-config-el)
      (message "Tangled successfully."))
  (error (message "Error tangling config.org: %s" err)))

;; Load config.el
(condition-case err
    (when (file-exists-p my-config-el)
      (load my-config-el))
  (error (message "Error loading config.el: %s" err)))

(message "✅ init.el completed")
