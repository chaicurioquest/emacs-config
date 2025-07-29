;;; init.el --- Bootstrap Emacs config.org
;; Minimal bootstrapper for Emacs 30 configuration.
;; Loads config.el (tangled from config.org) or tangles config.org if needed.
;; Sets early optimizations (GC, package.el) for faster startup.
;; Synced via https://github.com/chaicurioquest/emacs-config.

;; Minimal bootstrapper for Emacs 30 configuration.

;; Speed up startup by increasing garbage collection threshold.
(setq gc-cons-threshold (* 50 1000 1000))
(setq package-enable-at-startup nil)

;; Configure package.el as a fallback for straight.el.
;;(require 'package)
;;(setq package-archives '(("melpa" . "https://melpa.org/packages/")
;;                         ("gnu" . "https://elpa.gnu.org/packages/")))
;;(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))

;; Bootstrap straight.el for reproducible package management.
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package via straight.el
(straight-use-package 'use-package)

;; Straight to treat Org as a built-in package, preventing cloning and building
(straight-override-recipe '(org :type built-in))


;; Ensure org is loaded for tangling
(require 'org)

;; Define paths
(defvar my-config-org (expand-file-name "config.org" user-emacs-directory))
(defvar my-config-el (expand-file-name "config.el" user-emacs-directory))


;; Variable to track configuration status
(defvar config-load-status "not started")

;; Tangle config.org if needed
(condition-case err
    (when (or (not (file-exists-p my-config-el))
              (file-newer-than-file-p my-config-org my-config-el))
      (message "Tangling %s to %s..." my-config-org my-config-el)
      (org-babel-tangle-file my-config-org my-config-el)
      (message "Tangled successfully."))
  (error (message "Error tangling config.org: %s" err)
         (setq config-load-status "failed to tangle")))

;; Load config.el
(condition-case err
    (when (file-exists-p my-config-el)
      (load my-config-el)
      (setq config-load-status "success"))
  (error (message "Error loading config.el: %s" err)
         (setq config-load-status "failed to load")))


;; Start the Emacs with Full Screen and Font Size for readablity
;; Maximize the initial frame on startup
 (add-to-list 'initial-frame-alist '(fullscreen . maximized))


;; Set default font size (adjust the :height value as needed; 100 is default, 120-140 is larger/readable)
(set-face-attribute 'default nil :height 160)
