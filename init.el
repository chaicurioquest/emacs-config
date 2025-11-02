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
      (message "[%s] Tangling %s to %s..." (format-time-string "%T") my-config-org my-config-el)
      (org-babel-tangle-file my-config-org my-config-el)
      (message "[%s] Tangled successfully." (format-time-string "%T")))
  (error (message "[%s] Error tangling config.org: %s" (format-time-string "%T") err)
         (setq config-load-status "failed to tangle")))

;; Load config.el
(condition-case err
    (when (file-exists-p my-config-el)
      (load my-config-el)
      (message "[%s] Loaded config.el successfully." (format-time-string "%T"))
      (setq config-load-status "success"))
  (error (message "[%s] Error loading config.el: %s" (format-time-string "%T") err)
         (setq config-load-status "failed to load")))

;; Load private tweaks if present (won't error if missing)
;; (let ((private-file (expand-file-name "private.el" user-emacs-directory)))
;;  (when (file-exists-p private-file)
;;    (load-file private-file)))

;; Start the Emacs with Full Screen and Font Size for readablity
;; Maximize the initial frame on startup
 (add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Set default font size (adjust the :height value as needed; 100 is default, 120-140 is larger/readable)
(set-face-attribute 'default nil :height 160)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(whiteboard))
 '(my-org-refile-to-ids
   '(("+fleeting" . "df82ceed-3a1c-470c-8703-6fa24823e363")
     ("+meeting" . "d39cd7c9-99bf-483e-b338-4bd08ae6e413")
     ("+task" . "07bb75d6-077e-4ea8-a8a7-f7dc86cb5737")))
 '(warning-suppress-log-types '((ox-latex) (emacs))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
