;;; init.el --- Bootstrap Emacs config.org
;; Minimal bootstrapper for Emacs 30 configuration.
;; Loads config.el (tangled from config.org) or tangles config.org if needed.
;; Sets early optimizations (GC, package.el) for faster startup.
;; Synced via https://github.com/chaicurioquest/emacs-config.

;; Bootstrap straight.el for reproducible package management.
;; To update: M-x straight-pull-package RET straight RET
;;            then: M-x straight-freeze-versions RET
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
         "https://raw.githubusercontent.com/radian-software/straight.el/main/install.el"
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

;; Auto-tangle, byte-compile, and load config.org (best practice)
(when (getenv "MY_DEBUG_DEVICE")
  (message "=== Tangling and loading config.org ==="))

;; Never let org silently eval untrusted blocks during tangle-load
(setq org-confirm-babel-evaluate t)  ; must be set BEFORE org-babel-load-file

;; org-babel-load-file loads config.el directly without going through my/tangle-if-needed guards
(org-babel-load-file my-config-org)

;; Load private tweaks if present (won't error if missing)
;; user files specific setting goes here 
(let ((private-file (expand-file-name "private.el" user-emacs-directory)))
  (when (file-exists-p private-file)
    (load-file private-file)))

;; Start Emacs with Full Screen and Font Size for readability
;; Maximize the initial frame on startup
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
;; Set default font size (adjust the :height value as needed; 100 is default, 120-140 is larger/readable)
(set-face-attribute 'default nil :height 160)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(my-org-refile-to-ids
   '(("+fleeting" . "df82ceed-3a1c-470c-8703-6fafa24823e363")
     ("+meeting" . "d39cd7c9-99bf-483e-b338-4bd08ae6e413")
     ("+task" . "07bb75d6-077e-4ea8-a8a7-f7dc86cb5737")))
 '(safe-local-variable-values nil)
 ;; '(warning-suppress-log-types '((ox-latex) (emacs)))
 )

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
