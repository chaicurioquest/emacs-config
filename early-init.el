;;; early-init.el --- Early startup configuration for Emacs 30.1.90

;; === SECURITY: CVE-2024-53920 mitigation ===
;; Must be set FIRST, before any .el files are visited or loaded.
;; Tells Emacs which directories contain trusted Lisp code for
;; macro-expansion, Flymake, and code completion.
(setq trusted-content
      (list
       (file-truename (expand-file-name user-emacs-directory))
       ;; Add any other dirs you control and trust:
       ;; (expand-file-name "~/dotfiles/")
       ))

;; NEVER set to :all — that disables all protection.

(when (getenv "MY_DEBUG_DEVICE")
  (message "=== EARLY-INIT START ==="))

;; Increase GC threshold early for faster startup
(setq gc-cons-threshold (* 100 1024 1024))

;; Disable package.el early (use straight.el)
(setq package-enable-at-startup nil)

;; Safer GUI check for early-init context
;;(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
;;(push '(vertical-scroll-bars . nil) default-frame-alist)


(when (getenv "MY_DEBUG_DEVICE")
  (message "=== EARLY-INIT COMPLETE ==="))