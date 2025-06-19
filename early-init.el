
;; ~/.emacs.d/early-init.el
;; Load before GUI and init.el. Optimizes startup and UI appearance.

;; Enable debug messages if environment variable is set
(when (getenv "MY_DEBUG_DEVICE")
  (message "=== STARTING EARLY-INIT ==="))

;; Prevent package.el from loading packages (use straight.el instead)
(setq package-enable-at-startup nil)

;; Speed up startup (reset later in config.org)
(setq gc-cons-threshold (* 50 1000 1000))

;; Load device-specific logic early
(load (expand-file-name "device.el" user-emacs-directory))

;; Set default directory based on device
(setq default-directory
      (cond ((eq my-device 'laptop) "~/wspace/org/notes/")
            ((eq my-device 'termux) "~/storage/shared/notes/")
            (t "~")))

;; Disable GUI elements early (avoid flicker)
(when (eq my-device 'laptop)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))
(when (eq my-device 'termux)
  (set-fringe-mode 0)) ;; Termux: minimal display

(when (getenv "MY_DEBUG_DEVICE")
  (message "=== EARLY-INIT COMPLETE ==="))
