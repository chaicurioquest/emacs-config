(defun my-termux-p ()
  "Return non-nil if running in Termux (Android)."
  (getenv "PREFIX")) ;; reliable in Termux

(defun my-android-prop (prop)
  "Get Android system property PROP using getprop."
  (when (my-termux-p)
    (string-trim (shell-command-to-string (format "getprop %s" prop)))))

(defun my-tablet-p ()
  "Return non-nil if device appears to be an Android tablet."
  (when (my-termux-p)
    (let ((chars (my-android-prop "ro.build.characteristics"))
          (model (my-android-prop "ro.product.model")))
      (or (and chars (string-match "tablet" chars))
          (and model (string-match "\\(Tab\\|Tablet\\)" model))))))

(defun my-phone-p ()
  "Return non-nil if device appears to be an Android phone."
  (when (my-termux-p)
    (not (my-tablet-p))))

(defvar my-device
  (cond
   ;; Termux tablet
   ((my-tablet-p) 'tablet)
   ;; Termux phone
   ((my-phone-p) 'phone)
   ;; Generic laptop/desktop
   ((memq system-type '(gnu/linux darwin windows-nt)) 'laptop)
   ;; Fallback
   (t 'generic))
  "Current device type: 'laptop, 'tablet, 'phone, or 'generic.")

(provide 'device)
