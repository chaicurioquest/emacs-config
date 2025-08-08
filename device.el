;; ~/.emacs.d/device.el
;; Device detection logic

(defun my-termux-p ()
  "Return non-nil if running in Termux (Android)."
  (or (getenv "PREFIX")
      (string-match "linux-android" system-configuration)))

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
   ;; Laptop / desktop detection
   ((memq system-type '(gnu/linux darwin windows-nt))
    'laptop)
   ;; Tablet detection
   ((my-tablet-p)
    'tablet)
   ;; Phone detection
   ((my-phone-p)
    'phone)
   ;; Fallback
   (t 'generic))
  "Current device type: 'laptop, 'tablet, 'phone, or 'generic.")

(provide 'device)
