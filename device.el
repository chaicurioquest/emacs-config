;; ~/.emacs.d/device.el
;; Shared device detection logic used by early-init.el and config.org.

(defvar my-device-configs
  (let ((table (make-hash-table :test 'equal)))
    (puthash "ram" 'laptop table)  ;; Replace with actual hostname
    (puthash "termux" 'termux table)
    table)
  "Map system names to symbolic device types.")

(defvar my-device
  (or (gethash system-name my-device-configs)
      (if (string-match "termux" system-configuration) 'termux 'generic))
  "Current device type.")
