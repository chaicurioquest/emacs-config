;; ~/.emacs.d/init.el
;; Always define the tangler, then generate config.el if missing.

(require 'org)

(defun my-tangle-config-org ()
  "Tangle all Emacs Lisp blocks from config.org and org/*.org into config.el."
  (let* ((body-list '())
         (output-file (expand-file-name "config.el" user-emacs-directory))
         (org-dir (expand-file-name "org" user-emacs-directory))
         (org-files (cons (expand-file-name "config.org" user-emacs-directory)
                          (when (file-directory-p org-dir)
                            (directory-files org-dir t "\\.org$")))))
    (dolist (file org-files)
      (with-current-buffer (find-file-noselect file)
        (org-babel-map-src-blocks nil
          (let* ((info (org-babel-get-src-block-info 'light))
                 (lang (nth 0 info))
                 (params (nth 2 info))
                 (tfile (cdr (assq :tangle params)))
                 (cancelled? (save-excursion
                               (org-back-to-heading t)
                               (re-search-forward org-todo-regexp (line-end-position) t)
                               (string= (match-string 0) "CANCELLED")))
                 (block-name (save-excursion
                               (org-back-to-heading t)
                               (org-get-heading t t t t))))
            (unless (or (string= tfile "no")
                        (and tfile (not (string= tfile "config.el")))
                        cancelled?
                        (not (string= lang "emacs-lisp")))
              (push (list block-name (nth 1 info)) body-list))))))
    (with-temp-file output-file
      (insert ";; Auto-generated from config.org on " (format-time-string "%Y-%m-%d") "\n\n")
      (dolist (block (reverse body-list))
        (insert ";; From: " (car block) "\n" (cadr block) "\n\n"))
      (message "Wrote %s" output-file))))

;; Always define the tangler function for manual use
;; Tangle on startup if config.el doesn't exist
(let ((config-el (expand-file-name "config.el" user-emacs-directory)))
  (unless (file-exists-p config-el)
    (message "config.el missing — tangling config.org now...")
    (my-tangle-config-org))
  (load config-el))
