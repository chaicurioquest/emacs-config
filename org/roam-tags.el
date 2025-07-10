(defvar my-roam-tags
  '("academics" "research" "personal" "notes" "projects" "students" "presentations"
    "events" "grants" "labwork" "concepts" "ideas" "references" "reviews"
    "draft" "final" "confidential" "internal" "public")
  "List of tags for org-roam Zettelkasten notes, filtered from .filetags.")
  (message "✅ roam-tags.el loaded and provided feature 'roam-tags'")
(provide 'roam-tags) ;; Emacs won’t re-evaluate the file if it’s already provided. (require 'feature-name) will load the file only once and only when needed.

