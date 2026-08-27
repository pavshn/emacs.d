;; -*- lexical-binding: t; -*-
(use-package org
  :ensure nil

  :init
  (setq org-directory "~/org"
        org-default-notes-file
        (expand-file-name "inbox.org" org-directory))

  :custom
  ;; Visual
  (org-startup-indented t)
  (org-hide-emphasis-markers t)
  (org-startup-folded 'content)

  ;; UX
  (org-return-follows-link t)

  ;; TODO logging
  (org-log-done 'time)

  ;; Quick capture
  (org-capture-templates
   '(("i" "Inbox"
      entry
      (file "~/org/inbox.org")
      "* %?\n%U\n")))

  :bind
  ("C-c c" . org-capture))

(setq org-startup-with-inline-images t)
