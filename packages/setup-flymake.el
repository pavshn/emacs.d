(use-package flymake
  :ensure nil
  :defer t
  :hook ((prog-mode . my/maybe-enable-flymake))
  :bind (:map flymake-mode-map
              ("M-9" . flymake-goto-next-error)
              ("M-8" . flymake-goto-prev-error)
              ("C-c ! n" . flymake-goto-next-error)
              ("C-c ! p" . flymake-goto-prev-error)
              ("C-c ! l" . flymake-show-buffer-diagnostics)
              ("C-c ! t" . toggle-flymake-diagnostics-at-eol))
  :custom
  (flymake-show-diagnostics-at-end-of-line 'short)
  (flymake-indicator-type 'margins)
  (flymake-margin-indicators-string
   `((error "!" compilation-error)
     (warning "?" compilation-warning)
     (note "i" compilation-info)))
  :config
  (defun my/maybe-enable-flymake ()
    (unless (my/elisp-config-file-p)
      (flymake-mode 1)))

  (defun my/elisp-config-file-p ()
    (and buffer-file-name
         (derived-mode-p 'emacs-lisp-mode)
         (file-in-directory-p
          (file-truename buffer-file-name)
          (file-truename user-emacs-directory))))

  (defun toggle-flymake-diagnostics-at-eol ()
    "Toggle the display of Flymake diagnostics at the end of the line
and restart Flymake to apply the changes."
    (interactive)
    (setq flymake-show-diagnostics-at-end-of-line
          (not flymake-show-diagnostics-at-end-of-line))
    (flymake-mode -1)
    (flymake-mode 1)
    (message "Flymake diagnostics at end of line: %s"
             (if flymake-show-diagnostics-at-end-of-line
                 "Enabled" "Disabled"))))

(provide 'setup-flymake)
