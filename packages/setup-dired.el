(with-eval-after-load 'dired
  (require 'dired-x)

  ;; Make dired less verbose, toggle with (
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)

  ;; Move files between split panes
  (setq dired-dwim-target t)

  ;; C-a is nicer in dired if it moves back to start of files
  (define-key dired-mode-map (kbd "C-a") 'dired-back-to-start-of-files)

  ;; Mirror OSX folder navigation
  (define-key dired-mode-map (kbd "M-<up>") 'dired-jump)
  (define-key dired-mode-map (kbd "M-<down>") 'dired-find-file)

  ;; Delete files with k
  (define-key dired-mode-map (kbd "k") 'dired-do-delete))

;; Nicer navigation also in writeable dired
(with-eval-after-load 'wdired
  (define-key wdired-mode-map (kbd "C-a") 'dired-back-to-start-of-files))

(defun dired-back-to-start-of-files ()
  (interactive)
  (backward-char (- (current-column) 2)))

;; Simlpe yet performant sidebar leveraging Dired
(use-package dired-sidebar
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands))

(provide 'setup-dired)
