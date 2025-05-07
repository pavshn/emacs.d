(global-set-key (kbd "C--") 'undo)

;; Killing words backwards
(global-set-key (kbd "C-w") 'kill-region-or-backward-word)

(defun kill-region-or-backward-word ()
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word 1)))

(provide 'editing)
