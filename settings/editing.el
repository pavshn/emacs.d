;; -*- lexical-binding: t; -*-
(keymap-global-set "C-c \\" 'fill-region)

(keymap-global-set "C--" 'undo)

;; Killing words backwards
(keymap-global-set "C-w" 'kill-region-or-backward-word)

(defun kill-region-or-backward-word ()
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word 1)))

(provide 'editing)
