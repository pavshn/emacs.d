;; Based
(use-package gruvbox-theme
  :ensure t
  :config (load-theme 'gruvbox-dark-medium t))

(set-face-font 'default "Iosevka Comfy 11")

;; Disable blinking
(setq visible-bell       nil
      ring-bell-function #'ignore)

(setq-default tab-width 4)

;; Set default window size
(setq initial-frame-alist
      (append initial-frame-alist '((width . 140) (height . 56))))

(provide 'appearance)
