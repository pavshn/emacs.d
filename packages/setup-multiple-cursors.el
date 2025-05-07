(use-package multiple-cursors
  :defer t
  :bind (("C-c a" . mc/mark-all-dwim)
         ("C-c n" . mc/mark-next-like-this-symbol)
         ("C-c p" . mc/mark-previous-like-this-symbol)
         ("C-c r" . set-rectangular-region-anchor)))

(provide 'setup-multiple-cursors)
