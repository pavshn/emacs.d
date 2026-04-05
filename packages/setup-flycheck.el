(use-package flycheck
  :ensure t
  :defer t
  :hook (prog-mode . flycheck-mode)
  :bind (:map flycheck-mode-map
              ("M-9" . flycheck-next-error)
              ("M-8" . flycheck-previous-error)
              ("C-c ! n" . flycheck-next-error)
              ("C-c ! p" . flycheck-previous-error)
              ("C-c ! l" . flycheck-list-errors)
              ("C-c ! t" . flycheck-mode)))

(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
