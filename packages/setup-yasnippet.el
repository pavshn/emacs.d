(use-package
 yasnippet
 :ensure
 :config
 (yas-reload-all)
 (add-hook 'prog-mode-hook 'yas-minor-mode)
 (add-hook 'text-mode-hook 'yas-minor-mode))
