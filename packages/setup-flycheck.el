;; -*- lexical-binding: t; -*-

(use-package flycheck
  :ensure t
  :defer t
  :hook ((prog-mode . flycheck-mode)
         (haskell-mode . (lambda () (flycheck-mode -1))))
  :bind (:map flycheck-mode-map
              ("M-9" . flycheck-next-error)
              ("M-8" . flycheck-previous-error)
              ("C-c ! n" . flycheck-next-error)
              ("C-c ! p" . flycheck-previous-error)
              ("C-c ! l" . flycheck-list-errors)
              ("C-c ! t" . flycheck-mode))
  :config
  (setq-default flycheck-clang-language-standard "c++26"))

(use-package flycheck-rust
  :ensure t
  :after flycheck
  :hook (rust-mode . flycheck-rust-setup))
