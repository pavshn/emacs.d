;;; setup-languages.el --- Programming language modes -*- lexical-binding: t; -*-

(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
(use-package neocaml
  :ensure t)
(use-package dune
  :ensure t)

(use-package clang-format
  :hook (c++-mode . clang-format-on-save-mode)
  :config
  (setq clang-format-fallback-style "LLVM"
        clang-format-on-save-p 'always))

(use-package haskell-mode
  :ensure t
  :init
  (setq haskell-process-type 'cabal-new-repl
        haskell-process-log t)
  :hook ((haskell-mode . haskell-indent-mode)
         (haskell-mode . interactive-haskell-mode)
         (haskell-mode . haskell-doc-mode)))

(use-package racket-mode
  :ensure t)

(use-package rust-mode
  :ensure t)

(use-package scala-ts-mode
  :mode "\\.scala\\'"
  :interpreter ("scala" . scala-ts-mode))

(use-package sbt-mode
  :after scala-ts-mode
  :config
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  (setq sbt:program-options '("-Dsbt.supershell=false"))
  (which-key-add-keymap-based-replacements
    sbt-mode-map
    "C-c s" '("SBT hydra" . sbt-hydra)))

(provide 'setup-languages)
;;; setup-languages.el ends here
