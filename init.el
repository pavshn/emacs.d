;; This config is heavily inspired by the magnars' emacs-reboot config
;; https://github.com/magnars/emacsd-reboot

;; Add settings to load-path
(add-to-list 'load-path (expand-file-name "settings" user-emacs-directory))

;; Optimize startup of Emacs
(require 'fast-startup)

;; Keep emacs Custom-settings in separate file, not appended to init.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Set up appearance early
(require 'appearance)

;; Configure the package manager
(require 'packages)

;; Add helpers
(require 'utils)

;; Add utilities
(require 'basics)
(require 'navigation)
(require 'editing)

;; Set up Straight (for packages on github)
(require 'setup-straight)

(require 'better-defaults)

;; Load all packages
(dolist (file (directory-files packages-dir t "^[^#].*el$"))
  (when (file-regular-p file)
    (load file)))

(use-package vterm :ensure t)
