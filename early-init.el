;;; early-init.el --- Emacs Solo (no external packages) Configuration --- Early Init  -*- lexical-binding: t; -*-
;;
;; Author: Rahul Martim Juliato
;; URL: https://github.com/LionyxML/emacs-solo
;; Package-Requires: ((emacs "30.1"))
;; Keywords: config
;; SPDX-License-Identifier: GPL-3.0-or-later
;;

;;; Commentary:
;;  Early init configuration for Emacs Solo
;;

;;; Code:

;;; -------------------- PERFORMANCE & HACKS
;; HACK: inscrease startup speed
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      vc-handled-backends '(Git))

;; HACK: avoid being flashbanged
(defun emacs-solo/avoid-initial-flash-of-light ()
  "Avoid flash of light when starting Emacs."
  (setq mode-line-format nil)
  ;; These colors should match your selected theme for maximum effect
  (set-face-attribute 'default nil :background "#282828" :foreground "#282828"))

(defun emacs-solo/reset-default-foreground ()
  "Reset the foreground color of the default face."
    (set-face-attribute 'default nil :foreground (face-foreground 'default)))

(emacs-solo/avoid-initial-flash-of-light)                           ; HACK start
(add-hook 'after-init-hook #'emacs-solo/reset-default-foreground)   ; HACK undo

(provide 'early-init)
;;; early-init.el ends here
