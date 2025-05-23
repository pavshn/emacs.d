;; Deadgrep
;;
;; The fast, beautiful text search that your Emacs deserves.

(use-package deadgrep
  :bind (("M-s g" . deadgrep)
         (:map deadgrep-mode-map
               ("q" . deadgrep-quit)))

  :config
  (wrap-fullscreen deadgrep))

;; Consider deadgrep-edit-mode as an alternative to wgrep.

(defun deadgrep-quit ()
  (interactive)
  (let ((prev my/previous-window-configuration))
    (quit-window)
    (when prev (register-val-jump-to prev nil))))

(provide 'setup-deadgrep)
