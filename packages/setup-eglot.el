(use-package rust-mode
  :init)

(defun eglot-go-config ()
  "Setup Eglot Go config."
  (eglot-ensure)
  (add-hook 'go-mode-hook 'eglot-ensure)
  (setq-local eglot-ignored-server-capabilities '(:documentHighlightProvider))
  (add-hook 'before-save-hook #'eglot-format-buffer nil t))

(defun eglot-rust-config ()
  "Setup Eglot Rust config."
  (eglot-ensure)
  (add-hook 'before-save-hook #'eglot-format-buffer nil t))

;; (defun eglot-python-config ()
;;   "Setup Eglot for Python with Ruff."
;;   (eglot-ensure)
;;   (add-hook 'before-save-hook #'eglot-format nil t))

(defun eglot-python-config ()
  "Setup Eglot for Python with BasedPyright and Ruff."
  (eglot-ensure)
  ;; Configure Ruff formatting on save
  (add-hook 'before-save-hook 'ruff-format-buffer)
  ())

(use-package eglot
  :ensure t
  :hook ((python-mode . eglot-python-config)
         (rust-mode . eglot-rust-config)
         (go-mode . eglot-go-config))
  :config
  (setq eglot-send-changes-idle-time 0.5)
  ;; Server configurations
  (add-to-list 'eglot-server-programs
               '((rust-ts-mode rust-mode) .
                 ("rust-analyzer" :initializationOptions (:check (:command "clippy")))))
  (add-to-list 'eglot-server-programs
               '(go-mode "gopls"))
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) "basedpyright-langserver" "--stdio")))

;; (straight-use-package
;;   '(el-patch :type git :host github :repo "jdtsmith/eglot-booster"))

(use-package eglot-booster
  :vc (:url "https://github.com/jdtsmith/eglot-booster")
  ;; :ensure nil
  :after eglot
  :config (eglot-booster-mode))

;; Configure Ruff via Flymake (replaces flymake-ruff)
;; (with-eval-after-load 'flymake
;;   (defun python-flymake-ruff-config ()
;;     "Configure Flymake to use Ruff for Python diagnostics."
;;     (setq-local python-flymake-command
;;                 `("ruff" "--quiet" "check" "--preview"
;;                   "--output-format=pylint"
;;                   "--stdin-filename=stdin" "-")
;;                 flymake-mode-line-lighter " 🐍 "
;;                 flymake-no-changes-timeout 1.0
;;                 flymake-mode-line-counter-format
;;                 '("" flymake-mode-line-error-counter
;;                   flymake-mode-line-warning-counter
;;                   flymake-mode-line-note-counter))
;;     (add-hook 'flymake-diagnostic-functions #'python-flymake -90 t))

;;   (add-hook 'eglot-managed-mode-hook #'python-flymake-ruff-config))

;; (use-package flymake-ruff
;;   :ensure t
;;   :hook (eglot-managed-mode . flymake-ruff-load))

;; Update faces for Eglot
(custom-set-faces
 '(highlight ((t (:weight bold :background unspecified :underline nil))))
 '(eglot-highlight-symbol-face ((t (:weight bold :background unspecified :underline nil)))))

(defun eglot-is-highlighted (pos)
  "Check if position POS is inside an Eglot highlight."
  (seq-find (lambda (ov)
              (eq (overlay-get ov 'face) 'eglot-highlight-symbol-face))
            (overlays-at pos)))

(defun eglot-highlight-past-start (pos)
  "Return the start position of the Eglot highlight at POS, or POS if none."
  (let ((ov (eglot-is-highlighted pos)))
    (if ov
        (overlay-start ov)
      pos)))

(defun eglot-highlight-past-end (pos)
  "Return the end position of the Eglot highlight at POS, or POS if none."
  (let ((ov (eglot-is-highlighted pos)))
    (if ov
        (overlay-end ov)
      pos)))

(defun eglot-next-highlight (&optional recurse)
"Jump to the next Eglot highlight in the buffer.
Wraps around to the beginning if no highlight is found until the end."
  (interactive)
  (let* ((orig (point))
         (prev-pos -1)
         (pos (if (eglot-is-highlighted orig)
                  (eglot-highlight-past-end orig)
                orig))
         (found nil))
    (while (and (/= prev-pos pos)
                (not (setq found (eglot-is-highlighted pos))))
      (setq prev-pos pos
            pos (next-overlay-change pos)))
    (if (and (not recurse)
             (not found)
             (goto-char (point-min))
             (not (eglot-next-highlight t)))
        (unless recurse
          (message "No next highlight found")
          (goto-char orig))
      (when found
        (goto-char pos)))))

(defun eglot-previous-highlight (&optional recurse)
"Jump to the previous Eglot highlight in the buffer.
Wraps around to the end if no highlight is found until the beginning."
  (interactive)
  (let* ((orig (point))
         (prev-pos -1)
         (pos (if (eglot-is-highlighted orig)
                  (eglot-highlight-past-start orig)
                orig))
         (found nil))
    (while (and (/= prev-pos pos)
                (not (setq found (eglot-is-highlighted (1- pos)))))
      (setq prev-pos pos
            pos (previous-overlay-change pos)))
    (if (and (not recurse)
             (not found)
             (goto-char (point-max))
             (not (eglot-previous-highlight t)))
        (unless recurse
          (message "No previous highlight found")
          (goto-char orig))
      (when found
        (goto-char (1- pos))))))

;; Optional keybindings
(keymap-global-set "M-]" 'eglot-next-highlight)
(keymap-global-set "M-[" 'eglot-previous-highlight)

(use-package eldoc-box
  :ensure t
  :config
  (global-set-key (kbd "C-c h") #'eldoc-box-help-at-point)
  ;; <up> to scroll down, <down> to scroll up. why the fuck not, right?
  (global-set-key (kbd "M-<down>") #'eldoc-box-scroll-up)
  (global-set-key (kbd "M-<up>") #'eldoc-box-scroll-down))

(provide 'setup-eglot)
;;; setup-eglot.el ends here
