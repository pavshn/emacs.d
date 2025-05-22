;; Consult
;;
;; Provides search and navigation commands based on the Emacs completion
;; function completing-read. Completion allows you to quickly select an item
;; from a list of candidates.

(use-package consult
  :bind (("C-x f" . consult-recent-file)
         ("C-x C-i" . consult-imenu)
         ("C-x M-i" . consult-imenu-multi)
         ("C-x i" . consult-outline)
         ("C-x C-y" . consult-yank-from-kill-ring)
         ("C-v" . consult-line)
         ("M-v" . consult-line-multi)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-y" . consult-yank-pop)
         ;; Modifies buffer
         ("M-s k" . consult-keep-lines)
         ;; Temporarily hide lines
         ;; Use C-u prefix to show the hidden lines again
         ("M-s f" . consult-focus-lines)
         ("M-s s" . consult-ripgrep)
         ("C-c d" . consult-flymake)
         ("C-x b" . consult-buffer))

  :after (perspective)

  :config
  ;; Show only perspective-buffers with consult-buffer
  (consult-customize consult--source-buffer :hidden t :default nil)
  (add-to-list 'consult-buffer-sources persp-consult-source))

;; (use-package consult-flycheck
;;   :bind (("M-g f" . consult-flycheck)))

(setq completion-in-region-function
      (lambda (&rest args)
        (apply (if vertico-mode
                   #'consult-completion-in-region
                 #'completion--in-region)
               args)))

(provide 'setup-consult)
