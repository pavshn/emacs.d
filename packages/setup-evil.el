;; C-z toggles evil/emacs modes in current buffer
(use-package evil
  :ensure t
  :init
  ;; Enable Evil Mode globally
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-C-i-jump t)
  :config
  (evil-mode 1)

  ;; Display state info in mode line
  (setq evil-normal-state-tag  " N "
        evil-insert-state-tag  " I "
        evil-visual-state-tag  " V "
        evil-motion-state-tag  " M "
        evil-emacs-state-tag   " E "
        evil-operator-state-tag " OP ")
  
  ;; Set cursor style and color based on Evil state
  (setq evil-normal-state-cursor '(box "#fbf1c7") ; Normal
        evil-insert-state-cursor '(bar "#fbf1c7") ; Insert
        evil-visual-state-cursor '(box "#458588") ; Visual
        evil-emacs-state-cursor '(box "#f86155")) ; Emacs
  
  ;; Remap basic keybindings
  (define-key evil-normal-state-map (kbd "H") 'previous-buffer)
  (define-key evil-normal-state-map (kbd "L") 'next-buffer)
  (define-key evil-normal-state-map (kbd "C-k") 'kill-line)  ; C-k to kill line in normal mode
  (define-key evil-normal-state-map (kbd "C-w") 'evil-window-map)  ; C-w for window commands
  (define-key evil-insert-state-map (kbd "C-g") #'evil-normal-state) ; C-g to exit the insert mode
  (define-key evil-insert-state-map (kbd "C-c C-c") #'evil-normal-state) ; C-c C-c - same
  (define-key evil-visual-state-map (kbd "p") 'evil-paste-after)  ; Paste in visual mode
  
  ;; Keep emacs state in the following modes
  (evil-set-initial-state 'vterm-mode 'emacs)
  (evil-set-initial-state 'dired-mode 'emacs)
)

;; Keybinding for Evil mode (for convenience)
;;(use-package evil-collection
;;  :after evil
;;  :ensure t
;;  :config
;;  (evil-collection-init))  ; Initializes all Evil bindings for common modes
