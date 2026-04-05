;; C-z toggles evil/emacs modes
(use-package evil
  :ensure t
  :init
  ;; Enable Evil Mode globally
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-C-i-jump t)
  :config
  (evil-mode 1)

  ;; Basic Evil mode configurations
  (setq evil-normal-state-cursor 'box)  ; Normal state cursor
  (setq evil-insert-state-cursor 'bar)  ; Insert state cursor
  (setq evil-visual-state-cursor 'hollow)  ; Visual state cursor
  
  ;; Remap basic keybindings
  (define-key evil-normal-state-map (kbd "C-k") 'kill-line)  ; C-k to kill line in normal mode
  (define-key evil-normal-state-map (kbd "C-w") 'evil-window-map)  ; C-w for window commands
  (define-key evil-insert-state-map (kbd "C-g") #'evil-normal-state) ; C-g to exit the insert mode
  (define-key evil-visual-state-map (kbd "p") 'evil-paste-after)  ; Paste in visual mode
  
  ;; Enable line numbers in normal state
  (evil-set-initial-state 'text-mode 'emacs)  ; Keep text-mode in Emacs state (non-Vim)
)

;; Keybinding for Evil mode (for convenience)
(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))  ; Initializes all Evil bindings for common modes
