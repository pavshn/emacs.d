;; Auto refresh buffers
(use-package autorevert
  :defer 2
  :config (global-auto-revert-mode 1))

;; Remember and restore the last cursor location of opened files
(save-place-mode 1)

;; Save what you enter into minibuffer prompts
(setq history-length 25)
(savehist-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Show keybinding prefixes faster
(setq echo-keystrokes 0.1)

;; Move files to trash when deleting
(setq delete-by-moving-to-trash t)

;; Shift is more useful as a modifier
(setq shift-select-mode nil)

;; Transparently open compressed files
(auto-compression-mode t)

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; UTF-8 please
(setq locale-coding-system 'utf-8) ; pretty
(set-terminal-coding-system 'utf-8) ; pretty
(set-keyboard-coding-system 'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8) ; with sugar on top

;; Remove text in active region if inserting text
(use-package delsel
  :defer 1
  :config (delete-selection-mode 1))

;; Always display column numbers
(setq column-number-mode t)

;; Save a list of recent files visited. (open recent file with C-x f)
(use-package recentf
  :defer 1 ;; Loads after 1 second of idle time.
  :config (recentf-mode 1)
  :custom (recentf-max-saved-items 100))  ;; just 20 is too recent

;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)

;; Easily navigate sillycased words
(use-package subword
  :defer 1
  :config (global-subword-mode 1)
  :diminish subword-mode)

;; Don't visually break lines for me, please
(setq-default truncate-lines t)

;; 80 chars is a good width.
(setq-default fill-column 79)
(global-display-fill-column-indicator-mode t)

(provide 'basics)
