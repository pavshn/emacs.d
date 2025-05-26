(keymap-global-set "C-," (lambda () (interactive) (other-window -1)))
(keymap-global-set "C-." (lambda () (interactive) (other-window 1)))

(keymap-global-set "<next>" 'scroll-half-page-down)
(keymap-global-set "<prior>" 'scroll-half-page-up)
(keymap-global-set "C-v" 'scroll-half-page-down)
(keymap-global-set "M-v" 'scroll-half-page-up)

(keymap-global-set "C-a" 'smart-beginning-of-line)

(keymap-global-set "C-g" #'keyboard-quit-dwim)

(keymap-global-set "M-0" 'delete-other-windows)
(keymap-global-set "M-o" 'other-window)

;; Navigate paragraphs
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)

;; Faster vertical navigation
(keymap-global-set "C-S-n" (lambda () (interactive) (ignore-errors (forward-line 5) (recenter))))
(keymap-global-set "C-S-p" (lambda () (interactive) (ignore-errors (forward-line -5) (recenter))))

;; Where was I again?
(global-set-key (kbd "M-B") 'goto-last-modification)

;; Make C-<backspace> work as intended
(keymap-global-set "C-<backspace>" 'kill-to-bol)

(defun kill-to-bol ()
  "Kill from point to beginning of line."
  (interactive)
  (kill-line 0))

(defun smart-beginning-of-line ()
  "Toggle between beginning of a line and first non-whitespace character."
  (interactive)
  (let ((pt (point)))
    (back-to-indentation)
    (when (= pt (point))
      (move-beginning-of-line 1))))

(defun scroll-half-page-down ()
  "Scroll down half a page, emulating vim's C-d."
  (interactive)
  (let ((half-height (/ (window-height) 2)))
  (ignore-errors (forward-line half-height) (recenter))))

(defun scroll-half-page-up ()
  "Scroll up half a page, emulating vim's C-u."
  (interactive)
  (let ((half-height (/ (window-height) 2)))
  (ignore-errors (forward-line (- half-height)) (recenter))))

(defun goto-last-modification ()
  (interactive)
  (undo-fu-only-undo)
  (undo-fu-only-redo))

(defun keyboard-quit-dwim ()
"Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(provide 'navigation)

