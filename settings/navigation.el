;;(global-set-key (kbd "C-.") 'scroll-up-line)
;;(global-set-key (kbd "C-,") 'scroll-down-line)

(keymap-global-set "C-," (lambda () (interactive) (other-window -1)))
(keymap-global-set "C-." (lambda () (interactive) (other-window 1)))

(keymap-global-set "C-v" 'scroll-down-half-page)
(keymap-global-set "M-v" 'scroll-up-half-page)

(keymap-global-set "C-a" 'smart-beginning-of-line)

(keymap-global-set "C-g" #'keyboard-quit-dwim)

(keymap-global-set "M-0" 'delete-other-windows)

;; Navigate paragraphs
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)

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

(defun scroll-half-page-up ()
  "scroll down half the page"
  (interactive)
  (scroll-down (/ (window-body-height) 2)))

(defun scroll-half-page-down ()
  "scroll up half the page"
  (interactive)
  (scroll-up (/ (window-body-height) 2)))

(defun scroll-down-half-page ()
  "scroll down half a page while keeping the cursor centered"
  (interactive)
  (let ((ln (line-number-at-pos (point)))
        (lmax (line-number-at-pos (point-max))))
    (cond
     ((= ln 1)
      (move-to-window-line nil))
     ((= ln lmax)
      (recenter (window-end)))
     (t
      (progn
        (move-to-window-line -1)
        (recenter))))))

(defun scroll-up-half-page ()
  "scroll up half a page while keeping the cursor centered"
  (interactive)
  (let ((ln (line-number-at-pos (point)))
        (lmax (line-number-at-pos (point-max))))
    (cond
     ((= ln 1)
      nil)
     ((= ln lmax)
      (move-to-window-line nil))
     (t
      (progn
        (move-to-window-line 0)
        (recenter))))))

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

