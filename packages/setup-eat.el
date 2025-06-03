(use-package eat
  :ensure t)

(customize-set-variable
       'eat-semi-char-non-bound-keys
       (append
        (list (vector meta-prefix-char ?o))
        eat-semi-char-non-bound-keys))
