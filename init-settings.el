;; Always show line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; Use all columns (default is 72)
(setq fill-column 80)

;; Keep cursor away from edges
(require-package 'smooth-scrolling)

;; Sentences do not need double spaces to end. Period.
(set-default 'sentence-end-double-space nil)

;; Add parts of each file's directory to the buffer name if not unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(provide 'init-settings)
