(setq line-number-mode t
      column-number-mode t
      ;; Use all columns (default is 72)
      fill-column 80
      ;; Avoid beeping by flashing the window
      visible-bell t)

;; Sentences do not need double spaces to end. Period.
(set-default 'sentence-end-double-space nil)

;; Keep cursor away from edges
(require-package 'smooth-scrolling)

;; Add parts of each file's directory to the buffer name if not unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(provide 'init-settings)
