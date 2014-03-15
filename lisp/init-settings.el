(setq line-number-mode t
      column-number-mode t
      ;; Use all columns (default is 72)
      fill-column 80
      ;; Avoid beeping by flashing the window
      visible-bell t)

;; Sentences do not need double spaces to end. Period.
(set-default 'sentence-end-double-space nil)

;; Accept y or n to answer yes or no questions
(defalias 'yes-or-no-p 'y-or-n-p)

;; Keep cursor away from edges
(require-package 'smooth-scrolling)

;; Key bindings
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; Add parts of each file's directory to the buffer name if not unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(provide 'init-settings)
