(when (>= emacs-major-version 24)
  (require-package 'elpy)
  (elpy-enable)
  ;; Remove flymake mode from elpy when flycheck is installed
  (when (package-installed-p 'flycheck)
    (setq elpy-default-minor-modes
          (remove 'flymake-mode
                  elpy-default-minor-modes)))
  ;; Auto indent
  (define-key python-mode-map (kbd "RET")
              'newline-and-indent))

(provide 'init-python)
