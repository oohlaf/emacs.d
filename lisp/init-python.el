(when (>= emacs-major-version 24)
  (require-package 'elpy)
  (elpy-enable))

(provide 'init-python)
