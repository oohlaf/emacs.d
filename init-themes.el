(setq theme-name 'birds-of-paradise-plus)
(setq theme-pkg 'birds-of-paradise-plus-theme)

;(when (>= emacs-major-version 24)
  (require-package theme-pkg) ;)
  (load-theme theme-name)

(provide 'init-themes)
