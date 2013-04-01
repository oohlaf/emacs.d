(setq theme-pkg 'birds-of-paradise-plus-theme)
(setq theme-name 'birds-of-paradise-plus)

(when (>= emacs-major-version 24)
  (require-package theme-pkg)

  ;; Set a default theme
  ;; This will be overriden when a theme is defined in the custom-file
  (setq-default custom-enabled-themes (list theme-name))

  ;; Loop through all enabled themes in reverse order
  ;; and load those themes that are not yet defined.
  ;; The theme with the highest precedence is loaded last.
  ;;
  ;; When a custom-file is loaded containing safe themes,
  ;; all themes should be defined and this function should
  ;; do nothing.
  (defun reapply-themes ()
    (dolist (theme (reverse custom-enabled-themes))
      (unless (custom-theme-p theme)
        (message "Loading enabled theme %S." theme)
        (load-theme theme)))
    (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

  ;; Call reapply as init hook, so custom-files can change theme settings
  (add-hook 'after-init-hook 'reapply-themes))

(provide 'init-themes)
