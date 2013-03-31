(setq theme-name 'birds-of-paradise-plus)
(setq theme-pkg 'birds-of-paradise-plus-theme)

;(when (>= emacs-major-version 24)
  (require-package theme-pkg) ;)

(setq-default custom-enabled-themes '(birds-of-paradise-plus))

(defun reapply-themes ()
  "Forcibly load the themes listed in custom-enabled-themes."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme t))))

(add-hook 'after-init-hook 'reapply-themes)

(provide 'init-themes)
