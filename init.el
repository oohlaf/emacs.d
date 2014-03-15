;; No splash screen
(setq inhibit-startup-message t)

(let ((minver 24))
  (unless (>= emacs-major-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Load package system
(require 'init-elpa)

;; Configure specific features or modes
(require 'init-flycheck)
(require 'init-ido)
(require 'init-git)
;; Be smart about parens pairs
(require 'init-smartparens)
;; Show menu of recently edited files
(require 'init-recentfiles)
;; Org mode
(require 'init-org)

;; Language modes
(require 'init-lisp)
(require 'init-php)
(require 'init-python)

;; Layout
(require 'init-themes)
(require 'init-settings)

;; Variables configured via the interactive customize interface
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
