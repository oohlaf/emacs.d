;; No splash screen
(setq inhibit-startup-message t)

(add-to-list 'load-path (expand-file-name "~/.emacs.d"))

;; Load package system
(require 'init-elpa)

;; Configure specific features or modes
(require 'init-git)

;; Show menu of recently edited files
(require 'init-recentfiles)

;; Layout
(require 'init-themes)
(require 'init-settings)

;; Variables configured via the interactive customize interface
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))
