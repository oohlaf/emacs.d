;; No splash screen
(setq inhibit-startup-message t)

(add-to-list 'load-path (expand-file-name "~/.emacs.d"))

;; Load package system
(require 'init-elpa)

;; Configure specific features or modes
(require 'init-git)

(require 'init-recentfiles)
(require-package 'smooth-scrolling)

;; Variables configured via the interactive customize interface
(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

;; Finally enable my preferences
(require 'init-settings)
