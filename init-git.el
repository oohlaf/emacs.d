;;; Based on https://github.com/purcell/emacs.d/blob/master/init-git.el
(require-package 'magit)
(require-package 'git-gutter-fringe)
(require-package 'git-blame)
(require-package 'git-commit-mode)
(require-package 'gitignore-mode)
(require-package 'gitconfig-mode)

(eval-after-load 'magit
  '(progn
     ;; Full screen magit-status
     ;; Taken from http://whattheemacsd.com/setup-magit.el-01.html
     (defadvice magit-status (around magit-fullscreen activate)
       (window-configuration-to-register :magit-fullscreen)
       ad-do-it
       (delete-other-windows))

     (defun magit-quit-session ()
        "Restores the previous window configuration and kills the magit buffer"
        (interactive)
        (kill-buffer)
	(when (get-register :magit-fullscreen)
          (jump-to-register :magit-fullscreen)))

     (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)))

(provide 'init-git)
