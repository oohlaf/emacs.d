(require-package 'org-fstree)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-agenda-files
      (list "~/Documents/org/personal.org"
            "~/Documents/org/hp.org"
            "~/Documents/org/people.org"))

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"
                        "PHONE" "MEETING"))))

(setq org-log-done t
      org-completion-use-ido t
      org-tag-column 80
      org-startup-indented t)

;; Wrap long lines without breaking individual words
(add-hook 'org-mode-hook
          (lambda ()
            (org-indent-mode t)
            (visual-line-mode t)))

;; On Windows enable Outlook integration
(if (or (eq system-type 'windows-nt)
        (eq system-type 'cygwin))
    (require-package 'org-outlook))

(provide 'init-org)
