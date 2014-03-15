(require-package 'org-fstree)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; On Windows enable Outlook integration
(if (or (eq system-type 'windows-nt)
        (eq system-type 'cygwin))
  (require-package 'org-outlook))

(provide 'init-org)
