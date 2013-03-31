;; Melpa and Marmalade are based on package.el
;; Emacs >= 24 includes package.el
(when (< emacs-major-version 24)
  (add-to-list 'load-path (expand-file-name "~/.emacs.d/package")))
(require 'package)

;; Taken from:
;; https://github.com/purcell/emacs.d/blob/master/init-elpa.el
(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
	(package-refresh-contents)
	(require-package packe min-version t)))))

;; Add standard package repositories
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)

;; Fire up package.el
(package-initialize)

(provide 'init-elpa)
