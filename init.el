;;; init.el --- Olaf's Emacs Configuration

;;; Commentary:
;; 

;;; Code:

;;;_* Initialization

(defconst emacs-start-time (current-time))

;; No splash screen
(setq inhibit-startup-message t)

(unless noninteractive
  (message "Loading %s..." load-file-name))

(let ((minver 24))
  (unless (>= emacs-major-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))

;;;_ * Packaging

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(setq package-enable-at-startup nil)
(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")))

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
        (require-package package min-version t)))))

(require 'package)
(package-initialize)

(require-package 'use-package)
(require 'use-package)
(setq use-package-idle-interval 2)

;;;_ * Utility macros and functions

(defmacro hook-into-modes (func modes)
  `(dolist (mode-hook ,modes)
     (add-hook mode-hook ,func)))

(defvar lisp-modes '(emacs-lisp-mode
                     inferior-emacs-lisp-mode
                     ielm-mode
                     lisp-mode
                     inferior-lisp-mode
                     lisp-interaction-mode
                     slime-repl-mode))

(defvar lisp-mode-hooks
  (mapcar (function
           (lambda (mode)
             (intern
              (concat (symbol-name mode) "-hook"))))
          lisp-modes))

;;;_* Packages

;;;_ * Allout

(use-package allout
  :diminish allout-mode
  :commands allout-mode
  :config
  (progn
    (defvar allout-unprefixed-keybindings nil)

    (defun my-allout-mode-hook ()
      (dolist (mapping '((?b . allout-hide-bodies)
                         (?c . allout-hide-current-entry)
                         (?l . allout-hide-current-leaves)
                         (?i . allout-show-current-branches)
                         (?e . allout-show-entry)
                         (?o . allout-show-to-offshoot)))
        (eval `(bind-key ,(concat (format-kbd-macro allout-command-prefix)
                                  " " (char-to-string (car mapping)))
                         (quote ,(cdr mapping))
                         allout-mode-map)))

      (if (memq major-mode lisp-modes)
          (unbind-key "C-k" allout-mode-map)))

    (add-hook 'allout-mode-hook 'my-allout-mode-hook)))

;;;_ * Ido

(use-package ido
  :init
  (progn
    (ido-everywhere t)
    (ido-mode t))
  :config
  (progn
    (use-package ido-hacks
      :ensure t
      :init (ido-hacks-mode 1))

    ;; Silence warning references to free variables when
    ;; compiling ido-ubiquitous
    (defvar predicate nil)
    (defvar inherit-input-method nil)
    (defvar ido-cur-item nil)
    (defvar ido-cur-list nil)
    (defvar ido-default-item nil)

    (use-package ido-ubiquitous
      :ensure t
      :commands ido-ubiquitous-mode
      :init (ido-ubiquitous-mode 1))

    (use-package idomenu
      :ensure t
      :bind ("C-c i" . idomenu)
      :config (setq imenu-auto-rescan t))

    (use-package smex
      :ensure t
      :bind ("M-x" . smex)
      :init (smex-initialize))

    (setq ido-enable-flex-matching t
          ido-use-filename-at-point nil
          ido-auto-merge-work-directories-length 0
          ido-use-virtual-buffers t
          ido-default-buffer-method 'selected-window
          ido-default-file-method 'selected-window)))

;;;_ * Lisp

(use-package lisp-mode
  :init
  (progn
    (defvar lisp-mode-initialized nil)
    (defun initialize-lisp-mode ()
      (unless lisp-mode-initialized
        (setq lisp-mode-initialized t)

        (use-package eldoc
          :diminish eldoc-mode
          :defer t
          :init
          (use-package eldoc-extension
            :ensure t
            :defer t
            :init
            (add-hook 'emacs-lisp-mode-hook
                      #'(lambda () (require 'eldoc-extension)) t)))))

    (use-package highlight-cl
      :ensure t
      :init
      (mapc (function
             (lambda (mode-hook)
               (add-hook mode-hook
                         'highlight-cl-add-font-lock-keywords)))
            lisp-mode-hooks))

    (defun my-elisp-indent-or-complete (&optional arg)
      (interactive "p")
      (call-interactively 'lisp-indent-line)
      (unless (or (looking-back "^\\s-*")
                  (bolp)
                  (not (looking-back "[-A-Za-z0-9_*+/=<>!?]+")))
        (call-interactively 'lisp-complete-symbol)))

    (defun my-lisp-indent-or-complete (&optional arg)
      (interactive "p")
      (if (or (looking-back "^\\s-*") (bolp))
          (call-interactively 'lisp-indent-line)
        (call-interactively 'slime-indent-and-complete-symbol)))

    (defun my-lisp-mode-hook ()
      (initialize-lisp-mode)
      (auto-fill-mode 1)
      (eldoc-mode 1)

      (if (memq major-mode
                '(emacs-lisp-mode inferior-emacs-lisp-mode ielm-mode))
          (progn
            (bind-key "<M-return>" 'outline-insert-heading emacs-lisp-mode-map)
            (bind-key "<tab>" 'my-elisp-indent-or-complete emacs-lisp-mode-map))
        (bind-key "<tab>" 'my-lisp-indent-or-complete lisp-mode-map)))

    (hook-into-modes #'my-lisp-mode-hook lisp-mode-hooks)))

;;;_* Settings

(setq
 ;; Show line and column numbers
 line-number-mode t
 column-number-mode t
 ;; Avoid beeping, just flash the window
 visible-bell t)

;; Don't end sentences with double spaces within paragraphs. Period.
(set-default 'sentence-end-double-space nil)

(setq-default
 ;; No tabs, just spaces
 indent-tabs-mode nil
 ;; Use all columns (default is 72)
 fill-column 80)

;; Accept y or n to answer yes or no questions
(defalias 'yes-or-no-p 'y-or-n-p)

;; On Windows speed up file operations
(if (or (eq system-type 'windows-nt)
        (eq system-type 'cygwin))
    (setq w32-get-true-file-attributes nil))

;; Use server mode when in a GUI
(when (and window-system (not noninteractive))
  (add-hook 'after-init-hook 'server-start t))

;; Variables configured via the interactive customize interface
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(when window-system
  (let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
    (message "Loading %s...done (%.3fs)" load-file-name elapsed))
  (add-hook 'after-init-hook
            `(lambda ()
               (let ((elapsed (float-time (time-subtract (current-time)
                                                         emacs-start-time))))
                 (message "Loading %s...done (%.3fs) [after-init]"
                          ,load-file-name elapsed)))
            t))

;;;_* Closing

(provide 'init)

;; Local Variables:
;; mode: emacs-lisp
;; mode: allout
;; End:

;;; init.el ends here
