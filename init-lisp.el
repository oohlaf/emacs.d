;; Taken from
;; https://github.com/purcell/emacs.d/blob/master/init-lisp.el

;; Automatic byte compilation
(require-package 'auto-compile)
(auto-compile-on-save-mode 1)
(auto-compile-on-load-mode 1)

;; Support byte-compilation in a sub-process, as required by highlight-cl
(defun sanityinc/byte-compile-file-batch (filename)
  "Byte-compile FILENAME in batch mode, ie. a clean sub-process."
  (interactive "fFile to byte-compile in batch mode: ")
  (let ((emacs (car command-line-args)))
    (compile
      (concat
	emacs " "
	(mapconcat
	  'shell-quote-argument
	  (list "-Q" "-batch" "-f" "batch-byte-compile" filename)
	  " ")))))

;; Highlight current sexp
(require-package 'hl-sexp)

;; Prevent flickery behavior due to hl-sexp-mode unhighlighting
;; before each command
(eval-after-load 'hl-sexp
  '(defadvice hl-sexp-mode (after unflicker (&optional turn-on) activate)
     (when turn-on
       (remove-hook 'pre-command-hook #'hl-sexp-unhighlight))))

;; Enable desired features for all lisp modes
(require-package 'rainbow-delimiters)
(require-package 'highlight-cl)
(autoload 'highlight-cl-add-font-lock-keywords "highlight-cl")

(defun sanityinc/lisp-setup()
  "Enable features useful in any Lisp mode."
  (rainbow-delimiters-mode t)
  ;(enable-paredit-mode)
  (turn-on-eldoc-mode))

(defun sanityinc/emacs-lisp-setup()
  "Enable features useful when working with elisp."
  ;(elisp-slime-nav-mode t)
  ;(set-up-hippie-expand-for-elisp)
  ;(ac-emacs-lisp-mode-setup)
  (checkdoc-minor-mode))

(defconst sanityinc/elispy-modes
  '(emacs-lisp-mode ielm-mode)
  "Major modes relating to elisp.")

(defconst sanityinc/lispy-modes
  (append sanityinc/elispy-modes
	  '(lisp-mode inferior-lisp-mode lisp-interaction-mode))
  "All lispy major modes.")

(require 'derived)

(dolist (hook (mapcar #'derived-mode-hook-name sanityinc/lispy-modes))
  (add-hook hook 'sanityinc/lisp-setup))

(dolist (hook (mapcar #'derived-mode-hook-name sanityinc/elispy-modes))
  (add-hook hook 'sanityinc/emacs-lisp-setup))

;; Check for balanced parens
(defun sanityinc/maybe-check-parens ()
  "Run `check-parens' if this is a lispy mode."
  (when (memq major-mode sanityinc/lispy-modes)
    (check-parens)))

(add-hook 'after-save-hook #'sanityinc/maybe-check-parens)

(require-package 'eldoc-eval)


(add-to-list 'auto-mode-alist '("\\.emacs-project\\'" . emacs-lisp-mode))
;; package.el archive files
(add-to-list 'auto-mode-alist '("archive-contents\\'" . emacs-lisp-mode))

;; Enhance lisp modes keybindings
;; https://github.com/technomancy/emacs-starter-kit/blob/v2/modules/starter-kit-lisp.el
(define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)
(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)

;; Delete .elc files when reverting the .el from VC or magit
;;
;; When .el files are open, we can intercept when they are modified
;; by VC or magit in order to remove .elc files that are likely to
;; be out of sync.
;;
;; This is handy while actively working on elisp files, though
;; obviously it doesn't ensure that unopened files will also have
;; their .elc counterparts removed - VC hooks would be necessary for
;; that.
(defvar sanityinc/vc-reverting nil
  "Whether or not VC or Magit is currently reverting buffers.")

(defadvice revert-buffer (after sanityinc/maybe-remove-elc activate)
  "If reverting from VC, delete any .elc file that will now be out of sync."
  (when sanityinc/vc-reverting
    (when (and (eq 'emacs-lisp-mode major-mode)
               buffer-file-name
               (string= "el" (file-name-extension buffer-file-name)))
      (let ((elc (concat buffer-file-name "c")))
        (when (file-exists-p elc)
          (message "Removing out-of-sync elc file %s" (file-name-nondirectory elc))
          (delete-file elc))))))

(defadvice magit-revert-buffers (around sanityinc/reverting activate)
  (let ((sanityinc/vc-reverting t))
    ad-do-it))
(defadvice vc-revert-buffer-internal (around sanityinc/reverting activate)
  (let ((sanityinc/vc-reverting t))
    ad-do-it))

;; A quick way to jump to the definition of a function given its key binding
(global-set-key (kbd "C-h K") 'find-function-on-key)

(provide 'init-lisp)
