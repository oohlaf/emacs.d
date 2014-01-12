(require-package 'php-mode)
(require-package 'flymake-php)

(add-hook 'php-mode-hook
  (lambda ()
    'flymake-php-load
    (php-enable-wordpress-coding-style)))

(require-package 'geben)
(autoload 'geben "geben" "DBGp protocol frontend, a script debugger" t)

(provide 'init-php)
