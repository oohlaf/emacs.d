(require-package 'php-mode)

(add-hook 'php-mode-hook 'php-enable-wordpress-coding-style)

(require-package 'geben)
(autoload 'geben "geben" "DBGp protocol frontend, a script debugger" t)

(provide 'init-php)
