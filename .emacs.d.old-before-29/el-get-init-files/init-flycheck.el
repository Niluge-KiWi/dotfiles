(add-hook 'after-init-hook #'global-flycheck-mode)
(setq-default flycheck-disabled-checkers '(terraform-tflint))

(add-to-list 'flycheck-checkers 'html-aspell-dynamic)
(add-to-list 'flycheck-checkers 'markdown-aspell-dynamic)
