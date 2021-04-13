(add-hook 'after-init-hook #'global-flycheck-mode)


(add-to-list 'flycheck-checkers 'html-aspell-dynamic)
(add-to-list 'flycheck-checkers 'markdown-aspell-dynamic)
