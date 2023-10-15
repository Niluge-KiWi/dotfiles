(add-hook 'yaml-mode-hook
          (lambda ()
            (highlight-indentation-mode)))

(add-to-list 'auto-mode-alist '("\\.dockerapp$" . yaml-mode))
