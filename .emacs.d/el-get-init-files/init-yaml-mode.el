;;; init-yaml-mode.el --- el-get init file for package yaml-mode

(add-hook 'yaml-mode-hook
          (lambda ()
            (highlight-indentation-mode)))

(add-to-list 'auto-mode-alist '("\\.dockerapp$" . yaml-mode))
