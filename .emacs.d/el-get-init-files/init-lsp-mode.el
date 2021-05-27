;; need yas-minor-mode for company-capf to properly work, which is automatically used by lsp-mode
(add-hook 'lsp-mode-hook 'yas-minor-mode)


;; yaml
;; apply kubernetes yaml schema on kubernetes yaml files
(setq lsp-yaml-schemas
      '(:kubernetes "/*-k8s.yaml"
        :kubernetes "/kubernetes-*.yaml"))
(add-hook 'yaml-mode-hook #'lsp-deferred)

;; terraform
(defun lsp-terraform--make-launch-cmd ()
  '("terraform-ls" "serve"))
