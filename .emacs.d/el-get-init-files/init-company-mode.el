(with-eval-after-load 'company
  (define-key company-mode-map (kbd "M-/") 'company-complete-common-or-cycle)
  (setq company-backends (mapcar (lambda (element) (if (listp element) (delete 'company-etags element) element)) company-backends))
  (setq company-backends (delete 'company-capf company-backends))
  )
