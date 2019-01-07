(with-eval-after-load 'company
  (setq company-backends (mapcar (lambda (element) (delete 'company-etags element)) company-backends)))
