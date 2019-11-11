(with-eval-after-load 'company
  (setq company-backends (mapcar (lambda (element) (if (listp element) (delete 'company-etags element) element)) company-backends)))
