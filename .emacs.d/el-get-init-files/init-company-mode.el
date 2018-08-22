(setq company-backends (mapcar (lambda (element) (delete 'company-etags element)) company-backends))
