(with-eval-after-load 'company
  (define-key company-mode-map (kbd "M-/") 'company-complete-common-or-cycle)
  ;; don't automatically invoke company after a typing delay, because it automatically completes with the prefix common to all suggestions, which way too often injects wrong things without any confirmation
  (setq company-idle-delay nil)
  (setq company-backends (mapcar (lambda (element) (if (listp element) (delete 'company-etags element) element)) company-backends))
  (setq company-backends (delete 'company-capf company-backends))
  )
