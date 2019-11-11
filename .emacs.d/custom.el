(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ace-jump-face-background ((t (:foreground "#6f6f6f"))))
 '(ace-jump-face-foreground ((((class color)) (:foreground "#f0dfaf"))))
 '(diredp-date-time ((t (:foreground "#8fb28f"))))
 '(diredp-dir-heading ((t (:foreground "#7cb8bb"))))
 '(diredp-dir-priv ((t (:foreground "#8cd0d3"))))
 '(diredp-exec-priv ((t nil)))
 '(diredp-file-name ((t (:foreground "#dcdccc"))))
 '(diredp-file-suffix ((t (:foreground "#dcdccc"))))
 '(diredp-flag-mark ((t (:foreground "#cc9393"))))
 '(diredp-flag-mark-line ((t (:foreground "#cc9393" :background "#5f5f5f"))))
 '(diredp-ignored-file-name ((t (:foreground "#4c7073"))))
 '(diredp-inode+size ((t (:foreground "#7cb8bb"))))
 '(diredp-link-priv ((t nil)))
 '(diredp-no-priv ((t nil)))
 '(diredp-other-priv ((t nil)))
 '(diredp-rare-priv ((t nil)))
 '(diredp-read-priv ((t nil)))
 '(diredp-symlink ((t (:foreground "#5c888b"))))
 '(diredp-write-priv ((t nil)))
 '(ecb-default-highlight-face ((((class color) (background dark)) (:background "#75507b"))))
 '(ecb-tag-header-face ((((class color) (background dark)) (:background "#5f7f5f"))))
 '(erc-my-nick-face ((t (:foreground "#8c5353"))))
 '(js2-highlight-vars-face ((((class color) (background light)) (:background "#8cd0d3")) (((class color) (background dark)) (:background "#366060"))))
 '(js2-highlight-vars-second-face ((((class color) (background light)) (:background "#dc8cc3")) (((class color) (background dark)) (:background "#814c9e")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(ecb-layout-always-operate-in-edit-window (quote (switch-to-buffer)))
 '(ecb-options-version "2.40")
 '(elpy-rpc-python-command "python3")
 '(global-font-lock-mode t)
 '(inhibit-startup-echo-area-message (user-login-name))
 '(inhibit-startup-screen t)
 '(initial-buffer-choice t)
 '(initial-scratch-message nil)
 '(magit-cherry-pick-arguments nil)
 '(magit-git-global-arguments (quote ("--no-pager" "--literal-pathspecs")))
 '(magit-merge-arguments (quote ("--no-ff")))
 '(magit-popup-use-prefix-argument (quote default))
 '(magit-revert-buffers t t)
 '(magit-tag-arguments (quote ("--annotate")))
 '(menu-bar-mode nil)
 '(org-agenda-files
   (quote
    ("~/doc/agenda.org" "~/.emacs.d/org/todo.org" "~/.emacs.d/journal/")))
 '(safe-local-variable-values
   (quote
    ((eval ignore-errors "Write-contents-functions is a buffer-local alternative to before-save-hook"
           (add-hook
            (quote write-contents-functions)
            (lambda nil
              (delete-trailing-whitespace)
              nil))
           (require
            (quote whitespace))
           "Sometimes the mode needs to be toggled off and on."
           (whitespace-mode 0)
           (whitespace-mode 1))
     (whitespace-line-column . 80)
     (whitespace-style face tabs trailing lines-tail))))
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))
