;;; Emacs Bedrock
;;;
;;; Extra config: Base enhancements

;;; Usage: Append or require this file from init.el to enable various UI/UX
;;; enhancements.

;;; Contents:
;;;
;;;  - Motion aids
;;;  - Power-ups: Embark and Consult
;;;  - Minibuffer and completion
;;;  - Misc. editing enhancements

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Motion aids
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package avy
  :ensure t
  :demand t
  :bind (("C-c j" . avy-goto-line)
         ("s-j"   . avy-goto-char-timer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Power-ups: Embark and Consult
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Consult: Misc. enhanced commands
(use-package consult
  :ensure t
  ;; Other good things to bind: consult-line-multi, consult-history,
  ;; consult-outline, consult-org-agenda, etc.
  :bind (("C-x b" . consult-buffer)  ; orig. switch-to-buffer
         ("M-y" . consult-yank-pop)  ; orig. yank-pop
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         )
  :config
  ;; Narrowing lets you restrict results to certain groups of candidates
  (setq consult-narrow-key "<")
  )

;; track recent files, available in consult-buffer
(use-package recentf
  :config
  ;; save more than 20 files on exit
  (setq recentf-max-saved-items 1000)
  ;; enable global minor mode
  (recentf-mode t))

(use-package embark
  :ensure t
  :demand t
  :after avy
  :bind (("C-." . embark-act)   ; doesn't work in terminal
	 ("C-c a" . embark-act) ; backup for terminal
	 ("M-." . embark-dwim)  ; unsure which to use yet
	 ("C-;" . embark-dwim)  ; unsure which to use yet. back with M-, is unnatural with this
	 ("C-h B" . embark-bindings)) ; alternative for `describe-bindings'

  :init
  ;; Add the option to run embark when using avy
  (defun bedrock/avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  ;; After invoking avy-goto-char-timer, hit "." to run embark at the next
  ;; candidate you select
  (setf (alist-get ?. avy-dispatch-alist) 'bedrock/avy-action-embark))

(use-package embark-consult
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Minibuffer and completion
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Vertico: better vertical completion for minibuffer commands
(use-package vertico
  :ensure t
  :init
  (defun my/match-components-literally ()
    "Components match literally for the rest of the session."
    (interactive)
    (setq-local orderless-matching-styles '(orderless-literal)
		orderless-style-dispatchers nil))
  (vertico-mode)

  :bind
  (:map vertico-map
        ("C-l" . my/match-components-literally)
	    ("C-o" . embark-export))

  :config
  ;; needed with `read-file-name-completion-ignore-case'.
  ;; cf:
  ;; - https://github.com/minad/vertico/issues/341
  ;; - https://debbugs.gnu.org/cgi/bugreport.cgi?bug=60264
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy))

(use-package vertico-directory
  :after vertico
  :bind (:map vertico-map
              ("M-DEL" . vertico-directory-delete-word)))

;; Marginalia: annotations for minibuffer
(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

;; Popup completion-at-point
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :bind
  (:map corfu-map
        ("SPC" . corfu-insert-separator)
        ("C-n" . corfu-next)
        ("C-p" . corfu-previous)))

;; Part of corfu
(use-package corfu-popupinfo
  :after corfu
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(0.25 . 0.1))
  (corfu-popupinfo-hide nil)
  :config
  (corfu-popupinfo-mode))

;; Make corfu popup come up in terminal overlay
(use-package corfu-terminal
  :if (not (display-graphic-p))
  :ensure t
  :config
  (corfu-terminal-mode))

;; Fancy completion-at-point functions; there's too much in the cape package to
;; configure here; dive in when you're comfortable!
(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

;; Pretty icons for corfu
(use-package kind-icon
  :if (display-graphic-p)
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package eshell
  :bind (:map eshell-mode-map
              ("C-r" . consult-history)))

;; Orderless: powerful completion style
(use-package orderless
  :ensure t
  :config
  ;; note to the future: this was the big hard new thing to configure when starting from scratch in 2023-10 with emacs-bedrock,
  ;; main issue:
  ;; - C-s bound to consult-line, instead of isearch-forward
  ;; - I wanted flex search like I had with IDO (but not for string search in buffer)
  ;; - completion-category-overrides does *not* override completion-styles, but preprends it
  ;; this can be changed again:
  ;; - check consult readme & wiki, orderless readme, possibly issues there too.
  ;; - maybe just drop consult-line? or maybe change default orderless style around consult-line?


  ;; Make the stock file and buffer completion styles case insensitive, more coherent with 'orderless-smart-case'
  (setq read-file-name-completion-ignore-case t)
  (setq read-buffer-completion-ignore-case t)


  (orderless-define-completion-style orderless+initialism
    (orderless-matching-styles '(orderless-initialism
				                 orderless-literal
                                 orderless-regexp)))

  (orderless-define-completion-style orderless+initialism+flex
    (orderless-matching-styles '(orderless-initialism
				 orderless-flex
				 orderless-literal
				 orderless-regexp)))

  ;; List of completion styles to use, everywhere, after the per-category styles
  ;; (orderless readme: The 'basic' completion style is specified as fallback in addition to orderless in order to ensure that completion commands which rely on dynamic completion tables, e.g., completion-table-dynamic or completion-table-in-turn, work correctly)
  (setq completion-styles '(orderless basic))
  ;; disable default specialization per category
  (setq completion-category-defaults nil)
  ;; Note that 'completion-category-overrides' is not really an override, but rather prepended to the default 'completion-styles'.
  (setq completion-category-overrides
	'(
	  ;; for files:
	  ;; - for TRAMP: need 'basic' *first*
	  ;; - the 'partial-completion' style allows you to use wildcards for file completion and partial paths, e.g., '/u/s/l' for '/usr/share/local'.
	  ;; - 'orderless' with flex after 'partial-completion'
	  (file (styles basic partial-completion orderless+initialism+flex))

	  ;; for buffer:
	  ;; - since we have recentf in C-x b consult-buffers, strings that have matched when opening a file should still match the 'file' (recentf) source/group in consult-buffers
	  ;;   => buffers should be (usually/almost) as verbose in matching as file
	  ;; TODO fix, somehow setting orderless+initialism+flex (or any orderless style with flex) does *not* work for buffers, but do work for files, whyyy :cry:
	  (buffer (styles partial-completion orderless+initialism+flex))

      ;; for M-x: also flex
	  (command (styles orderless+initialism+flex))
      ;; for some other completions categories: initialism
      (symbol (styles orderless+initialism))
      (variable (styles orderless+initialism)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Misc. editing enhancements
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ctrlf
  ;; same keybindings as isearch
  :ensure t
  :init
  (ctrlf-mode 1)
  :bind (:map ctrlf-minibuffer-mode-map
              ;; muscle-memory; try to use "M-s o" instead?
              ("C-o" . ctrlf-occur)
              ;; muscle-memory; try to use "M-s ." instead?
              ("C-w" . ctrlf-forward-symbol-at-point)
              ;; TODO legacy, maybe remove that?; try to use "M-s _" instead?
              ("C-e" . ctrlf-toggle-symbol))
  )

;; Modify search results en masse, using ripgrep/rg instead of grep with wgrep
(use-package rg
  :ensure t
  :config
  (setq wgrep-auto-save-buffer t)
  ;; C-c s to open transient menu for ripgrep
  (setq rg-keymap-prefix (kbd "C-c s"))
  (rg-enable-default-bindings))
