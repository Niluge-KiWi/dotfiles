;;; Emacs Bedrock
;;;
;;; Extra config: Development tools

;;; Usage: Append or require this file from init.el for some software
;;; development-focused packages.
;;;
;;; It is **STRONGLY** recommended that you use the base.el config if you want to
;;; use Eglot. Lots of completion things will work better.
;;;
;;; This will try to use tree-sitter modes for many languages. Please run
;;;
;;;   M-x treesit-install-language-grammar
;;;
;;; Before trying to use a treesit mode.

;;; Contents:
;;;
;;;  - Built-in config for developers
;;;  - Version Control
;;;  - Common file types
;;;  - Eglot, the built-in LSP client for Emacs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Built-in config for developers
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package emacs
  :config
  ;; Treesitter config

  ;; Tell Emacs to prefer the treesitter mode
  ;; You'll want to run the command `M-x treesit-install-language-grammar' before editing.
  (setq major-mode-remap-alist
        '((yaml-mode . yaml-ts-mode)
          (bash-mode . bash-ts-mode)
          (js2-mode . js-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (json-mode . json-ts-mode)
          (css-mode . css-ts-mode)
          (python-mode . python-ts-mode)))

  ;; style

  ;; default indent
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)

  ;; delete trailing whitespaces before save; TODO replace with https://git.sr.ht/~bkhl/trimspace-mode
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Version Control
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Magit: best Git client to ever exist
(use-package magit
  :ensure t
  :bind (("C-c C-s" . magit-status))      ; muscle memory
  :bind (:map magit-mode-map
	      ;; muscle memory legacy, maybe remove: use native ^ now that we use querty
	      ;;("o" . magit-section-up)
	      )

  :config
  ;; hide untracked section by default, in addition to stashes
  (setq magit-section-initial-visibility-alist
	'((stashes . hide)
	  (untracked . hide)))
  ;; no buffer saving when magit-status
  (setq magit-save-repository-buffers nil)
  ;; show process buffer for long operations
  (setq magit-process-popup-time 5)
  ;; magit-status: switch to buffer instead of pop to buffer
  (setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  ;; intra line diff highlight
  (setq magit-diff-refine-hunk t)
  ;; magit-status: try to go to corresponding hunk
  (setq magit-status-goto-file-position t)
  ;; https://github.com/magit/magit/issues/2012#issuecomment-619366605 ; but not really used
  (transient-append-suffix 'magit-log "-A"
			   '("-1" "First parent" "--first-parent"))
  )

(use-package magit-wip
  ;; Git WIP: https://github.com/bartman/git-wip
  :after magit
  :config
  ;; enable on all repositories
  ;; alternative: remove it and enable on per-repo basis:
  ;;   git config --add magit.extension wip-save
  (magit-wip-mode 1))

(use-package forge
  :ensure t
  :after magit)

(use-package git-commit
  :ensure t
  :hook ((git-commit-setup . git-commit-turn-on-flyspell)))

(use-package git-link
  :ensure t
  :bind ("C-x v w" . git-link))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Common file types
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package markdown-mode
  :hook ((markdown-mode . visual-line-mode)))

(use-package yaml-mode
  :ensure t)

(use-package json-mode
  :ensure t)

(use-package lua-mode
  :ensure t
  :mode "\\.lua$"
  :interpreter "lua")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Various file types
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package php-mode
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Eglot, the built-in LSP client for Emacs
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package eglot
  ;; no :ensure t here because it's built-in

  ;; Configure hooks to automatically turn-on eglot for selected modes
  ; :hook
  ; (((python-mode ruby-mode elixir-mode) . eglot))

  :custom
  (eglot-send-changes-idle-time 0.1)

  :config
  (fset #'jsonrpc--log-event #'ignore)  ; massive perf boost---don't log every event
  ;; Sometimes you need to tell Eglot where to find the language server
  ; (add-to-list 'eglot-server-programs
  ;              '(haskell-mode . ("haskell-language-server-wrapper" "--lsp")))
  )
