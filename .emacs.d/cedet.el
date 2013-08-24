;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Cedet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; loaded earlier, to avoid double load of EIEIO
;; (load-file "~/.emacs.d/el-get/cedet/common/cedet.el")
;; Choose one level of features
(semantic-load-enable-minimum-features)
;; (semantic-load-enable-code-helpers)
;; (semantic-load-enable-gaudy-code-helpers)
;; (semantic-load-enable-excessive-code-helpers)

;; This enables parsing of header files.
(setq semantic-idle-work-update-headers-flag t)

;; Tag decoration
(global-semantic-decoration-mode 1)
(require 'semantic-decorate-include)

;; Project management
(global-ede-mode t)

;; Bookmarks on tags, to navigate through them
;; TODO use ido for the bookmark ring
(global-semantic-mru-bookmark-mode 1)
(defun semantic-ia-fast-jump-back ()
  (interactive)
  (if (ring-empty-p (oref semantic-mru-bookmark-ring ring))
      (error "Semantic Bookmark ring is currently empty"))
  (let* ((ring (oref semantic-mru-bookmark-ring ring))
         (alist (semantic-mrub-ring-to-assoc-list ring))
         (first (cdr (car alist))))
    ;; (if (semantic-equivalent-tag-p (oref first tag) (semantic-current-tag))
    ;;     (setq first (cdr (car (cdr alist)))))
    (semantic-mrub-visit first)
    (ring-remove ring 0)))

;; Semantic Database
(require 'semanticdb)
(global-semanticdb-minor-mode 1)
(setq semanticdb-default-save-directory (expand-file-name "~/.emacs.d/semanticdb"))
;;(setq semanticdb-project-roots (list (expand-file-name "~/dev")))

;; Where to search for tags
(setq-mode-local c-mode semanticdb-find-default-throttle
                 '(local project unloaded system recursive))
(setq-mode-local c++-mode semanticdb-find-default-throttle
                 '(local project unloaded system recursive))

;; Automatically find system include path
;; require GNU "global" program
(require 'semantic-gcc)
;; Enable support for gnu global
(require 'semanticdb-global)
(semanticdb-enable-gnu-global-databases 'c-mode)
(semanticdb-enable-gnu-global-databases 'c++-mode)

;; Smart Completion and Jump
(require 'semantic-ia)

;; Enable ctags for some languages:
;;  Unix Shell, Perl, Pascal, Tcl, Fortran, Asm
;; require exuberent-ctags: "If you use C++ templates or boost, you should NOT enable it."
;;(semantic-load-enable-primary-exuberent-ctags-support)

;; Semantic-idle: what do do when idle
(global-semantic-idle-scheduler-mode 1)
(setq semantic-idle-scheduler-idle-time 2)
;; header-line: current context
(global-semantic-idle-breadcrumbs-mode 1)

;; Pulse: less cpu
(setq pulse-iterations 2)
(setq pulse-delay 0.1)

;; ECB
;; (semanticdb-create-ebrowse-database "/usr/include")

;; Keybindings
(defun my-cedet-hook ()
  ;;(local-set-key (kbd "<C-return>") 'semantic-ia-complete-symbol-menu) ;; TODO auto-complete instead
  (local-set-key (kbd "C-c ?") 'semantic-ia-complete-symbol)
  ;;
  (local-set-key (kbd "C-c >") 'semantic-complete-analyze-inline)
  (local-set-key (kbd "C-c =") 'semantic-decoration-include-visit)

  (local-set-key (kbd "C-c j") 'semantic-ia-fast-jump)
  (local-set-key (kbd "C-c b") 'semantic-mrub-switch-tags)
  (local-set-key (kbd "C-c C-d") 'semantic-ia-show-doc)
  (local-set-key (kbd "C-c d") 'semantic-ia-show-summary)
  (local-set-key (kbd "C-c p") 'semantic-analyze-proto-impl-toggle)
  (local-set-key (kbd "C-c r") 'semantic-symref)
  (local-set-key (kbd "C-c C-r") 'semantic-symref-symbol)
  ;; senator
  ;; TODO toggle folding
  (local-set-key (kbd "C-c -") 'senator-fold-tag)
  (local-set-key (kbd "C-c +") 'senator-unfold-tag)
  (local-set-key (kbd "C-c C-p") 'senator-previous-tag)
  (local-set-key (kbd "C-c C-n") 'senator-next-tag)

  ;; most used
  (local-set-key (kbd "C-j") 'semantic-ia-fast-jump)
  (local-set-key (kbd "M-j") 'semantic-ia-fast-jump-back)
  )

;;(add-hook 'semantic-init-hooks 'my-cedet-hook)
(add-hook 'c-mode-common-hook 'my-cedet-hook)
(add-hook 'emacs-lisp-mode-hook 'my-cedet-hook)

(defun my-c-mode-cedet-hook ()
  ;; TODO setup auto-complete with such prefix
  ;; (local-set-key (kbd ".") 'semantic-complete-self-insert)
  ;; (local-set-key (kbd ">") 'semantic-complete-self-insert)
  (local-set-key (kbd "C-c t") 'eassist-switch-h-cpp)
  (local-set-key (kbd "C-x t") 'eassist-switch-h-cpp)
  (local-set-key (kbd "C-c m") 'eassist-list-methods)
  )

(add-hook 'c-mode-common-hook 'my-c-mode-cedet-hook)


;; Parse a whole project:
;; export SEMANTIC_PATH=~/.emacs.d/el-get/cedet/semantic/
;; find . -name "*.h" -o -name "*.hh" -o -name "*.hxx" -o -name "*.hpp" -o -name "*.cpp" -o -name "*.cc" -o -name "*.c" | xargs ${SEMANTIC_PATH}/semanticdb.sh
;;
;; And to generate include-path:
;; find . \( -name "*.h" -o -name "*.hh" -o -name "*.hxx" -o -name "*.cpp" -o -name "*.cc" -o -name "*.c" \) -exec dirname "{}" \; | sort | uniq
