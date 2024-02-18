;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 2023-10: .emacs bankruptcy, started again from emacs bedrock
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Minimal init.el

;;; Contents:
;;;
;;;  - Basic settings
;;;  - Discovery aids
;;;  - Minibuffer/completion settings
;;;  - Interface enhancements/defaults
;;;  - Tab-bar configuration
;;;  - Theme
;;;  - Navigation/edition
;;;  - Optional extras
;;;  - Built-in customization framework

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Basic settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Guardrail

(when (< emacs-major-version 29)
  (error (format "Emacs Bedrock only works with Emacs 29 and newer; you have version ~a" emacs-major-version)))

;; Package initialization
;;
;; Use default package archives, with MELPA
(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))

;; own code
(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))


(setq initial-major-mode 'fundamental-mode)  ; default mode for the *scratch* buffer

;; Automatically reread from disk if the underlying file changes, using inotify
(setq auto-revert-check-vc-info t)
(global-auto-revert-mode)

;; Save history of minibuffer
(savehist-mode)

;; Save and restore last cursor place in files
(save-place-mode)

;; Fix archaic defaults
(setq sentence-end-double-space nil)

;; Make right-click do something sensible
(when (display-graphic-p)
  (context-menu-mode))

;; Don't litter file system with *~ backup files; put them all inside
;; ~/.emacs.d/backup or wherever
(defun bedrock--backup-file-name (fpath)
  "Return a new file path of a given file path.
If the new path's directories does not exist, create them."
  (let* ((backupRootDir (expand-file-name "emacs-backup/" user-emacs-directory))
         (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path
         (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") )))
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath))
(setq make-backup-file-name-function 'bedrock--backup-file-name)

;; Quit minibuffer even when we lost focus
;; see http://www.emacswiki.org/emacs/IgnacioKeyboardQuit , with a little bit of modifications
(defun my-keyboard-quit()
  "Escape the minibuffer or cancel region consistently using 'Control-g'.
Normally if the minibuffer is active but we lost focus (say, we clicked away or set the cursor into another buffer)
we can quit by pressing 'ESC' three times. This function handles it more conveniently, as it checks for the condition
of not being in the minibuffer but having it active. Otherwise simply doing the ESC or (keyboard-escape-quit) would
brake whatever split of windows we might have in the frame."
  (interactive)
  (if (and (not (window-minibuffer-p (selected-window)))
	       (or mark-active (active-minibuffer-window)))
      (keyboard-escape-quit)
    (keyboard-quit)))
(keymap-global-set "C-g" 'my-keyboard-quit)

;; remove useless default global keybindings
(keymap-global-unset "C-x C-r")         ; find-file-read-only; muscle-memory to unlearn: open recent file; merged into ~consult-file C-x C-f
(keymap-global-unset "C-x f")           ; set-fill-column
(keymap-global-unset "C-x C-d")         ; list-directory: use dired C-x d instead
(keymap-global-unset "C-x C-q")         ; read-only-mode


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Discovery aids
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Show the help buffer after startup
(add-hook 'after-init-hook 'help-quick)

;; which-key: shows a popup of available keybindings when typing a long key
;; sequence (e.g. C-x ...)
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Minibuffer/completion settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For help, see: https://www.masteringemacs.org/article/understanding-minibuffer-completion
;; TODO review that with extra/base.el

(setq enable-recursive-minibuffers t)                ; Use the minibuffer whilst in the minibuffer
(setq completion-cycle-threshold 1)                  ; TAB cycles candidates
(setq completions-detailed t)                        ; Show annotations
(setq tab-always-indent 'complete)                   ; When I hit TAB, try to complete, otherwise, indent
(setq completion-styles '(basic initials substring)) ; Different styles to match input to candidates

(setq completion-auto-help 'always)                  ; Open completion always; `lazy' another option
(setq completions-max-height 20)                     ; This is arbitrary
(setq completions-detailed t)
(setq completions-format 'one-column)
(setq completions-group t)
(setq completion-auto-select 'second-tab)            ; Much more eager
;(setq completion-auto-select t)                     ; See `C-h v completion-auto-select' for more possible values

(keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete) ; TAB acts more like how it does in the shell

;; See also the file extras/base.el

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Interface enhancements/defaults
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Mode line information
(setq line-number-mode t)                        ; Show current line in modeline
(setq column-number-mode t)                      ; Show column as well

(setq x-underline-at-descent-line nil)           ; Prettier underlines
(setq switch-to-buffer-obey-display-actions t)   ; Make switching buffers more consistent

(setq-default show-trailing-whitespace nil)      ; By default, don't underline trailing spaces
(setq-default indicate-buffer-boundaries 'left)  ; Show buffer top and bottom in the margin

;; Enable horizontal scrolling
(setq mouse-wheel-tilt-scroll t)
(setq mouse-wheel-flip-direction t)

;; Mouse, clipboard, primary selection
(setq x-selection-timeout 100)
;; selection is put in the X primary selection
(setq select-enable-primary t)
;; and not in X clipboard
(setq select-enable-clipboard nil)
;; including transient mark
(setq select-active-regions t)

(defun toggle-clipboard-selection ()
  "Toggle clipboard/primary selection"
  (interactive)
  (if select-enable-clipboard
      (progn
        (setq select-enable-clipboard nil
              select-enable-primary t)
        (message "Primary selection"))
    (setq select-enable-clipboard t
          select-enable-primary nil)
    (message "Clipboard")))

(defun focus-new-frames (&optional frame)
  "Focus new frames."
  (select-frame-set-input-focus frame))
(add-hook 'after-make-frame-functions 'focus-new-frames)

;; Misc. UI tweaks
(blink-cursor-mode -1)                                ; Steady cursor
(pixel-scroll-precision-mode)                         ; Smooth scrolling

;; Display line numbers in programming mode
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq-default display-line-numbers-width 3)           ; Set a minimum width

;; Nice line wrapping when working with text
(add-hook 'text-mode-hook 'visual-line-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; highlights
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package hl-line
  ;; Modes to highlight the current line with
  :hook ((text-mode-hook prog-mode-hook) . hl-line-mode))

(use-package highlight-indentation
  :ensure t
  )


;; see also symbol-overlay

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tab-bar configuration
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Hide the tab bar when it has only one tab
(setq tab-bar-show 1)

;; Add the time to the tab-bar, if visible
(add-to-list 'tab-bar-format 'tab-bar-format-align-right 'append)
(add-to-list 'tab-bar-format 'tab-bar-format-global 'append)
(setq display-time-format "%a %F %R")
(setq display-time-default-load-average nil) ; this information is useless for most
(display-time-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Theme
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package solarized-theme
  :ensure t
  :init
  ;; make the fringe stand out from the background
  (setq solarized-distinct-fringe-background t)
  ;; Don't change the font for some headings and titles
  ;;(setq solarized-use-variable-pitch nil)
  ;; Don't change size of org-mode headlines (but keep other size-changes)
  ;;(setq solarized-scale-org-headlines nil)
  ;; Change the size of markdown-mode headlines (off by default)
  (setq solarized-scale-markdown-headlines t)
  ;; make the modeline high contrast
  (setq solarized-high-contrast-mode-line t)
  ;; finally, load and enable the theme
  :config
  (load-theme 'solarized-light-high-contrast t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Window and buffer management
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Move through windows with Alt-<arrow keys>
(windmove-default-keybindings 'meta)

;; Move buffer through windows with Alt-Shift-<arrow keys>
(use-package windswap
  :ensure t
  :init
  (windswap-default-keybindings 'meta 'shift))

;; resize windows
(use-package emacs
  :bind
  (("S-C-<left>" . shrink-window-horizontally)
   ("S-C-<right>" . enlarge-window-horizontally)
   ("S-C-<down>" . shrink-window)
   ("S-C-<up>" . enlarge-window))
  )

;; Cancel and redo windows configurations
(use-package winner
  :init
  ;; C-c left/right to undo/redo changes in window configuration
  (winner-mode))


;; buffer management
(use-package ibuffer
  :bind
  (("C-x C-b" . ibuffer)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Navigation/edition
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; global key bindings for navigation
(use-package emacs
  :bind
  (("C-M-<" . end-of-buffer)))            ; easier on typematrix


;; global key bindings for edition
(use-package emacs
  :init
  (defun my/kill-whitespace ()
    "Kill the whitespace between two non-whitespace characters"
    (interactive "*")
    (save-excursion
      (save-restriction
        (save-match-data
	      (progn
	        (re-search-backward "[^ \t\r\n]" nil t)
	        (re-search-forward "[ \t\r\n]+" nil t)
	        (replace-match "" nil nil))))))
  :bind
  (("<C-return>" . ffap)        ; find file (or url) at point
   ("M-k" . my/kill-whitespace) ; kill-whitespace seems more usefull than kill-sentence
   ("C-x k" . kill-this-buffer) ; no need to ask which buffer to kill
   ("C-c u" . uncomment-region)
   ("C-c c" . comment-region)))

(use-package symbol-overlay
  :ensure t
  :bind
  (;; old, muscle-memory keybindings
   ("C-1" . symbol-overlay-put)
   ("C-2" . symbol-overlay-jump-next)
   ("C-3" . symbol-overlay-jump-prev)
   ("C-0" . symbol-overlay-remove-all)
   ;; new, suggested keybindings; experimental key binding
   ("M-i" . symbol-overlay-put)
   ("M-n" . symbol-overlay-jump-next)
   ("M-p" . symbol-overlay-jump-prev)
   (:map symbol-overlay-map
	     ;; TODO some key to remove-all
	     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Various keybindings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package keyfreq
  :ensure t
  :config
  (setq keyfreq-file (expand-file-name "keyfreq" user-emacs-directory))
  (setq keyfreq-file-lock (expand-file-name "keyfreq.lock" user-emacs-directory))
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Optional extras
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Uncomment the (load-file …) lines or copy code from the extras/ elisp files
;; as desired

;; UI/UX enhancements mostly focused on minibuffer and autocompletion interfaces
(load-file (expand-file-name "extras/base.el" user-emacs-directory))

;; Packages for software development
(load-file (expand-file-name "extras/dev.el" user-emacs-directory))

;; Vim-bindings in Emacs (evil-mode configuration)
;(load-file (expand-file-name "extras/vim-like.el" user-emacs-directory))

;; Org-mode configuration
(load-file (expand-file-name "extras/org.el" user-emacs-directory))

;; Tools for academic researchers
;(load-file (expand-file-name "extras/researcher.el" user-emacs-directory))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Customs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load-file custom-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Personal config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq personal-config-file (expand-file-name "perso.el" user-emacs-directory))
(if (file-exists-p personal-config-file)
    (load-file personal-config-file)
  (message "No personal config file found."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Work config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq work-config-file (expand-file-name "work.el" user-emacs-directory))
(if (file-exists-p work-config-file)
    (load work-config-file)
  (message "No work config file found."))
