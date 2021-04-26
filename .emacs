;; -*- coding: utf-8 -*-
;;; Emacs of Thomas Riccardi. Homepage : http://github.com/Niluge-KiWi/dotfiles
;; Many things from http://github.com/antoine-levitt/perso, and some other things

;; Can be viewed in outline mode

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Base setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; increase garbage collecting start threshold
(setq gc-cons-threshold (* 20 1024 1024))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Path setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/"))
;;(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/erc-5.3/"))


;;byte-recompile elisp files if they need to be
;;(byte-recompile-directory "~/.emacs.d" 0)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Package Management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;-------ELPA
;; ;; This provides support for the package system and
;; ;; interfacing with ELPA, the package archive.
;; (unless (boundp 'list-packages)
;;   (when
;;       (load
;;        (expand-file-name "~/.emacs.d/elpa/package.el"))
;;     (package-initialize)))
;; several archives for elpa
(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
	("melpa" . "https://melpa.org/packages/")
	("melpa-stable" . "https://stable.melpa.org/packages/")
	("org" . "http://orgmode.org/elpa/"))
      package-archive-priorities
      '(("gnu" . 5)
	("melpa" . 0)
	("melpa-stable" . 10)
	("org" . 20)))
(package-initialize)

;;-------el-get
;; Manage the external elisp bits and pieces you depend upon
(add-to-list 'load-path (expand-file-name "~/.emacs.d/el-get/el-get"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(setq el-get-user-package-directory "~/.emacs.d/el-get-init-files/")

(setq el-get-sources
      '(
        ace-jump-mode
        ace-window
        ag
        (:name cmake-font-lock :type git
               :url "https://github.com/Lindydancer/cmake-font-lock.git")
        apache-mode
        (:name bpftrace-mode :type elpa
               :repo ("melpa" . "https://melpa.org/packages/"))
        browse-kill-ring
        buffer-move

        cmake-mode
        (:name coffee-mode
               :type git
               :url "https://github.com/defunkt/coffee-mode"
               :description "Emacs Major Mode for CoffeeScript"
               :features coffee-mode
               :prepare (progn
                          (add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
                          (add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))))
        color-theme-zenburn
        (:name company :type elpa)
        (:name company-quickhelp :type elpa)
        (:name company-statistics :type elpa)
        (:name company-terraform :type elpa)
        (:name cucumber
               :description "Emacs mode for editing Cucumber plain text stories"
               :type git
               :url "https://github.com/michaelklishin/cucumber.el.git")
        (:name dash :type elpa
               :repo ("melpa" . "https://melpa.org/packages/"))
        (:name docker :type elpa
               :repo ("melpa" . "https://melpa.org/packages/"))
        (:name dockerfile-mode
               :description "An emacs mode for handling Dockerfiles."
               :type git
               :url "https://github.com/spotify/dockerfile-mode"
               :prepare (progn
                          (add-to-list 'auto-mode-alist
                                       '("Dockerfile" . dockerfile-mode))))
        (:name docker-tramp :type elpa
               :repo ("melpa" . "https://melpa.org/packages/"))
        (:name elpy :type elpa)
        erc-view-log
        (:name expand-region :type git
               :url "https://github.com/magnars/expand-region.el.git")
        flx
        (:name flycheck :type elpa
               :repo ("melpa" . "https://melpa.org/packages/"))
        fold-dwim
        (:name forge :type elpa
               :repo ("melpa" . "https://melpa.org/packages/"))
        (:name git-link :type elpa)
        (:name gnuplot :type elpa)
        (:name go-company :type elpa)
        go-def
        go-eldoc
        go-errcheck
        go-flymake
        go-imports
        go-lint
        go-mode
        go-rename
        go-template-mode
        go-test
        (:name graphviz-dot-mode :type: elpa)
        (:name grep-a-lot :type git
               :url "https://github.com/emacsmirror/grep-a-lot.git"
               :features grep-a-lot)
        groovy-emacs-mode
        (:name handlebars :type git
               :url "https://github.com/danielevans/handlebars-mode.git"
               :features handlebars-mode)
        (:name hide-lines :type emacswiki)
        (:name highlight-indentation)
        (:name highlight-parentheses)
        (:name highlight-symbol)
        ido-completing-read-plus
        (:name iedit :type git
               :url "https://github.com/victorhge/iedit.git")
        (:name ioccur :type git
               :url "https://github.com/thierryvolpiatto/ioccur.git")
        (:name jade :type git
               :url "https://github.com/brianc/jade-mode.git")
        (:name jinja2-mode :type elpa)
        (:name js2 :type git
               :url "https://github.com/mooz/js2-mode.git")
        (:name jshint-mode :type git
               :url "https://github.com/daleharvey/jshint-mode.git")
        json-mode
        keyfreq
        (:name lsp-mode :type elpa
               :repo ("melpa" . "https://melpa.org/packages/"))
        (:name lsp-docker :type elpa)
        lua-mode
        (:name magit :type elpa
               :repo ("melpa" . "https://melpa.org/packages/"))
        markdown-mode
        mediawiki
        (:name miniedit :type git
               :url "https://github.com/emacsmirror/miniedit.git")
        (:name modern-cpp-font-lock :type elpa)
        (:name multi-eshell :type git
               :url "https://github.com/Niluge-KiWi/multi-eshell.git"
               :features multi-eshell)
        (:name mustache :type git
               :url "https://github.com/mustache/emacs.git"
               :features mustache-mode)
        (:name nginx-mode :type elpa)
        (:name org :type elpa)
        (:name org-journal :type elpa)
        (:name org-tree-slide :type elpa)
        (:name plantuml-mode :type elpa)
        ;; (:name powerline :type git
        ;;        ;; :url "https://github.com/milkypostman/powerline.git"
        ;;        :url "https://github.com/jonathanchu/emacs-powerline.git"
        ;;        :features powerline)
        (:name py-isort
               :type github
               :pkgname "dakra/py-isort.el"
               :branch "isort-add-remove")
        rainbow-mode
        ruby-block
        ruby-end
        (:name s
               :description "The long lost Emacs string manipulation library."
               :type git
               :url "https://github.com/magnars/s.el")
        (:name smex :type elpa)
        (:name sql-indent :type emacswiki)
        (:name terraform-doc :type elpa)
        (:name terraform-mode :type elpa)
        (:name typescript-mode :type elpa)
        (:name undo-tree  :type git
               :url "http://www.dr-qubit.org/git/undo-tree.git"
               :features undo-tree)
        web-mode
        (:name widen-window :type emacswiki
               :features widen-window)
        (:name writeroom-mode :type elpa)
        (:name wuxch-dired-copy-paste :type emacswiki
               :features wuxch-dired-copy-paste)
        yaml-mode
        ;;yasnippet
        ))

(setq my-packages (mapcar 'el-get-as-symbol (mapcar 'el-get-source-name el-get-sources)))
(el-get 'sync my-packages)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Base
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; start garbage collector less often
(setq gc-cons-threshold (* 20 1024 1024))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Desktop and server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;if we are alone, run server, and load desktop
;;very crude hack
(defvar emacs-is-master nil
  "Is emacs the server?")
(when (string= "1\n"
	       (shell-command-to-string
		"ps x | grep emacs | grep -v grep | grep -v emacs-bin | grep -v emacsclient | wc -l"))
  (setq emacs-is-master t)
  ;;(server-start)
  )

;; desktop
(setq desktop-load-locked-desktop t
	  desktop-path '("~/.emacs.d/")
	  desktop-dirname "~/.emacs.d/"
	  desktop-base-file-name "desktop")
;;(desktop-save-mode 1)
;; save every 10mins
;; (setq desktop-save-timer (run-with-timer 600 600 (lambda () (cl-flet ((message (&rest args) nil))
;;                                                               (desktop-save-in-desktop-dir)))))

;; pipe to emacs
(defun fake-stdin-slurp (current-dir filename)
  "Emulate stdin slurp using emacsclient hack"
  ;;; TODO tail mode on file: better for large pipes
  (switch-to-buffer (generate-new-buffer "*stdin*"))
  (cd current-dir)
  (insert-file filename)
  (end-of-buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Graphical display
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; there is some stuff in customize, but can't move it
;; here for technical reasons

;; fullscreen
(defun toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
			 (if (equal 'fullboth current-value)
			     (if (boundp 'old-fullscreen) old-fullscreen nil)
			   (progn (setq old-fullscreen current-value)
				  'fullboth)))))
;; ;; one emacs to rule them all and in fullscreen bind them
;; (when emacs-is-master
;;   (toggle-fullscreen))

;; new frames are maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(frame-resize-pixelwise . t))

;; (defadvice raise-frame (after make-it-work (&optional frame) activate)
;;     "Work around some bug? in raise-frame/Emacs/GTK/Metacity/something.
;;      Katsumi Yamaoka posted this in
;;      http://article.gmane.org/gmane.emacs.devel:39702"
;;     (call-process
;;      "wmctrl" nil nil nil "-i" "-R"
;;      (frame-parameter (or frame (selected-frame)) 'outer-window-id)))
;;(add-hook 'server-switch-hook 'raise-frame)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Colour theme and fonts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; add colors to zenburn
(setq zenburn-override-colors-alist
      '(("zenburn-green-yellow-2" . "#89a610")
        ("zenburn-green-yellow-1" . "#99b32f")
        ("zenburn-green-yellow"   . "#afd21c")
        ("zenburn-green-yellow+1" . "#d1ed5d")
        ("zenburn-green-yellow+2" . "#daed8d")

        ("zenburn-yellow-green-2" . "#a3ab10")
        ("zenburn-yellow-green-1" . "#b1b831")
        ("zenburn-yellow-green"   . "#cfd81d")
        ("zenburn-yellow-green+1" . "#e8ef5e")
        ("zenburn-yellow-green+2" . "#eaef8f")

        ("zenburn-dark-blue-2"    . "#1f3076")
        ("zenburn-dark-blue-1"    . "#293c87")
        ("zenburn-dark-blue"      . "#354897")
        ("zenburn-dark-blue+1"    . "#4457a4")
        ("zenburn-dark-blue+2"    . "#5666ab")

        ("zenburn-purple-2"       . "#612e7e")
        ("zenburn-purple-1"       . "#6d368a")
        ("zenburn-purple"         . "#784097")
        ("zenburn-purple+1"       . "#814c9e")
        ("zenburn-purple+2"       . "#8959a4")
        ("zenburn-purple+3"       . "#8f64a8")
        ("zenburn-purple+4"       . "#9772ad")))
;; load-only
(load-theme 'zenburn t nil)
;; customize
(zenburn-with-color-variables
  (custom-theme-set-faces
   'zenburn
;;;;; basic coloring
   `(cursor ((t (:background ,zenburn-fg :foreground ,zenburn-bg))))
;;;;; ioccur
   `(ioccur-overlay-face ((t (:background ,zenburn-bg+1))))
   `(ioccur-match-overlay-face ((t (:background ,zenburn-bg+1))))
   `(ioccur-title-face ((t (:background ,zenburn-bg+1))))
   `(ioccur-regexp-face ((t (:background "#506070" :underline t))))
   `(ioccur-match-face ((t (:background "#506070"))))
;;;;; show-paren (less aggressive highlight for show-paren-style 'expression)
   `(show-paren-mismatch ((t (:foreground ,zenburn-red+1 :background ,zenburn-bg+3))))
   `(show-paren-match ((t (:background ,zenburn-bg-1))))
;;;;; org-mode levels
   `(org-level-7 ((t (:foreground ,zenburn-red-2))))
   `(org-level-8 ((t (:foreground ,zenburn-blue-3))))
   ))
;; finally, enable zenburn theme
(enable-theme 'zenburn)

(setq font-use-system-font t) ;; since emacs 23.2

;; powerline
;; (require 'powerline)
;; (powerline-default-theme)
;; (setq powerline-arrow-shape 'arrow14)
;; (custom-set-faces
;;  '(mode-line ((t (:foreground "#030303" :background "#bdbdbd" :box nil))))
;;  '(mode-line-inactive ((t (:foreground "#f9f9f9" :background "#666666" :box nil)))))
;; (powerline-revert)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mouse
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(setq mouse-yank-at-point t)
(setq mouse-highlight t)

;; TODO better fix
(defun mouse-avoidance-point-position ()
  "Return the position of point as (FRAME X . Y).
Analogous to `mouse-position'."
  (let* ((inhibit-point-motion-hooks t)
         (edges (window-inside-edges))
	(x-y (posn-x-y (posn-at-point))))
    (cons (selected-frame)
	  (cons (+ (car edges)
		   (/ (car x-y) (frame-char-width)))
		(+ (car (cdr edges))
		   (/ (cdr x-y) (frame-char-height)))))))
;; control mouse clipboard. In particular, select-active-regions, activated in 23.2, sucks.
;; window selection is put in the X primary selection
(setq x-select-enable-primary t)
;; and not in X clipboard
(setq x-select-enable-clipboard nil)
(setq x-selection-timeout 10)
(setq mouse-drag-copy-region t)
(defun toggle-clipboard-selection ()
  "Toggle clipboard/primary selection"
  (interactive)
  (if x-select-enable-clipboard
      (progn
        (setq x-select-enable-clipboard nil
              x-select-enable-primary t)
        (message "Primary selection"))
    (setq x-select-enable-clipboard t
          x-select-enable-primary nil)
    (message "Clipboard")))

;; disable moving mouse when changing frame focus (desktop shortcut for emacsclient)
(setq focus-follows-mouse nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General-purpose functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun toggle-variable (symb)
  (set symb (not (eval symb))))

(defun current-mm ()
  (buffer-local-value 'major-mode (current-buffer)))

(defun current-buffer-not-mini ()
  "Return current-buffer if current buffer is not the *mini-buffer*
  else return buffer before minibuf is activated."
  (if (not (window-minibuffer-p)) (current-buffer)
    (if (eq (get-lru-window) (next-window))
	(window-buffer (previous-window)) (window-buffer (next-window)))))

(defun launch-command (command filename)
  "Launches command with argument filename, discarding all output"
  (let ((process-connection-type nil))
	(start-process-shell-command command nil (concat "nohup " (shell-quote-argument command) " " (shell-quote-argument filename) " &"))))

(defun gnome-open-file (filename)
  "gnome-opens the specified file."
  (interactive "fFile to open: ")
  (launch-command  "/usr/bin/gnome-open" filename))

(defun basename-cons(f)
  (cons (file-name-nondirectory f) f))

(defun sudo-edit (&optional arg)
  "Edit a file as root"
  (interactive "p")
  (if arg
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun toggle-alternate-file-as-root (&optional filename)
  "Toggle between the current file as the default user and as root.
From http://atomized.org/2011/01/toggle-between-root-non-root-in-emacs-with-tramp/"
  (interactive)
  (let* ((filename (or filename (buffer-file-name)))
         (parsed (when (tramp-tramp-file-p filename)
                   (coerce (tramp-dissect-file-name filename)
                           'list)))
		 (old-pnt (point)))
    (unless filename
      (error "No file in this buffer."))

	(unwind-protect
		(find-alternate-file
		 (if (equal '("sudo" "root") (butlast parsed 2))
			 ;; As non-root
			 (if (or
				  (string= "localhost" (nth 2 parsed))
				  (string= (system-name) (nth 2 parsed)))
				 (nth 3 parsed)
			   (apply 'tramp-make-tramp-file-name
					  (append (list tramp-default-method nil) (cddr parsed))))

		   ;; As root
		   (if parsed
			   (apply 'tramp-make-tramp-file-name
					  (append '("sudo" "root") (cddr parsed)))
			 (tramp-make-tramp-file-name "sudo" "root" "localhost" filename))))
	  (goto-char old-pnt))))
(global-set-key (kbd "C-c C-r") 'toggle-alternate-file-as-root)

;; copy-filename
(defun copy-filename()
  "Copy filename to both kill ring and clipboard"
  (interactive)
  (let* ((filename (file-name-nondirectory (buffer-file-name)))
         (line-number (line-number-at-pos))
         (text (format "%s:%d" filename line-number)))
    (x-select-text text)
    (kill-new text)
    (message "Copied %s" text)))

(defun reload-emacs () (interactive) (load-file "~/.emacs"))
(defun edit-emacs () (interactive) (find-file "~/.emacs"))

(defun indent-whole-buffer ()
  "Indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil))

(defun kill-whitespace ()
  "Kill the whitespace between two non-whitespace characters"
  (interactive "*")
  (save-excursion
    (save-restriction
      (save-match-data
	(progn
	  (re-search-backward "[^ \t\r\n]" nil t)
	  (re-search-forward "[ \t\r\n]+" nil t)
	  (replace-match "" nil nil))))))

(defun truncate-list (list n)
  "Truncate LIST to at most N elements destructively."
  (when n
	(let ((here (nthcdr (1- n) list)))
	  (when (consp here)
		(setcdr here nil))))
  list)

(defun eval-and-replace ()
  "Evaluate the sexp at point and replace it with its value

Taken from http://nflath.com/2009/08/easier-emacs/ by N Flath."
    (interactive)
    (backward-kill-sexp)
    (condition-case nil
        (prin1 (eval (read (current-kill 0)))
               (current-buffer))
      (error (message "Invalid expression")
             (insert (current-kill 0)))))
(global-set-key (kbd "C-c C-e") 'eval-and-replace)

;; from http://www.emacswiki.org/emacs/AlignCommands#toc7
(defun align-repeat (start end regexp)
  "Repeat alignment (for all columns) with respect to the given regular expression."
  (interactive "r\nsAlign regexp: ")
  (align-regexp start end
                (concat "\\(\\s-*\\)" regexp) 1 1 t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Misc. settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; instead of / or whatever
(setq default-directory (expand-file-name "~/"))
;; ;; OH MY GOD IT'S A SECURITY VULNERABILITY, WE ARE ALL GONNA DIE
;; (setq enable-local-variables :all)
;;automatic indent
(global-set-key (kbd "RET") 'newline-and-indent)
;;no transient mark
(transient-mark-mode -1)

;;blinking cursor is distracting and useless
(blink-cursor-mode -1)

;;display buffer name in title bar
(setq my-title '("%b" (:eval (if (buffer-file-name)
                                 `(" - "
                                   ,(file-name-directory
									 (abbreviate-file-name (buffer-file-name))))))
				 " - " invocation-name "@" system-name))
(setq frame-title-format my-title)
(setq icon-title-format my-title)

;; stop creating #autosave# files
(setq auto-save-default nil)

;;please add a final newline each time I save a buffer
(setq require-final-newline 't)

;; hl current line everywhere
(global-hl-line-mode t)

;; tramp
(setq tramp-auto-save-directory "~/.emacs.d/tramp-autosave")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Occur
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; occur: force unique name
(add-hook 'occur-hook
          #'(lambda ()
              (occur-rename-buffer t)))

;;---- ioccur
(require 'ioccur)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hide lines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'hide-lines "hide-lines" "Hide lines based on a regexp" t)
(global-set-key (kbd "C-c h") 'hide-lines)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Scrolling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;scroll one line at a time
(setq scroll-conservatively 10000)
;;keep cursor at current position when scrolling
(setq scroll-preserve-screen-position t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Word wrapping
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pour gérer les lignes trop longues
;; amazing new variable in e23. No need to worry about longlines any more
(setq-default word-wrap t)
;; ... but still use ll sometimes for reading dense text
(defalias 'll 'longlines-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Auto revert
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; automatically update buffers when changed
(global-auto-revert-mode t)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-interval 30) ;30s is enough
(setq auto-revert-verbose nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interactively Do Things
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;-------ido
;;makes C-x C-f and C-x b a lot easier
(require 'ido)
(setq ido-create-new-buffer 'always
      ido-enable-flex-matching t
      ;;ido-max-prospects 12
      ido-max-window-height 1
      ido-save-directory-list-file "~/.emacs.d/ido.last")
(ido-mode 'both) ;; for buffers and files
(ido-everywhere 1)
;; open the buffers and files in the selected-window, like switch-to-buffer
(setq ido-default-file-method 'selected-window)
(setq ido-default-buffer-method 'selected-window)
;; ido-ubiquitous
(ido-ubiquitous-mode t)

;;-------icomplete
;; completion for commands that don't use ido (like help)
(icomplete-mode 1)
;; TODO test icicles

;;-------smex
;; super M-x : ido + frequency
(require 'smex)
(setq smex-history-length 7
      smex-save-file "~/.emacs.d/smex-items")
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "<menu>") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
;;(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
;; save every hour
(setq smex-save-to-file-timer (run-with-timer 3600 3600 'smex-save-to-file))

;;-------flx
(require 'flx-ido)
(flx-ido-mode t)
;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)
(setq flx-ido-threshhold 1000)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Uniquify
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; uniquify buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;; plus some useful functions
;; TODO: support `/foo`, currently it fails with:
;; Debugger entered--Lisp error: (wrong-type-argument stringp nil)
;; find-file-name-handler(nil file-remote-p)
;; file-remote-p(nil)
;; uniquify-get-proposed-name("Dockerfile" "" 1)
(defun uniquify-get-filename (filename depth)
  "Get 'uniquified' filename, given a filename and a prefix depth."
  (let ((dir (file-name-directory filename))
		(file (file-name-nondirectory filename)))
	;; remove trailing slash
	(if (string-match "/$" dir)
		(setq dir (substring dir 0 -1)))
	(uniquify-get-proposed-name file dir depth)))

(defun uniquify-filename-list (file-list &optional depth)
  "Uniquify a list of filenames by returning an alist of filename and uniquified filenames.
Optional depth is for internal use."
  (unless depth
	(setq depth 0))
  (let ((conflicting-list ())
		(final-uniq-file-alist ())
		(uniq-file-alist (mapcar
						  (lambda (file)
							`(,file . ,(uniquify-get-filename file depth)))
						  file-list))
		uniq-file-alist2
		item
		item2
		conflict)
	(while uniq-file-alist
	  (setq item (car uniq-file-alist)
			uniq-file-alist (cdr uniq-file-alist)
			conflict nil
			uniq-file-alist2 uniq-file-alist)
	  ;; Search for and remove all conflicts from remaining list
	  (while uniq-file-alist2
		(setq item2 (car uniq-file-alist2)
			  uniq-file-alist2 (cdr uniq-file-alist2))
		(when (string= (cdr item) (cdr item2))
		  ;; Found conflict
		  (setq conflict t)
		  (push (car item2) conflicting-list)
		  (setq uniq-file-alist (delq item2 uniq-file-alist))
		  (setq uniq-file-alist2 (delq item2 uniq-file-alist2))))
	  (if conflict
		  (push (car item) conflicting-list)
		(push item final-uniq-file-alist)))
	;; now recurse with colliding files
	(if conflicting-list
		(setq final-uniq-file-alist
			  (append
			   final-uniq-file-alist
			   (uniquify-filename-list conflicting-list (+ 1 depth)))))
	final-uniq-file-alist))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Undo-tree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq undo-tree-mode-lighter "")
(global-undo-tree-mode)
(set-default 'undo-tree-visualizer-timestamps t)
(add-hook 'erc-mode-hook #'(lambda () (setq undo-tree-dont-activate t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Highligh symbol
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'highlight-symbol)

(zenburn-with-color-variables
  (setq highlight-symbol-colors
        `(,zenburn-cyan
          ,zenburn-green+4
          ,zenburn-magenta
          ,zenburn-yellow
          ,zenburn-red-2
          ,zenburn-blue-3
          ,zenburn-orange
          ,zenburn-purple+4
          ,zenburn-yellow-green+2)))

;; azerty
(global-set-key (kbd "C-&") 'highlight-symbol-at-point)
(global-set-key (kbd "C-é") 'highlight-symbol-next)
(global-set-key (kbd "C-\"") 'highlight-symbol-prev)
(global-set-key (kbd "C-à") 'highlight-symbol-remove-all)

;; qwerty
(global-set-key (kbd "C-1") 'highlight-symbol-at-point)
(global-set-key (kbd "C-2") 'highlight-symbol-next)
(global-set-key (kbd "C-3") 'highlight-symbol-prev)
(global-set-key (kbd "C-0") 'highlight-symbol-remove-all)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Parenthesis editing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;visual paren matching
(show-paren-mode t)
(setq show-paren-style 'expression)
(setq show-paren-delay 0)

;;rainbow parentheses highlighting ! \o/
(require 'highlight-parentheses)
(zenburn-with-color-variables
  (setq hl-paren-colors
	(list zenburn-red-4 zenburn-orange zenburn-yellow-green+1 zenburn-green zenburn-blue zenburn-dark-blue+2 zenburn-purple+2 nil)) ;; a final fake color, because the last one seems to be ignored
  (setq hl-paren-background-colors
	(make-list (length hl-paren-colors) zenburn-bg-1)))

;;highlight-parentheses is a buffer-local minor mode : create a global
;;minor mode of our own
(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)

; from http://emacswiki.org/emacs/ShowParenMode#toc1
(defadvice show-paren-function
    (after show-matching-paren-offscreen activate)
  "If the matching paren is offscreen, show the matching line in the
        echo area. Has no effect if the character before point is not of
        the syntax class ')'."
  (interactive)
  (let* ((cb (char-before (point)))
         (matching-text (and cb
                             (char-equal (char-syntax cb) ?\) )
                             (blink-matching-open))))
    (when matching-text (message matching-text))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dired
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc. dired add-ons
(require 'dired-x)

;; omit hidden files + default
(setq-default dired-omit-files-p t) ; Buffer-local variable
(setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))

;;clean dired default view : don't display groups, use human-readable sizes
(setq dired-listing-switches "-alhG"
      dired-free-space-args "-Pkm" ;; TODO k and m for what?
      dired-auto-revert-buffer t)

;;add xdg-open as C-ret
(defun dired-xdg-open-file ()
  "Opens the current file from a Dired buffer."
  (interactive)
  (launch-command "xdg-open" (dired-get-file-for-visit)))
(define-key dired-mode-map (kbd "<C-return>") 'dired-xdg-open-file)
;; LANG=Fr breaks the regexp that matches file sizes (, instead of . separator)
(add-hook 'dired-mode-hook '(lambda () (setenv "LC_NUMERIC" "C")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; VC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; deactivate
(setq vc-handled-backends nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Magit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'magit)
(require 'forge)

(setq forge-pull-notifications nil)


;;TODO REMOVE this line

;; hide untracked section by default
(push (cons [untracked status] 'hide) magit-section-initial-visibility-alist)
;; no buffer saving when magit-status
(setq magit-save-repository-buffers nil)
;; use ido in prompts
(setq magit-completing-read-function 'magit-ido-completing-read)
;; show process buffer for long operations
(setq magit-process-popup-time 5)
;; magit-status: switch to buffer instead of pop to buffer
(setq magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
;; intra line diff highlight
(setq magit-diff-refine-hunk t)
;; "u" and "U" are already taken by unstage, set "o" in addition to uneasy "^" on azerty keyboards
(define-key magit-mode-map (kbd "o") 'magit-section-up)

(global-set-key (kbd "C-c s") 'magit-status)
(global-set-key (kbd "C-c C-s") 'magit-status)

;; flyspell on log
(add-hook 'git-commit-setup-hook '(lambda () (flyspell-lang "american")))

;; https://github.com/magit/magit/issues/2012#issuecomment-619366605
(with-eval-after-load 'magit
  (transient-append-suffix 'magit-log "-A"
    '("-1" "First parent" "--first-parent")))

;; ignore whitespace
;; TODO fix ignore whitespace
;; (defun magit-toggle-whitespace ()
;;   (interactive)
;;   (if (member "-w" (magit-diff-refresh-arguments))
;;       (magit-dont-ignore-whitespace)
;;     (magit-ignore-whitespace)))

;; (defun magit-ignore-whitespace ()
;;   (interactive)
;;   (message "Ignore whitespace diffs")
;;   (let ((args (magit-diff-refresh-arguments)))
;;     (add-to-list args "-w")
;;     (add-to-list args "--ignore-blank-lines")
;;     (magit-diff-refresh args)))


;; (defun magit-dont-ignore-whitespace ()
;;   (interactive)
;;   (message "Display whitespace diffs")
;;   (magit-diff-refresh (remove "-w" (remove "--ignore-blank-lines" (magit-diff-refresh-arguments)))))


;;;(define-key magit-mode-map (kbd "W") 'magit-toggle-whitespace)


;; magit-git-wip
;; https://github.com/bartman/git-wip
;; see magit-wip.el to enable on per repo basis:
;;   git config --add magit.extension wip-save
(require 'magit-wip)
(magit-wip-after-save-mode 1)
(magit-wip-after-apply-mode 1)
(magit-wip-before-change-mode 1)

;; magit-blame
(require 'magit-blame)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Window management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;move between windows with meta-keypad
(windmove-default-keybindings 'meta)

;; TODO test this
;; widen window : widen selected window
;; (global-widen-window-mode t)
;; ;; et on ajoute les fonctions utilisées par windmove, comme ça on a tout ce qu'il faut!
;; (add-to-list 'ww-advised-functions 'windmove-up)
;; (add-to-list 'ww-advised-functions 'windmove-down)
;; (add-to-list 'ww-advised-functions 'windmove-right)
;; (add-to-list 'ww-advised-functions 'windmove-left)


;; resize windows
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)


;; Cancel and redo windows configurations
;;C-c left/right to undo/redo changes in window configuration
(require 'winner)
(setq winner-dont-bind-my-keys t) ;; default bindings conflict with org-mode
(winner-mode t) ;; turn on the global minor mode

;; buffer move
(require 'buffer-move)
(global-set-key (kbd "S-M-<left>")  'buf-move-left)
(global-set-key (kbd "S-M-<right>") 'buf-move-right)
(global-set-key (kbd "S-M-<up>")    'buf-move-up)
(global-set-key (kbd "S-M-<down>")  'buf-move-down)

;; ace-window
(global-set-key (kbd "M-o") 'ace-window)

;; ace-jump
(require 'ace-jump-mode)
(setq ace-jump-mode-scope 'frame)
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)
(global-set-key (kbd "C-c C-SPC") 'ace-jump-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Session Management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;save the minibuffer input
(savehist-mode 1)

;;save last edit place in files
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/places")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Recent files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;recent files, interaction with ido
(require 'recentf)

(defcustom recentf-ido-max-items 200
  "Maximum number of items of the recent list selection with ido
(recentf-ido-find-file-or-maybe-list).
If nil, do not limit."
  :group 'recentf)

(defun recentf-ido-find-file-or-maybe-list (&optional arg)
  "Find a recent file using Ido and uniquify,
or list all recent files if prefixed"
  (interactive "P")
  (if arg
	  (recentf-open-files)
	(let* ((file-list (truncate-list
					   (copy-list recentf-list)
					   recentf-ido-max-items))
		   (uniq-file-alist (uniquify-filename-list file-list))
		   ;; ask user
		   (file (ido-completing-read
				  (format "%s: " recentf-menu-title)
				  (mapcar (lambda (filename)
							(cdr (assoc filename uniq-file-alist)))
						  file-list)
				  nil t)))
	  ;; now find full filename back
	  (when file
		(find-file (car (rassoc file uniq-file-alist)))))))

(setq recentf-max-saved-items nil
      recentf-save-file "~/.emacs.d/recentf"
      recentf-ido-max-items 300)
(recentf-mode 1)
(global-set-key (kbd "C-x C-r") 'recentf-ido-find-file-or-maybe-list)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Imenu: jump between indexes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'imenu)
    (defun ido-goto-symbol (&optional symbol-list)
      "Refresh imenu and jump to a place in the buffer using Ido."
      (interactive)
      (unless (featurep 'imenu)
        (require 'imenu nil t))
      (cond
       ((not symbol-list)
        (let ((ido-mode ido-mode)
              (ido-enable-flex-matching
               (if (boundp 'ido-enable-flex-matching)
                   ido-enable-flex-matching t))
              name-and-pos symbol-names position)
          (unless ido-mode
            (ido-mode 1)
            (setq ido-enable-flex-matching t))
          (while (progn
                   (imenu--cleanup)
                   (setq imenu--index-alist nil)
                   (ido-goto-symbol (imenu--make-index-alist))
                   (setq selected-symbol
                         (ido-completing-read "Symbol? " symbol-names))
                   (string= (car imenu--rescan-item) selected-symbol)))
          (unless (and (boundp 'mark-active) mark-active)
            (push-mark nil t nil))
          (setq position (cdr (assoc selected-symbol name-and-pos)))
          (cond
           ((overlayp position)
            (goto-char (overlay-start position)))
           (t
            (goto-char position)))))
       ((listp symbol-list)
        (dolist (symbol symbol-list)
          (let (name position)
            (cond
             ((and (listp symbol) (imenu--subalist-p symbol))
              (ido-goto-symbol symbol))

             ((listp symbol)
              (setq name (car symbol))
              (setq position (cdr symbol)))

             ((stringp symbol)
              (setq name symbol)
              (setq position
                    (get-text-property 1 'org-imenu-marker symbol))))
            (unless (or (null position) (null name)
                        (string= (car imenu--rescan-item) name))
              (add-to-list 'symbol-names name)
              (add-to-list 'name-and-pos (cons name position))))))))

(global-set-key (kbd "C-x C-i") 'ido-goto-symbol)
;; TODO test this


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ibuffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dired for buffers
(require 'ibuffer)
(setq ibuffer-default-sorting-mode 'recency)

(setq ibuffer-always-show-last-buffer t)
(setq ibuffer-view-ibuffer t)

;; TODO make this work: it does not...
;;(add-to-list 'ibuffer-never-show-regexps "^\\*Minibuf-1\\*$")
;; for emacs <= 23
(require 'ibuf-ext)
(add-to-list 'ibuffer-never-show-predicates "^\\*Minibuf-1\\*$")

(global-set-key (kbd "C-x C-b") 'ibuffer)
;; TODO import al-ibuffer.el


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Programming modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;ignore case when matching a suffix (such as .F90)
(setq auto-mode-case-fold t)
;;tags
(setq tags-table-list '("~/.emacs.d")
      tags-revert-without-query t)
;;indent yanked code in programming languages modes
(load-library "yank-indent")
(add-to-list 'yank-indent-modes 'python-mode)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; RTags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://github.com/Andersbakken/rtags
;; (add-to-list 'load-path (expand-file-name "~/tmp/rtags/rtags/src"))
;; ;; (setenv "PATH"
;; ;;         (concat
;; ;;          (expand-file-name "~/tmp/rtags/bin") ";"
;; ;;          (getenv "PATH")))
;; (require 'rtags)
;; (setq rtags-path (expand-file-name "~/tmp/rtags")) ;; rtags appends /bin to this path
;; ;; prefix: C-x r
;; (rtags-enable-standard-keybindings c-mode-base-map)
;; ;;(setq rtags-rc-log-enabled nil)
;; (define-key c-mode-base-map (kbd "C-j") (function rtags-find-symbol-at-point))
;; (define-key c-mode-base-map (kbd "M-j") (function rtags-location-stack-back))
;; (define-key c-mode-base-map (kbd "C-M-j") (function rtags-location-stack-forward))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ag, the silver surfer, faster ack
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ag)
;; (setq ag-highlight-search t) ;; doesn't work on emacs 23:
;;;; error in process filter: run-hooks: Symbol's value as variable is void: compilation-filter-start
;;;; error in process filter: Symbol's value as variable is void: compilation-filter-start
(add-to-list 'ag-arguments "--word-regexp")
(add-to-list 'ag-arguments "--ignore=*~")
(add-to-list 'ag-arguments "--ignore=*/*~")
(add-to-list 'ag-arguments "--ignore-dir=node_modules")
(defun vc-svn-root (arg)
  nil)
(defun vc-hg-root (arg)
  nil)
;; enable follow minor mode by default
(defun my-ag-config ()
  (next-error-follow-minor-mode t))
(add-hook 'ag-mode-hook 'my-ag-config)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CMake
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq auto-mode-alist
      (append '(("CMakeLists\\.txt\\'" . cmake-mode)
                ("\\.cmake\\'" . cmake-mode)
                ("CMakeLists-install.txt" . cmake-mode))
              auto-mode-alist))

;; better font-lock
(autoload 'cmake-font-lock-activate "cmake-font-lock" nil t)
(add-hook 'cmake-mode-hook 'cmake-font-lock-activate)

;; add underscore to work split syntax table, like it's everywhere else
(defun my-cmake-fix-underscore ()
  (modify-syntax-entry ?_  "_" cmake-mode-syntax-table))
(add-hook 'cmake-mode-hook 'my-cmake-fix-underscore)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; C
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'cc-mode)
;;linux style
(setq c-default-style "linux")
(setq-default c-basic-offset 2)
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;;'electric' indentation : indent on newline
(define-key c-mode-base-map "\C-m"
  'c-context-line-break)
(define-key c-mode-base-map (kbd "M-q") nil)

;; subword mode
(add-hook 'c-mode-common-hook
          (lambda () (c-subword-mode 1)))

;; auto-newline mode
;; (add-hook 'c-mode-common-hook
;;           (lambda () (c-toggle-auto-newline 1)))

;; hide-show
(defun my-hide-show ()
  (local-set-key (kbd "C-c C-h C-s") 'hs-show-block)
  (local-set-key (kbd "C-c C-h C-h") 'hs-hide-block)
  (local-set-key (kbd "C-c C-h C-a") 'hs-show-all)
  (hs-minor-mode t))
(add-hook 'c-mode-common-hook 'my-hide-show)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; C++
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.ipp$" . c++-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ruby
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Cheffile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Berksfile" . ruby-mode))

(require 'ruby-block)
(require 'ruby-end)

(defun my-ruby-mode-config ()
   (ruby-block-mode t))
(add-hook 'ruby-mode-hook 'my-ruby-mode-config)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Python
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'python)
(defadvice run-python (after run-python-revert-patch)
  "revert patch which removes '' from sys.path"
  (python-shell-internal-send-string "import sys
sys.path.insert(0, '')"))
(ad-activate 'run-python)

;; elpy
(elpy-enable)
(setq elpy-rpc-python-command "python3")
(setq python-shell-interpreter "python3")
(setq elpy-use-ipython "python3")
;;(add-hook 'elpy-mode-hook (lambda () (elpy-shell-set-local-shell (elpy-project-root))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Javascript
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))

(add-hook 'js2-mode-hook
          (lambda ()
            (setq js2-enter-indents-newline t)
            (modify-syntax-entry ?` "\"" js2-mode-syntax-table)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CSS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.less$" . css-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Jade
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'jade-mode)
(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))

(add-to-list 'auto-mode-alist '("\\.less\\'" . less-css-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; php
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; lua
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))
(setq lua-indent-level 2)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; golang
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'before-save-hook #'gofmt-before-save)
(add-hook 'go-mode-hook (lambda ()
                          (set (make-local-variable 'company-backends) '(company-go))
                          (company-mode)))
(add-hook 'before-save-hook 'gofmt-before-save)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Helm charts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("_helpers\\.tpl" . web-mode))
(add-to-list 'auto-mode-alist '("NOTES\\.txt" . web-mode))
(setq web-mode-engines-alist
      '(("go"    . "_helpers\\.tpl")
        ("go"    . "NOTES\\.txt")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Apache
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load (expand-file-name "~/.emacs.d/el-get/apache-mode/apache-mode.el"))
(autoload 'apache-mode "apache-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.htaccess\\'"   . apache-mode))
(add-to-list 'auto-mode-alist '("httpd\\.conf\\'"  . apache-mode))
(add-to-list 'auto-mode-alist '("srm\\.conf\\'"    . apache-mode))
(add-to-list 'auto-mode-alist '("access\\.conf\\'" . apache-mode))
(add-to-list 'auto-mode-alist '("sites-\\(available\\|enabled\\)/" . apache-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SQL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; enable backslash as escape character
(add-hook 'sql-mode-hook

          (lambda ()
	    (modify-syntax-entry ?\\ "." sql-mode-syntax-table)))
;; sql-indent
(eval-after-load "sql"
  '(load-library "sql-indent"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Conf mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; systemd unit files
(add-to-list 'auto-mode-alist '("\\.service$" . conf-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Latex
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO manage auctex?
;; (condition-case err
;;     (progn (load "auctex.el" nil t t)
;; 	   (load "preview-latex.el" nil t t))
;;   (error
;;    (message "Failed to load auctex")))
;;don't ask to cache preamble
(setq preview-auto-cache-preamble t)
;;indent when pressing RET
(setq TeX-newline-function 'newline-and-indent
      LaTeX-math-abbrev-prefix (kbd "ù"))
;;always preview using gnome-open
(setq TeX-output-view-style
      '(("pdf" "." "gnome-open %o")
	("dvi" "." "dvipdf %o && gnome-open $(basename %o dvi)pdf")))
(defun my-tex-config ()
  (turn-on-reftex)
  (auto-fill-mode 1)
  (flyspell-mode 1)
  (flyspell-buffer)
  (TeX-PDF-mode 1)
  (LaTeX-math-mode 1)
  (local-set-key (kbd "C-c C-d") 'TeX-insert-braces)
  (local-set-key (kbd "C-c l") 'reftex-label)
  (local-set-key (kbd "C-c r") 'reftex-reference)
  (local-set-key (kbd "C-c b") 'reftex-citation)
  ;; undo TeX remaps, otherwise it interferes with compilation
  (define-key TeX-mode-map [remap next-error] nil)
  (define-key TeX-mode-map [remap previous-error] nil)

  ;; If the file contains local variables defining TeX-master, respect that.
  ;; Otherwise, look for a master file in the current directory
  ;; Define a local variable by
  ;; %%% Local Variables:
  ;; %%% TeX-master: "something"
  ;; %%% End:

  ;; list of master files to look for, increasing order of priority
  (setq list-of-master-files '("report" "master" "main"))
  ;; OK, this is a hack, but we force parsing of the file local variables here
  (hack-local-variables)
  (unless (stringp TeX-master)
    (dolist (name list-of-master-files)
      (when (file-exists-p (concat name ".tex"))
  	(setq TeX-master name))))

  ;; setup compilation, based on TeX-master
  ;; needs raise_process, which raises (using wmctrl) a process whose invocation
  ;; line matches the argument
  ;; (let ((master (if (stringp TeX-master)
  ;; 		    TeX-master
  ;; 		  (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))))
  ;;   (set (make-local-variable 'compile-command)
  ;; 	 (format
  ;; 	  "rubber -d %s && (raise_process.sh %s.pdf || nohup gnome-open %s.pdf > /dev/null)"
  ;; 	  master master master)))
  )
(add-hook 'LaTeX-mode-hook 'my-tex-config)

;; (defun my-bibtex-compilation-setup ()
;;   (set (make-local-variable 'compile-command)
;;        (format
;; 	"rubber -d main && (raise_process main.pdf || nohup gnome-open main.pdf > /dev/null)")))
;; (add-hook 'bibtex-mode-hook 'my-bibtex-compilation-setup 'attheend)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Outline
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq outline-minor-mode-prefix (kbd "s-o"))
;; TODO test
(defun my-outline-hook ()
  (define-key outline-mode-prefix-map [(control ?<)] nil)
  (define-key outline-mode-prefix-map [(control ?>)] nil)
  (local-set-key "\C-c\C-c" outline-mode-prefix-map))
(add-hook 'outline-minor-mode-hook 'my-outline-hook)

(add-hook 'LaTeX-mode-hook 'outline-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
(defun setup-outline-lisp ()
  "Only outline on ;;;, thank you."
  (setq outline-regexp ";;; "))
(add-hook 'emacs-lisp-mode-hook 'setup-outline-lisp)

;; fold-dwim
(setq fold-dwim-outline-style 'nested)
;; Have two toggles, one for the header we're in, and one general
(global-set-key (kbd "<f6>")  'fold-dwim-toggle)
(global-set-key (kbd "<f7>")  'fold-dwim-toggle-all)
;; This is suboptimal, not buffer-local, etc. I don't care.
(setq fold-dwim-general-toggle nil)
(defun fold-dwim-toggle-all ()
  (interactive)
  (if fold-dwim-general-toggle
      (fold-dwim-show-all)
    (fold-dwim-hide-all))
  (toggle-variable 'fold-dwim-general-toggle))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Markdown
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PKGBUILD
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'auto-mode-alist '("/PKGBUILD$" . pkgbuild-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Bash
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Temporary files when editing commandline from bash in emacs (C-x C-e in bash)
;  are bash scripts
(add-to-list 'auto-mode-alist '("/bash-fc-[0-9]*$" . sh-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Shell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq comint-scroll-to-bottom-on-input 'all)
(setq comint-move-point-for-output t)

(ansi-color-for-comint-mode-on)

;; TODO clean
(add-hook 'shell-mode-hook 'n-shell-mode-hook)
(defun n-shell-mode-hook ()
  "12Jan2002 - sailor, shell mode customizations."
  ;; (local-set-key '[up] 'comint-previous-input)
  ;; (local-set-key '[down] 'comint-next-input)
  ;; (local-set-key '[(shift tab)] 'comint-next-matching-input-from-input)
  (setq comint-input-sender 'n-shell-simple-send)
  )

(defun n-shell-simple-send (proc command)
  "17Jan02 - sailor. Various commands pre-processing before sending to shell."
  (cond
   ;; Checking for clear command and execute it.
   ((string-match "^[ \t]*clear[ \t]*$" command)
    (comint-send-string proc "\n")
    (erase-buffer)
    )
   ;; Checking for man command and execute it.
   ((string-match "^[ \t]*man[ \t]*" command)
    (comint-send-string proc "\n")
    (setq command (replace-regexp-in-string "^[ \t]*man[ \t]*" "" command))
    (setq command (replace-regexp-in-string "[ \t]+$" "" command))
    ;;(message (format "command %s command" command))
    (funcall 'man command)
    )
   ;; TODO add check for ec, less : open file in emacs
   ;; Send other commands to the default handler.
   (t (comint-simple-send proc command))
   )
  )


;;dirtrack
(setq dirtrack-list '("^\\([^@]*\\)@\\([^:]*\\):\\([^$]*\\)" 3))
(add-hook 'shell-mode-hook 'dirtrack-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Eshell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq multi-eshell-shell-function '(eshell))
(setq multi-eshell-name "*eshell*")
(global-set-key (kbd "M-<f1>") 'multi-eshell)
(setq eshell-aliases-file "~/.emacs.d/eshell/alias")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'org)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; (setq org-startup-indented t)
;; (require 'org-remember)
;; (org-remember-insinuate)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
;;(global-set-key (kbd "C-c r") 'remember)
(defun doc ()
  "Open doc.org"
  (interactive)
  (find-file "~/doc/doc.org"))
(defun notes ()
  "Open notes.org"
  (interactive)
  (find-file "~/doc/notes.org"))

(defun org-toggle-todo-with-timestamp-and-fold ()
  (interactive)
  (save-excursion
    (org-back-to-heading t) ;; Make sure command works even if point is
    ;; below target heading
    (let ((org-log-done (if org-log-done 'time nil)))
      (cond ((looking-at "\*+ TODO")
             (org-todo "DONE")
             (hide-subtree))
            ((looking-at "\*+ DONE")
             (org-todo "TODO")
             (hide-subtree))
            (t (message "Can only toggle between TODO and DONE."))))))

;; copied function to change format: we want 1 digit after dot, not 0.
(require 'org-colview) ;; require first, then overwrite
(defun org-columns--summary-estimate (estimates _)
  "Combine a list of estimates, using mean and variance.
The mean and variance of the result will be the sum of the means
and variances (respectively) of the individual estimates."
  (let ((mean 0)
        (var 0))
    (dolist (e estimates)
      (pcase (mapcar #'string-to-number (split-string e "-"))
	(`(,low ,high)
	 (let ((m (/ (+ low high) 2.0)))
	   (cl-incf mean m)
	   (cl-incf var (- (/ (+ (* low low) (* high high)) 2.0) (* m m)))))
	(`(,value) (cl-incf mean value))))
    (let ((sd (sqrt var)))
      (format "%s-%s"
	      (format "%.1f" (- mean sd))
	      (format "%.1f" (+ mean sd))))))

;; bindings
(add-hook 'org-load-hook
	  (lambda ()
	    ;change becase meta-{up,down,right,left} is already used to change selected window
	    (define-key org-mode-map (kbd "<M-left>") 'nil)
	    (define-key org-mode-map (kbd "<M-up>") 'nil)
	    (define-key org-mode-map (kbd "<M-right>") 'nil)
	    (define-key org-mode-map (kbd "<M-down>") 'nil)
	    (define-key org-mode-map (kbd "<C-S-left>") 'org-metaleft)
	    (define-key org-mode-map (kbd "<C-S-up>") 'org-metaup)
	    (define-key org-mode-map (kbd "<C-S-right>") 'org-metaright)
	    (define-key org-mode-map (kbd "<C-S-down>") 'org-metadown)
	    ;add
	    (define-key org-mode-map (kbd "C-c C-r") 'org-refile)
	    (define-key org-mode-map (kbd "C-c C-d") 'org-toggle-todo-with-timestamp-and-fold)
	    ;just remove
	    (define-key org-mode-map (kbd "<C-tab>") nil)
	    )
)

;; settings
(setq
 org-directory "~/.emacs.d/org"
 org-agenda-files (list "~/doc/agenda.org" "~/.emacs.d/org/todo.org")

 org-default-notes-file "~/.emacs.d/org/notes.org"
 org-mobile-directory "~/android/org-mode"
 org-export-preserve-breaks t
 org-agenda-ndays 7
 org-log-done 'note
 ;; org-startup-folded 'content
 org-deadline-warning-days 4
 org-agenda-show-all-dates t
 ;; org-agenda-skip-deadline-if-done t
 ;; org-agenda-skip-scheduled-if-done t
 org-agenda-start-on-weekday 1
 org-agenda-repeating-timestamp-show-all t
 ;; org-reverse-note-order t
 org-startup-indented nil
 org-remember-store-without-prompt t
 ;; org-hierarchical-todo-statistics
 org-remember-templates (quote ((116 "* TODO %?" "~/.emacs.d/org/todo.org" "Tasks")))
 org-remember-templates '(("Tasks" ?t "* TODO %?" "~/.emacs.d/org/todo.org" "Tasks")
                          ("Work" ?w "* TODO %?" "~/.emacs.d/org/todo.org" "Work")
                          ("Emacs" ?e "* TODO %?" "~/.emacs.d/org/todo.org" "Emacs"))
 org-todo-keywords '((sequence "TODO(t)" "MAYBE(m)" "|" "PR(p)" "DONE(d)" "WONTDO(w)" "INISSUE(i)" "LATER(l)"))
 )
(zenburn-with-color-variables
  (setq org-todo-keyword-faces `(("MAYBE" . ,zenburn-yellow-2)
                                 ("WONTDO" . ,zenburn-blue))))

;; enable <sTAB to create source code block
(require 'org-tempo)

(add-hook 'org-load-hook
	  (lambda ()
	    (setq shift-select-mode nil) ;; to modify dates
	    ))

;; babel src blocks
(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)
   (python . t)
   (emacs-lisp . t)
   (gnuplot . t)
   (R . t)
   ))
;; fontify in org mode
(setq org-src-fontify-natively t)

;; (setq org-clock-persist 'history)
;; (org-clock-persistence-insinuate)

;; activate narrow-to-region
;; C-x n n to narrow region
;; C-x n s in org-mode to narrow current bullet point
;; C-x n w to un-narrow
(put 'narrow-to-region 'disabled nil)

;; disable end-of-visual-line in org-mode
(setq line-move-visual nil)

;; flyspell on org-mode
(add-hook 'org-mode-hook
          (lambda ()
            (flyspell-mode 1)))

;; org-journal
(require 'org-journal)
(setq org-journal-dir "~/.emacs.d/journal/")
(setq org-journal-file-format "%Y-%m-%d.org")
(setq org-journal-tag-alist '(("manger" . ?1) ("home" . ?2) ("codereview" . ?r) ("ops" . ?o) ("meeting" . ?m) ("veille" . ?v)))
(setq org-journal-date-prefix "* ")
(setq org-journal-date-format "%Y-%m-%d, %A")
(setq org-journal-time-format "%R ")
(setq org-journal-find-file 'find-file)
(setq org-journal-carryover-items "")
;; link org-journal with org-agenda
(add-to-list 'org-agenda-files org-journal-dir)




(defun jds:human-time-to-seconds (human-time)
  "input: '03:14', output: 11640"
  (let* ((time (parse-time-string human-time))
         (seconds
          (+
           (nth 0 time)
           (* 60 (nth 1 time))
           (* 60 60 (nth 2 time)))))
    seconds))
(defun jds:seconds-to-human-time (seconds)
  "input: 11640, output: '03:14'"
  (format-seconds "%.2h:%.2m" seconds))
(defun jds:get-date ()
  "Read org-journal date at entry"
  (buffer-substring-no-properties (+ (point) 2) (+ (point) 12)))
(defun jds:get-timestamp (&optional pos)
  "Read org-journal timestamp at entry"
  (let ((pos (or pos (point))))
    (buffer-substring-no-properties (+ pos 3) (+ pos 8))))
;; TODO
;; - dynamic block for weekly/monthly summary
;; - support :pause: tags for long pause during the day
;; - better error when missing tag or other entry, for now get "date-to-time: Invalid date: 2020-02-02 "
(defun jds:get-summary ()
  (let* (
         (date (car (org-map-entries 'jds:get-date "LEVEL=1")))
         (times-human
          (list
           :arrival
           (car (org-map-entries 'jds:get-timestamp "LEVEL=2"))
           :lunch-start
           ;;(or
            (car (org-map-entries 'jds:get-timestamp "manger+LEVEL=2"))
            ;;"12:15")
           :lunch-end
           ;;(or
            (car (org-map-entries (lambda ()
                                    (jds:get-timestamp (org-get-next-sibling))) "manger+LEVEL=2"))
            ;;"13:20")
           :departure
           ;;(or
            (car (org-map-entries 'jds:get-timestamp "home+LEVEL=2"))
            ;;"19:00")
           ))
         (times
          (cl-loop for (key value) on times-human by 'cddr
                   append (list key (date-to-time (concat "2020-02-02 " value)))))
         (durations-minutes
          (list
           :morning
           (/ (time-to-seconds (time-subtract (plist-get times :lunch-start) (plist-get times :arrival))) 60)
           :lunch
           (/ (time-to-seconds (time-subtract (plist-get times :lunch-end) (plist-get times :lunch-start))) 60)
           :afternoon
           (/ (time-to-seconds (time-subtract (plist-get times :departure) (plist-get times :lunch-end))) 60)
           ))
         (durations-minutes
          (plist-put
           durations-minutes
           :work
           (+ (plist-get durations-minutes :morning) (plist-get durations-minutes :afternoon))))
         (durations-human
          (cl-loop for (key value) on durations-minutes by 'cddr
                   append (list key (jds:seconds-to-human-time (* 60 value)))))
         )
    ;; debug messages
    (message "jds: date: %s" date)
    (message "jds: times: %s"
                    (string-join
                     (cl-loop for (key value) on times-human by 'cddr collect value)
                     " "))
    (message "jds: durations-minutes: %s"
                    (string-join
                     (cl-loop for (key value) on durations-minutes by 'cddr collect (format "%s" value))
                     " "))
    (message "jds: durations: %s"
                    (string-join
                     (cl-loop for (key value) on durations-human by 'cddr collect value)
                     " "))
    ;; return nested plist
    (list
     :date date
     :times-human times-human
     :times times
     :durations-minutes durations-minutes
     :durations-human durations-human
     )))


;; TODO support (=ignore?) nil days in avg
;; TODO split interactive function and files list as input and maybe computation?
;; TODO dump csv
;; TODO dblock with weekly and monthly summary?
;; TODO update all old journal files
(defun jds:summary-multi-day (date-regex-prefix)
  "Usage: 2020-03-2[34567]"
  (interactive "sDate prefix? ")
  (let* ((files
          (directory-files (expand-file-name org-journal-dir) t
                           (concat "^" date-regex-prefix ".*\.org$")))
         (works-human
          (org-map-entries
           (lambda ()
             (let* ((org-trust-scanner-tags t))
               (org-entry-get nil "work")))
           "LEVEL=1" files))
         (works-minutes
          (org-map-entries
           (lambda ()
             (let* ((org-trust-scanner-tags t))
               (string-to-number (org-entry-get nil "work-minutes"))))
           "LEVEL=1" files))
         (sum
          (apply '+ works-minutes))
         (len
          (length works-minutes))
         (daily-avg
          (/ (float sum) len))
         (weekly-avg
          (* 5 daily-avg))
         (weekly-avg-human
          (jds:seconds-to-human-time (* 60 weekly-avg)))
         )
    (message "jds: %s: %s %s %s %s %s" works-minutes sum len daily-avg weekly-avg weekly-avg-human)
    (message "jds: weekly-avg: %s" weekly-avg-human)
    ))


(defun jds:update-properties ()
  "Update journal daily summary as top-level org entry properties"
  (interactive)
  (let* ((summary (jds:get-summary))
         (times-human (plist-get summary :times-human))
         (durations-minutes (plist-get summary :durations-minutes))
         (durations-human (plist-get summary :durations-human)))
    (org-map-entries
     (lambda ()
       (org-entry-put (point) "work" (plist-get durations-human :work))
       (org-entry-put (point) "work-minutes" (format "%d" (plist-get durations-minutes :work))))
     "LEVEL=1")))
(defun jds:update-properties-multi-day (date-prefix)
  (interactive "sDate prefix? ")
  (let* ((files
          (directory-files (expand-file-name org-journal-dir) t
                           (concat "^" (regexp-quote date-prefix) "-.*\.org$"))))
    (org-map-entries 'jds:update-properties "LEVEL=1" files)))

;; deprecated, to be repaced by weekly/monthly summary
(defun org-dblock-write:journal-daily-summary (params)
  ;;(jds:get-summary)
  )
(defun org-insert-dblock:journal-daily-summary ()
  "Wizard to interactively insert a journal-daily-summary dynamic block."
  (interactive)
  (let* ((params (list :name "journal-daily-summary")))
    (org-create-dblock params)
    (org-update-dblock)))
(if (fboundp 'org-dynamic-block-define)
    (org-dynamic-block-define "journal-daily-summary" 'org-insert-dblock:journal-daily-summary))





;; clocking work
;;  many from https://writequit.org/denver-emacs/presentations/2017-04-11-time-clocking-with-org.html
;; Show lot of clocking history so it's easy to pick items off the `C-c I` list
(setq org-clock-history-length 20)

(defun eos/org-clock-in ()
  (interactive)
  (org-clock-in '(4)))

(global-set-key (kbd "C-c I") #'eos/org-clock-in)
(global-set-key (kbd "C-c O") #'org-clock-out)

(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
;; Clock out when moving task to a done state
(setq org-clock-out-when-done t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ergo emacs
(require 'keyfreq)
(setq keyfreq-file "~/.emacs.d/keyfreq"
      keyfreq-file-lock "~/.emacs.d/keyfreq.lock")
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

;;see http://www.emacswiki.org/emacs/IgnacioKeyboardQuit , with a little bit of modifications
(defun my-keyboard-quit()
  "Escape the minibuffer or cancel region consistently using 'Control-g'.
Normally if the minibuffer is active but we lost focus (say, we clicked away or set the cursor into another buffer)
we can quit by pressing 'ESC' three times. This function handles it more conveniently, as it checks for the condition
of not being in the minibuffer but having it active. Otherwise simply doing the ESC or (keyboard-escape-quit) would
brake whatever split of windows we might have in the frame."
  (interactive)
  (if (and (not (window-minibuffer-p (selected-window)))
	     (active-minibuffer-window))
      (keyboard-escape-quit)
    (keyboard-quit)))
(define-key global-map (kbd "C-g") 'my-keyboard-quit)

;;find file at point
(global-set-key (kbd "<C-return>") 'ffap)

;;I just want C-x k to kill the buffer instead of just prompting me
;;for it
(defun kill-current-buffer ()
  (interactive)
  (when (or (not (and (fboundp 'erc-server-buffer-live-p)
                      (erc-server-buffer-live-p)))
            (y-or-n-p "Really kill erc buffer?"))
    (kill-buffer)))
(global-set-key (kbd "C-x k") 'kill-current-buffer)
;;like C-x k, but nicer :-)
(global-set-key (kbd "C-x l") 'bury-buffer)
;;could not live without
(global-set-key (kbd "M-q") 'backward-kill-word)
(global-set-key (kbd "C-M-q") 'backward-kill-sexp)
(global-set-key (kbd "C-q") 'backward-delete-char)
(global-set-key (kbd "C-Q") 'quoted-insert)
;;rebind previous M-q binding to M-s
(global-set-key (kbd "M-s") 'fill-paragraph)
;;nice to have, coherent with other keybindings, and bound to nothing
;;by default, so ...
(global-set-key (kbd "M-n") 'scroll-up)
(global-set-key (kbd "M-p") 'scroll-down)
;;shortcuts for region commenting
(global-set-key (kbd "C-c u") 'uncomment-region)
(global-set-key (kbd "C-c c") 'comment-region)
;;M-g defaults to a prefix, I just rebind next/previous error and bind
;;M-g to goto
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-c n") 'next-error)
(global-set-key (kbd "C-c p") 'previous-error)
;;switch between .c and .h
(global-set-key (kbd "C-c o") 'ff-find-other-file)
;;handy, but buggy on terminals
(global-set-key (kbd "C-,") 'undo)
(defun my-kill-whole-line ()
  (interactive)
  (let ((col (current-column)))
    (kill-whole-line 1)
    (move-to-column col)))
(global-set-key (kbd "C-S-k") 'my-kill-whole-line)
;;easy window management for azerty keyboards
(global-set-key (kbd "M-é") 'split-window-vertically)
(global-set-key (kbd "M-\"") 'split-window-horizontally)
(global-set-key (kbd "M-&") 'delete-other-windows)
(global-set-key (kbd "M-à") 'delete-window)

;;browse kill ring to look for forgotten copy/paste
(global-set-key (kbd "C-c k") 'browse-kill-ring)

;; kill-whitespace seems more usefull than kill-sentence
(global-set-key (kbd "M-k") 'kill-whitespace)
;; Join this line to previous and fix up whitespace at join.
(global-set-key (kbd "C-c j") 'join-line)

;; miniedit: edit minibuffer in real buffer
(autoload 'miniedit "miniedit" nil t)
(define-key minibuffer-local-map (kbd "C-M-e") 'miniedit)

;; replace $$ in M-! by the name of the associated buffer
(defun shell-command-replace (command &optional output-buffer error-buffer)
  "Same as shell-command, but replace occurences of $$ by the current buffer name"
  (interactive
   (list
    (read-shell-command "Shell command: " nil nil
			(and buffer-file-name
			     (file-relative-name buffer-file-name)))
    current-prefix-arg
    shell-command-default-error-buffer))
  (shell-command (replace-regexp-in-string "\\$\\$" (buffer-name (current-buffer-not-mini)) command)
		 output-buffer error-buffer))
(global-set-key (kbd "M-!") 'shell-command-replace)

;;zap to char -> zap up to char
;;found at emacs wiki, added the repeat part
(defun zap-up-to-char (arg char)
  "Kill up to, but not including ARGth occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found.
Ignores CHAR at point."
  (interactive "p\ncZap up to char: ")
  (let ((direction (if (>= arg 0) 1 -1)))
    (kill-region (point)
		 (progn
		   (forward-char direction)
		   (unwind-protect
		       (search-forward (char-to-string char) nil nil arg)
		     (backward-char direction))
		   (point))))
  ;;repeat same key to repeat command. adapted code found in kmacro
  (message "Press %s to repeat" (char-to-string char))
  (if (equal char (read-event))
      (zap-up-to-char arg char)
    (setq unread-command-events (list last-input-event))))
(defun zap-up-to-char-back (char)
  (interactive "cBackward zap up to char: ")
  (zap-up-to-char -1 char))
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "C-M-z") 'zap-up-to-char-back)

;; typematrix
(global-set-key (kbd "C-M-<") 'end-of-buffer)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Super keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; to use on console environments, where s- just doesn't work.
;; seriously, when is the last time you used insert?
(define-key function-key-map (kbd "<insert>") 'event-apply-super-modifier)
(define-key function-key-map (kbd "<insertchar>") 'event-apply-super-modifier)
(define-key function-key-map (kbd "<f5>") 'event-apply-super-modifier)
(global-set-key (kbd "<insertchar>") nil)
(global-set-key (kbd "<insert>") nil)
(global-set-key (kbd "<f5>") nil)
;;shortcuts to two-keys commands I often use
(global-set-key (kbd "s-s") 'save-buffer)
(global-set-key (kbd "s-b") 'switch-to-buffer)
(global-set-key (kbd "s-c") 'compile)
(global-set-key (kbd "s-j") 'compile-with-style-check)
(global-set-key (kbd "s-f") 'find-file)
(global-set-key (kbd "s-u") 'undo)
(global-set-key (kbd "s-i") 'indent-whole-buffer)
(global-set-key (kbd "s-x") 'exchange-point-and-mark)
(global-set-key (kbd "s-SPC") 'pop-global-mark)
(global-set-key (kbd "s-;") 'edit-emacs)
(global-set-key (kbd "s-k") 'kill-whitespace)
(global-set-key (kbd "<s-left>") 'winner-undo)
(global-set-key (kbd "<s-right>") 'winner-redo)
(defun open-shell-here ()
  (interactive)
  (launch-command "gnome-terminal" ""))
(global-set-key (kbd "s-h") 'open-shell-here)

(defun todos ()
  (interactive)
  (find-file "~/.emacs.d/org/todo.org"))
(global-set-key (kbd "s-n") 'note)
(global-set-key (kbd "s-t") 'todos)
(global-set-key (kbd "s-l") 'bury-buffer)
;; ghosts of past yanks
(global-set-key (kbd "s-y") (lambda ()
			      (interactive)
			      (popup-menu 'yank-menu)))
(defun duplicate-current-line ()
  (interactive)
  "Duplicate current line"
  (let ((text (buffer-substring (line-beginning-position) (line-end-position))))
    (save-excursion
      (end-of-line)
      (newline)
      (insert text))))
(global-set-key (kbd "s-d") 'duplicate-current-line)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Easy buffer switching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar switch-include-erc t
  "Do we include erc buffers in buffer switching?")
(defun toggle-switch-to-erc ()
  (interactive)
  (toggle-variable 'switch-include-erc)
  (message "Now %s"
	   (if switch-include-erc "including erc" "excluding erc")))
;;quickly switch buffers
(defun switch-to-nth-buffer (n arg)
  "Switches to nth most recent buffer. Ignores erc buffers unless switch-include-erc is non-nil."
  (catch 'tag
    (mapcar (lambda (b)
	      (if (or switch-include-erc
		      (not (eq (buffer-local-value 'major-mode b) 'erc-mode)))
		  (unless (minibufferp b)
					;(unless (string-match "^\\*" (buffer-name b))
		    (if (= n 1)
			(progn
			  (switch-to-buffer b)
			  (throw 'tag nil))
		      (setq n (- n 1))))));)
	    (cdr (buffer-list)))))

(defun switch-to-most-recent-buffer (&optional arg)
  (interactive "P")
  (switch-to-nth-buffer 1 arg))
(defun switch-to-second-most-recent-buffer (&optional arg)
  (interactive "P")
  (switch-to-nth-buffer 2 arg))
(defun switch-to-third-most-recent-buffer (&optional arg)
  (interactive "P")
  (switch-to-nth-buffer 3 arg))

;;fast switching between two buffers
(global-set-key (kbd "<s-tab>") 'switch-to-most-recent-buffer)
(global-set-key (kbd "s-TAB") 'switch-to-most-recent-buffer)
;;fast switching between three buffers
;; (global-set-key (kbd "<C-tab>") 'switch-to-second-most-recent-buffer)
;; (global-set-key (kbd "<C-s-tab>") 'switch-to-third-most-recent-buffer)
;; switch like alt+tab in standard wm
(global-set-key (kbd "<C-tab>") 'next-buffer)
(global-set-key (kbd "<C-S-iso-lefttab>") 'previous-buffer)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Misc editing commands without keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun duplicate-region (beg end &optional sep)
  "Duplicate the region"
  (interactive "*r")
  (let ((p (point)))
    (copy-region-as-kill beg end)
    (message "%d" (point))
    (goto-char end)
    (if (stringp sep) (insert sep))
    (yank)
    (goto-char p)))

(defun exchange-lines ()
  "Exchanges line at point with line at mark"
  (interactive)
  (save-excursion
    (transpose-lines 0)))

;;huge hack, but emacs internals are quite messy concerning
;;this. Don't even try to use regexps in the arguments :)
(defun query-exchange (str1 str2 &optional delimited start end)
  "Exchange str1 and str2 with a regexp replace"
  (interactive
   (let ((common
	  (query-replace-read-args
	   (concat "Query exchange"
		   (if current-prefix-arg " word" "")
		   " regexp"
		   (if (and transient-mark-mode mark-active) " in region" ""))
	   t)))
     (list (nth 0 common) (nth 1 common) (nth 2 common)
	   ;; These are done separately here
	   ;; so that command-history will record these expressions
	   ;; rather than the values they had this time.
	   (if (and transient-mark-mode mark-active)
	       (region-beginning))
	   (if (and transient-mark-mode mark-active)
	       (region-end)))))
  (defun my-aux-fun (match1 match2)
    (if (match-string 1) str2 str1))
  (defun my-add-word-boundary (str)
    (if current-prefix-arg (concat "\\<" str "\\>") str))
  (query-replace-regexp (format "\\(%s\\)\\|\\(%s\\)"
				(my-add-word-boundary str1)
				(my-add-word-boundary str2))
			'(my-aux-fun) delimited start end))


(defun increment-number-at-point ()
  "Increment the number at point, if any."
  (interactive)
  (skip-chars-backward "0123456789")
  (or (looking-at "[0123456789]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

(defun decrement-number-at-point ()
  "Increment the number at point, if any."
  (interactive)
  (skip-chars-backward "0123456789")
  (or (looking-at "[0123456789]+")
      (error "No number at point"))
  (replace-match (number-to-string (1- (string-to-number (match-string 0))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ediff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;vertical split (terminology is confusing)
(setq ediff-split-window-function 'split-window-horizontally)
;;no separate frame
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;;kill variants
(setq ediff-keep-variants nil)

;; restore window config on ediff, found on emacswiki
(add-hook 'ediff-load-hook
		  (lambda ()
			(add-hook 'ediff-before-setup-hook
					  (lambda ()
						(setq ediff-saved-window-configuration (current-window-configuration))))

			(let ((restore-window-configuration
				   (lambda ()
					 (set-window-configuration ediff-saved-window-configuration))))
			  (add-hook 'ediff-quit-hook restore-window-configuration 'append)
			  (add-hook 'ediff-suspend-hook restore-window-configuration 'append))))

;; LANG=Fr breaks ediff on some output ("\ Pas de fin de ligne \303\240 la fin du fichier.")
(add-hook 'ediff-mode-hook '(lambda () (setenv "LANG" "C")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Isearch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;zap to isearch
(defun zap-to-isearch ()
  (interactive)
  (kill-region isearch-opoint isearch-other-end)
  (isearch-done)
  (if (> isearch-other-end isearch-opoint)
      (backward-word)
    (forward-word)))

(define-key isearch-mode-map (kbd "M-z") 'zap-to-isearch)

;;C-o in isearch brings up every hit
(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

;; isearch ends at the beginning of word
(add-hook 'isearch-mode-end-hook 'my-goto-match-beginning)
(defun my-goto-match-beginning ()
  (when (and isearch-forward
			 (not isearch-mode-end-hook-quit))
    (goto-char isearch-other-end)))


(defun my-isearch-yank-region ()
  "Put selection from buffer into search string."
  (interactive)
  (when (region-active-p)
    (deactivate-mark))  ;;fully optional, but I don't like unnecesary highlighting
  (isearch-yank-internal (lambda () (mark))))
(define-key isearch-mode-map (kbd "C-d") 'my-isearch-yank-region)


(require 'thingatpt)
(defun my-isearch-yank-symbol ()
  "Pull next symbol from buffer into search string."
  (interactive)
  (isearch-yank-internal (lambda () (forward-symbol 1) (point))))

(defun my-isearch-yank-symbol-from-beginning ()
  "Move to beginning of symbol before yanking symbol in isearch-mode if search string is empty."
  (interactive)
  (if (= 0 (length isearch-string))
      (beginning-of-thing 'symbol))
  (my-isearch-yank-symbol))
(define-key isearch-mode-map (kbd "C-w") 'my-isearch-yank-symbol-from-beginning)
(define-key isearch-mode-map (kbd "C-x") 'isearch-yank-word-or-char)

;; inspired from http://emacswiki.org/emacs/SearchAtPoint
(defun my-isearch-whole-symbol ()
  "Search for the current string as a whole symbol."
  (interactive)
  (unless isearch-regexp
    (isearch-toggle-regexp))
  (unless (string-match "^\\\\_<.*\\\\_>" isearch-string)
    (setq isearch-string (concat "\\_<" isearch-string "\\_>")
          isearch-message (mapconcat 'isearch-text-char-description
                                     isearch-string "")
          ;; Don't move cursor in reverse search.
          isearch-yank-flag t))
  (ding)
  (isearch-search-and-update))

(defun my-isearch-yank-search-symbol ()
  "Yank and search symbol at point."
  (interactive)
  (my-isearch-yank-symbol-from-beginning)
  (my-isearch-whole-symbol))

(define-key isearch-mode-map (kbd "M-e") 'my-isearch-whole-symbol)
(define-key isearch-mode-map (kbd "C-e") 'my-isearch-yank-search-symbol)

;; merge regexp-search-ring and search-ring
;;TODO make this work
(defalias 'regexp-search-ring 'search-ring)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Regions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;expand-region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
;; cc modes
(er/c-define-construct er/c-mark-template-usage er/c-mark-fully-qualified-name "<"
                       "Mark the current template uage.")
(defun my-er/add-cc-mode-expansions ()
  (make-variable-buffer-local 'er/try-expand-list)
  (setq er/try-expand-list (append
                            er/try-expand-list
                            '(er/c-mark-template-usage-1
                              er/c-mark-template-usage-2))))

(add-hook 'c++-mode-hook 'my-er/add-cc-mode-expansions)

;;delete active region when inserting text in it
(delete-selection-mode t)
;; and still no transient mark please
(transient-mark-mode -1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dictionnaries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'flyspell)
(setq my-languages '("american" "francais"))
(setq-default ispell-local-dictionary (car my-languages))
(defun flyspell-lang (&optional lang)
  "Toggle flyspell-mode with the given lang, or prompt with ido."
  (interactive)
  (let* ((old-language ispell-local-dictionary)
		 (language (if lang lang
					 (ido-completing-read
					  "Spell Language:"
					  ;; set old language as first value, for simple toggle
					  (if (member old-language my-languages)
						  (cons old-language (remove old-language my-languages))
						my-languages)
					  nil t))))
    (if (and flyspell-mode
			 (string= old-language language))
		;; toggle off
		(flyspell-mode 0)
      ;; use language
	  (ispell-change-dictionary language)
	  (unless flyspell-mode
		(flyspell-mode t)))))

;; flyspell-prog-mode *not* on strings
(setq flyspell-prog-text-faces
      (delq 'font-lock-string-face
            flyspell-prog-text-faces))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; W3M
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; w3m
;; TODO load emacs-w3m with el-get
;; (require 'w3m)
;; (setq w3m-use-cookies t)
;; (setq w3m-use-title-buffer-name t)
;; (setq w3m-default-display-inline-images t)
;; (setq w3m-toggle-inline-images-permanently nil)
;; (setq mm-w3m-safe-url-regexp nil)
;; (define-key w3m-minor-mode-map "m"
;;   'w3m-view-url-with-external-browser)
;; (defun w3m-switch ()
;;   (interactive "")
;;   (if (eq 'w3m-mode (current-mm))
;;       (w3m-close-window)
;;     (w3m)))
;; (defalias 'w3m-ems-create-image 'create-image) ; this is only a workaround for emacs24, will be fixed
;; (global-set-key (kbd "s-w") 'w3m-switch)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tab completion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; need 23.2
(setq tab-always-indent 'complete)
(defun my-dabbrev-expand ()
  (when (and (not (bolp))
	     (looking-at "\\_>"))
    (dabbrev-expand nil)))
(defun my-dabbrev-expand-and-nil ()
  (my-dabbrev-expand)
  nil)
(setq completion-at-point-functions '(my-dabbrev-expand-and-nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; YASnippet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'yasnippet)
;; (add-to-list 'yas/snippet-dirs "~/.emacs.d/yas")
;; (yas/global-mode 1)
;; (setq yas/trigger-key nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; xiki
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Beginning of the el4r block:
;; RCtool generated this block automatically. DO NOT MODIFY this block!
;; (add-to-list 'load-path "/home/riccardi/.rvm/rubies/ruby-1.9.3-p327/share/emacs/site-lisp")
;; (require 'el4r)
;;(el4r-boot)
;; End of the el4r block.
;; User-setting area is below this line.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; XML
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'xml)

(defun xml-unescape-string (s)
  "Unescape protected entities in S."
  (let* ((xml-entity-alist '(("lt"   . "<")
                             ("gt"   . ">")
                             ("apos" . "'")
                             ("quot" . "\"")
                             ("amp"  . "&")))
         (re (concat "&\\("
                    (mapconcat (lambda (e)
                                 (car e)) xml-entity-alist "\\|")
                    "\\);")))
    (while (string-match re s)
      (setq s (replace-match
               (cdr (assoc (match-string 1 s) xml-entity-alist)) nil nil s)))
    s))

(defun xml-escape-region (beg end)
  (interactive "*r")
  (let ((escaped (xml-escape-string (buffer-substring beg end))))
    (delete-region beg end)
    (insert escaped)))

(defun xml-unescape-region (beg end)
  (interactive "*r")
  (let ((unescaped (xml-unescape-string (buffer-substring beg end))))
    (delete-region beg end)
    (insert unescaped)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Web related
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun google-search-region (beg end)
  (interactive "*r")
  (browse-url (format "http://www.google.com/search?sourceid=emacs&ie=UTF-8&q=%s"
                      (url-hexify-string (buffer-substring beg end)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Notification framework (used in ERC)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-frame-name (&optional frame)
  "Return the string that names FRAME (a frame).  Default is selected frame."
  (unless frame (setq frame (selected-frame)))
  (if (framep frame)
      (cdr (assq 'name (frame-parameters frame)))
    (error "Function `get-frame-name': Argument not a frame: `%s'" frame)))

(defun frame-focus-p (&optional frame)
  "Return t iff the given frame has the X focus.
If frame is a non X terminal frame, return (frame-visible-p frame)."
  (unless frame (setq frame (selected-frame)))
  (if (not (string= 'x (framep frame)))
      ;; not a X terminal frame
      (frame-visible-p frame)
    (let ((focus-result (shell-command-to-string "~/bin/getInputFocus"))
          (frame-name (get-frame-name frame)))
      (member frame-name (split-string focus-result "\n")))))

(defun window-focus-p (&optional window)
  "Return t iff the given window has the X focus.
That is, the window is the selected window, and the frame
displaying it has the focus."
  (unless window (setq window (selected-window)))
  (and (eq window (selected-window))
	   ;; The selected window always resides on the selected frame.
	   (frame-focus-p (selected-frame))))

(defun buffer-focus-p (&optional buffer-or-name)
  "Returns t iff the given buffer-or-name is displayed in a
  window that has the focus."
  (let ((window (get-buffer-window buffer-or-name)))
	(if window
		(window-focus-p window)
	  nil)))

;;notification
(defvar do-not-disturb nil
  "Set this if you don't want to be disturbed by notifications")
;;(require 'notifications)
;; (defmacro notify (&rest PARAM)
;;   "Notify user by graphical display"
;;   (unless do-not-disturb
;;     `(notifications-notify ,@PARAM)))
;; temporary macro with notify-send, because there is a bug with utf8 and dbus in emacs
(defun notify-raw (title message &optional backshash-escape html-escape)
  "Notify user by graphical display.
If escape is not nil, then disable interpretation of backshash escapes.
If html is not nil, then disable interpretation of html code."
  (unless do-not-disturb
    (call-process "notify-send" nil nil nil
                  title message (format "--icon=%s" "emacs"))))
(defun notify (title message)
  "Notify user by graphical display"
  (notify-raw title message nil t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ERC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "~/.emacs.d/erc.el")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Customs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Personal config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq personal-config-file "~/.emacs.d/perso.el")
(if (file-exists-p personal-config-file)
    (load personal-config-file)
  (message "No personal config file found."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Work config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq work-config-file "~/.emacs.d/work.el")
(if (file-exists-p work-config-file)
    (load work-config-file)
  (message "No work config file found."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; des tests pour gérer les buffers par frame

;;ido-make-buffer-list-hook


;; ;; a list of virtual desktops
;; a list of buffers for each virtual desktop

;;  (let ((frame (window-frame (get-buffer-window (current-buffer)))))

;; (defun virtual-desktops-init ()
;;   "init the virtual-desktops"
;;   (set-frame-parameter nil 'vd-buffers
;; 		       (list (current-buffer))))

;; (defadvice get-buffer-create (after set-buffer-to-desktop (buffer))
;;   "Add the newly created buffer to the current desktop"
;;   (if (bufferp buffer)
;;       (message (buffer-name buffer))
;;     (message buffer)))
;; ;; (set-frame-parameter
;; ;;    nil
;; ;;    'vd-buffers
;; ;;    (cons
;; ;;     (get-buffer buffer)
;; ;;     (frame-parameter nil 'vb-buffers))))

;; (ad-activate 'get-buffer-create)

;; (virtual-desktops-init)

;; (frame-parameter nil 'vd-buffers)

(defun my-backward-up-list (&optional ARG)
  (interactive)
  ;; TODO: use advice instead of this, to make numeric argument work.
  ;; (and how about negative argument?)
  (let ((start (nth 8 (syntax-ppss))))
    (if start
        (goto-char start)
      (backward-up-list ARG))))

;; fix gud override
;; TODO better than just copy & patch this function
(defun gud-display-line (true-file line)
  (let* ((last-nonmenu-event t)	 ; Prevent use of dialog box for questions.
	 (buffer
	  (with-current-buffer gud-comint-buffer
	    (gud-find-file true-file)))
	 (window (and buffer
		      (or (get-buffer-window buffer)
			  ;; (if (memq gud-minor-mode '(gdbmi gdba))
			  ;;     (or (if (get-buffer-window buffer 'visible)
			  ;;             (display-buffer buffer nil 'visible))
			  ;;         (unless (gdb-display-source-buffer buffer)
			  ;;           (gdb-display-buffer buffer nil 'visible))))
			  (display-buffer buffer))))
	 (pos))
    (if buffer
	(progn
	  (with-current-buffer buffer
	    (unless (or (verify-visited-file-modtime buffer) gud-keep-buffer)
		  (if (yes-or-no-p
		       (format "File %s changed on disk.  Reread from disk? "
			       (buffer-name)))
		      (revert-buffer t t)
		    (setq gud-keep-buffer t)))
	    (save-restriction
	      (widen)
	      (goto-line line)
	      (setq pos (point))
	      (or gud-overlay-arrow-position
		  (setq gud-overlay-arrow-position (make-marker)))
	      (set-marker gud-overlay-arrow-position (point) (current-buffer))
	      ;; If they turned on hl-line, move the hl-line highlight to
	      ;; the arrow's line.
	      (when (featurep 'hl-line)
		(cond
		 (global-hl-line-mode
		  (global-hl-line-highlight))
		 ((and hl-line-mode hl-line-sticky-flag)
		  (hl-line-highlight)))))
	    (cond ((or (< pos (point-min)) (> pos (point-max)))
		   (widen)
		   (goto-char pos))))
	  (when window
	    (set-window-point window gud-overlay-arrow-position)
	    (if (memq gud-minor-mode '(gdbmi gdba))
		(setq gdb-source-window window)))))))
