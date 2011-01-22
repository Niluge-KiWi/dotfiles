;; -*- coding: utf-8 -*-
;;; Emacs of Thomas Riccardi. Homepage : http://github.com/Niluge-KiWi/dotfiles
;; Many things from http://github.com/antoine-levitt/perso, and some other things

;; Can be viewed in outline mode


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Path setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path (expand-file-name "~/.emacs.d/"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/auto-complete/"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/erc-5.3/"))

;;byte-recompile elisp files if they need to be
;;(byte-recompile-directory "~/.emacs.d" 0)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Package Management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;-------ELAP
;; This provides support for the package system and
;; interfacing with ELPA, the package archive.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

;;-------el-get
;; Manage the external elisp bits and pieces you depend upon
(add-to-list 'load-path (expand-file-name "~/.emacs.d/el-get/el-get"))
(require 'el-get)
(setq el-get-sources
      '((:name el-get :type git
               :url "git://github.com/dimitri/el-get.git")
		(:name apache :type http
			   :url "http://www.emacswiki.org/cgi-bin/wiki/download/apache-mode.el")
		(:name auto-complete :type git
			   :url "git://github.com/m2ym/auto-complete.git")
		(:name auto-complete-etags :type http
			   :url "http://www.emacswiki.org/emacs/download/auto-complete-etags.el")
		(:name autopair :type http
			   :url "http://autopair.googlecode.com/svn/trunk/autopair.el")
		(:name browse-kill-ring :type http
			   :url "http://www.emacswiki.org/cgi-bin/wiki/download/browse-kill-ring.el")
		(:name cedet :type http-tar
			   :options ("xzf")
			   :info "common"
			   :build ("make")
			   :url "https://sourceforge.net/projects/cedet/files/cedet/cedet-1.0.tar.gz/download")
		(:name cmake :type http
			   :url "http://www.cmake.org/CMakeDocs/cmake-mode.el")
		(:name dired+ :type http
			   :url "http://www.emacswiki.org/cgi-bin/wiki/download/dired%2b.el")
		(:name erc-view-log :type git
			   :url "git@github.com:Niluge-KiWi/erc-view-log.git")
		(:name fold-dwim :type http
			   :url "http://www.dur.ac.uk/p.j.heslin/Software/Emacs/Download/fold-dwim.el")
		(:name gitsum :type git
			   :url "git://github.com/chneukirchen/gitsum.git")
		(:name google-weather :type git
			   :url "git://git.naquadah.org/google-weather-el.git")
		(:name magit :type git
			   :info "."
			   :build ("make all")
			   :url "git@github.com:Niluge-KiWi/magit.git")
		(:name minimap :type http
			   :url "http://randomsample.de/minimap.el")
		(:name php-mode :type http
			   :url "http://php-mode.svn.sourceforge.net/svnroot/php-mode/tags/php-mode-1.5.0/php-mode.el")
		(:name psvn :type http
			   :url "http://www.xsteve.at/prg/emacs/psvn.el")
		(:name rainbow-mode :type http
			   :url "http://git.naquadah.org/?p=rainbow.git;a=blob_plain;f=rainbow-mode.el;hb=HEAD")
		(:name ropemacs :type http-tar
			   :options ("zxf")
			   :url "http://bitbucket.org/agr/ropemacs/get/tip.tar.gz")
		(:name widen-window :type http
			   :url "http://coderepos.org/share/browser/lang/elisp/widen-window-mode/trunk/widen-window.el?format=txt")
		(:name window-numbering :type http
			   :url "http://nschum.de/src/emacs/window-numbering-mode/window-numbering.el")
		(:name wuxch-dired-copy-paste :type http
			   :url "http://www.emacswiki.org/emacs/download/wuxch-dired-copy-paste.el")
		(:name yaml-mode :type git
			   :url "git://github.com/yoshiki/yaml-mode.git")
		))

(el-get 'sync)

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
;;TODO desktop-save-mode

 ;; desktop
(setq desktop-load-locked-desktop t
	  desktop-path '("~/.emacs.d/")
	  desktop-dirname "~/.emacs.d/"
	  desktop-base-file-name "emacs.desktop")
(desktop-save-mode 1)
;; save every 10mins
(run-with-timer (* 10 60) (* 10 60) (lambda () (flet ((message (&rest args) nil))
												 (desktop-save-in-desktop-dir))))


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Minimap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'minimap)
;; 50ms is more than the repeat period for standard keyboards,
;; so no slow down when scrolling
(setq minimap-update-delay 0.05)

(defun minimap-toggle ()
  "Show minimap if hidden, hide if present."
  (interactive)
  (if (and minimap-bufname
	       (get-buffer minimap-bufname)
	       (get-buffer-window (get-buffer minimap-bufname)))
      (minimap-kill)
    (minimap-create)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Colour theme and fonts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'zenburn)
(zenburn)
(setq font-use-system-font t) ;; since emacs 23.2
;;old way:
;; do this in shell:
;;echo "Emacs.font: Monospace-12" >> ~/.Xresources
;;xrdb -merge ~/.Xresources

(defun reload-zenburn ()
  (interactive)
  (load-file (expand-file-name "~/.emacs.d/lisp/zenburn.el"))
  (color-theme-zenburn))

(require 'rainbow-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mouse
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(setq mouse-yank-at-point t)
(setq mouse-highlight t)
(mouse-avoidance-mode 'jump)
;; control mouse clipboard. In particular, select-active-regions, activated in 23.2, sucks.
;; selection by mouse is the window selection
(setq select-active-regions 'only)
;; window selection is put in the X primary selection
(setq x-select-enable-primary t)
;; and not in X clipboard
(setq x-select-enable-clipboard nil)
(setq mouse-drag-copy-region t)


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
    (start-process "" nil command filename)))

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
	  (replace-match " " nil nil))))))

(defun truncate-list (list n)
  "Truncate LIST to at most N elements destructively."
  (when n
	(let ((here (nthcdr (1- n) list)))
	  (when (consp here)
		(setcdr here nil))))
  list)


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

;; bypass emacs broken mechanism to detect browser
(setq browse-url-browser-function
      (lambda (url &rest args)
	(interactive)
	(launch-command "x-www-browser" url)))

;;just type y/n instead of yes/no RET. this should be default
(fset 'yes-or-no-p 'y-or-n-p)

;;blinking cursor is distracting and useless
(blink-cursor-mode -1)

;;display buffer name in title bar
(setq frame-title-format '("%b" " - " invocation-name "@" system-name))
(setq icon-title-format '("%b" " - " invocation-name "@" system-name))


;; ;;backups/autosaves : no autosaves, and backups in one centralised place
;; (setq auto-save-default nil)
;; (defvar backup-dir "~/.emacsbackups/")
;; (setq backup-directory-alist (list (cons "." backup-dir)))

;;please add a final newline each time I save a buffer
(setq require-final-newline 't)

;; TODO use own pastebin
(require 'pastebin)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Scrolling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;scroll one line at a time
(setq scroll-conservatively 100000000)
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
      ido-max-window-height 1)
(ido-mode 'both) ;; for buffers and files
(ido-everywhere 1)
;; to have the buffers and files open in the selected-window, as done by switch-to-buffer
(setq ido-default-file-method 'selected-window)
(setq ido-default-buffer-method 'selected-window)

;;-------icomplete
;; completion for commands that don't use ido (like help)
(icomplete-mode 1)
;; TODO test icicles

;;-------smex
;; super M-x : ido + frequency
(require 'smex)
(setq smex-history-length 32)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
;;(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Uniquify
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; uniquify buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;; plus some useful functions
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
;;; Parenthesis editing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;visual paren matching
(show-paren-mode t)
;;rainbow parentheses highlighting ! \o/
(require 'highlight-parentheses)
(setq hl-paren-colors
      (list zenburn-red-4 zenburn-orange zenburn-yellow-green+1 zenburn-green zenburn-blue zenburn-dark-blue+2 zenburn-purple+2 nil)) ;; a final fake color, because the last one seems to be ignored
(setq hl-paren-background-colors
	  (make-list (length hl-paren-colors) zenburn-bg-1))

;;highlight-parentheses is a buffer-local minor mode : create a global
;;minor mode of our own
(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Autopair
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'autopair)
(autopair-global-mode) ;; enable autopair in all buffers

(setq autopair-blink nil) ;; no blink
(setq autopair-autowrap t) ;; wrap region with character to insert

;; not in ERC
(add-hook 'erc-mode-hook
	  #'(lambda () (setq autopair-dont-activate t)))
;; pair $ correctly
(add-hook 'LaTeX-mode-hook
          #'(lambda ()
              (modify-syntax-entry ?$ "\"")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dired
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; misc. dired add-ons
(require 'dired-x)
;; omit hidden files + default
(setq dired-omit-files
      (concat dired-omit-files "\\|" "^\\..+$"))
;;clean dired default view : don't display groups, use human-readable sizes
(setq dired-listing-switches "-alhG"
      dired-free-space-args "-Pkm" ;; TODO k and m for what?
      dired-auto-revert-buffer t)
;; Omit, be quiet
(defadvice dired-omit-expunge (around dired-omit-be-quiet)
  "Be quiet."
  (flet ((message (&rest args) ))
    ad-do-it))
(ad-activate 'dired-omit-expunge)
(add-hook 'dired-mode-hook 'dired-omit-mode)

(require 'dired+)
;; copy/pasting in dired
(require 'wuxch-dired-copy-paste)
(define-key dired-mode-map (kbd "M-w") 'wuxch-dired-copy)
(define-key dired-mode-map (kbd "C-w") 'wuxch-dired-cut)
(define-key dired-mode-map (kbd "C-y") 'wuxch-dired-paste)
;;add gnome-open as C-ret
(defun dired-gnome-open-file ()
  "Opens the current file in a Dired buffer."
  (interactive)
  (launch-command "gnome-open" (dired-get-file-for-visit)))
(define-key dired-mode-map (kbd "<C-return>") 'dired-gnome-open-file)

;; LANG=Fr breaks the regexp that matches file sizes (, instead of . separator)
(add-hook 'dired-mode-hook '(lambda () (setenv "LC_NUMERIC" "C")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Psvn
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;C-x v s as main svn entry point
;;note : dired customisations have to be done BEFORE this
(require 'psvn)
;;(global-set-key (kbd "C-x v s") 'svn-examine)
;; TODO merge with magit C-c s: check if .svn is in current dir
;;default to a clean view.
(setq svn-status-hide-unknown t)
(setq svn-status-hide-unmodified t)
;; svn status in big repositories is too slow with verbose
(setq svn-status-verbose nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Gitsum
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; for something like git add --patch, but better:
;;  can split hunks where git add cant
;;  and can also manually edit the patch
(require 'gitsum)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Magit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Something like Egg, but still maintained and developped
(require 'magit)
(require 'magit-svn)

;; no subdirectories for unstaged files
;; TODO get them when open subsection on directory
(setq magit-omit-untracked-dir-contents t)
;; no buffer saving when magit-status
(setq magit-save-some-buffers nil)
;; M-arrows is for window-switching
(define-key magit-mode-map (kbd "<M-left>") nil)
;; "u" and "U" are already taken by unstage...
(define-key magit-mode-map (kbd "o") 'magit-goto-parent-section)

(global-set-key (kbd "C-c s") 'magit-status)
(global-set-key (kbd "C-c C-s") 'magit-status)

;; Diff with remote svn head
(magit-define-command svn-remote-diff ()
  (interactive)
  (magit-diff "remotes/trunk..HEAD"))
(magit-key-mode-insert-action 'svn "d" "Remote diff" 'magit-svn-remote-diff)

;; hide untracked section when opening magit-status
(defun my-magit-hide-untracked ()
  "Hide untracked section."
  (let ((untracked (magit-find-section '(untracked) magit-top-section)))
	(magit-section-set-hidden untracked t)))
(add-hook 'magit-status-mode-hook 'my-magit-hide-untracked)

;; display staged diff when writing commit log
(defun my-magit-display-diff ()
  "Show magit-status window and make staged section visible."
  (let ((buf (magit-find-buffer 'status default-directory)))
	(unless buf
	  ;; no magit-status, create it
	  (let ((curbuf (current-buffer)))
		(magit-status default-directory)
		(setq buf (current-buffer))
		(switch-to-buffer curbuf)))
	(unless buf
	  (error "Error finding/creating magit-status buffer"))
	;; we now have the right magit-status in buf
	(display-buffer buf t)
	(with-selected-window (get-buffer-window buf)
	  (magit-jump-to-staged)
	  (magit-section-expand (magit-find-section '(staged) magit-top-section))
	  (recenter 0))))
(add-hook 'magit-log-edit-mode-hook 'my-magit-display-diff)

;; fyspell on log
(add-hook 'magit-log-edit-mode-hook 'flyspell-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Window management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;move between windows with meta-keypad
(windmove-default-keybindings 'meta)

;; TODO test this
;; widen window : widen selected window
;; (require 'widen-window)
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


;; window numbering
;;
;; window-numbering-mode assigns a number to each window in a Emacs frame,
;; so you can reach any window with just one command (M-1 ... M-0)
;;
;; If you want to affect the numbers, use window-numbering-before-hook or window-numbering-assign-func.
;; For instance, to always assign the calculator window the number 9, add the following to your .emacs file:
;; (setq window-numbering-assign-func
;;       (lambda () (when (equal (buffer-name) "*Calculator*") 9)))

(require 'window-numbering)
(window-numbering-mode t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Session Management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;save the minibuffer input
(savehist-mode 1)

;;save last edit place in files
(setq-default save-place t)
(require 'saveplace)


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

(setq recentf-max-saved-items nil)
(recentf-mode 1)
(global-set-key (kbd "C-x C-r") 'recentf-ido-find-file-or-maybe-list)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Imenu: jump between indexes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'imenu)
(defun ido-goto-symbol ()
  "Update the imenu index and then use ido to select a symbol to navigate to."
  (interactive)
  (imenu--make-index-alist)
  (let ((name-and-pos '())
        (symbol-names '()))
    (flet ((addsymbols (symbol-list)
                       (when (listp symbol-list)
                         (dolist (symbol symbol-list)
                           (let ((name nil) (position nil))
                             (cond
                              ((and (listp symbol) (imenu--subalist-p symbol))
                               (addsymbols symbol))

                              ((listp symbol)
                               (setq name (car symbol))
                               (setq position (cdr symbol)))

                              ((stringp symbol)
                               (setq name symbol)
                               (setq position (get-text-property 1 'org-imenu-marker symbol))))

                             (unless (or (null position) (null name))
                               (add-to-list 'symbol-names name)
                               (add-to-list 'name-and-pos (cons name position))))))))
      (addsymbols imenu--index-alist))
    (let* ((selected-symbol (ido-completing-read "Symbol? " symbol-names))
           (position (cdr (assoc selected-symbol name-and-pos))))
      (push-mark)
      (goto-char position))))
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
;;; Cedet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-file "~/.emacs.d/el-get/cedet/common/cedet.el")
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

;; Templates
(global-srecode-minor-mode 1)

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
(setq semanticdb-default-save-directory (expand-file-name "~/.semanticdb"))
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
;; find . -name "*.h" -o -name "*.cpp" | xargs ~/.emacs.d/el-get/cedet/semantic/semanticdb.sh
;; and in emacs, for system databases: (semanticdb-create-ebrowse-database dir)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; C
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'cc-mode)
;;linux style
(setq c-default-style "linux")
(setq-default c-basic-offset 4)
(setq-default tab-width 4)

;;'electric' indentation : indent on newline
(define-key c-mode-base-map "\C-m"
  'c-context-line-break)
(define-key c-mode-base-map (kbd "M-q") nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; C++
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;TODO ceded...


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Python
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'python)
(defadvice run-python (after run-python-revert-patch)
  "revert patch which removes '' from sys.path"
  (python-send-string "import sys
sys.path.insert(0, '')"))
(ad-activate 'run-python)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PHP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'php-mode)
;; Toggle between PHP & HTML mode.  Useful when working on
;; php files, that can been intertwined with HTML code
(add-hook 'php-mode-hook
	  (lambda ()
	    (global-set-key [f5] 'html-mode)))
(add-hook 'html-mode-hook
	  (lambda ()
	    (global-set-key [f5] 'php-mode)))
;; TODO use Nxhtml instead http://www.emacswiki.org/emacs/NxhtmlMode


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; YAML
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(add-hook 'yaml-mode-hook
	  '(lambda ()
	     (setq indent-tabs-mode nil)
	     (setq c-indent-level 2)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Apache
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'apache-mode "apache-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.htaccess\\'"   . apache-mode))
(add-to-list 'auto-mode-alist '("httpd\\.conf\\'"  . apache-mode))
(add-to-list 'auto-mode-alist '("srm\\.conf\\'"    . apache-mode))
(add-to-list 'auto-mode-alist '("access\\.conf\\'" . apache-mode))
(add-to-list 'auto-mode-alist '("sites-\\(available\\|enabled\\)/" . apache-mode))


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
(add-hook 'LaTeX-mode-hook 'outline-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'outline-minor-mode)
(defun setup-outline-lisp ()
  "Only outline on ;;;, thank you."
  (setq outline-regexp ";;; "))
(add-hook 'emacs-lisp-mode-hook 'setup-outline-lisp)
(require 'fold-dwim)
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
;;; Org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq org-startup-indented t)
(require 'remember)
(add-hook 'remember-mode-hook 'org-remember-apply-template)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c r") 'remember)

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
	    ;just remove
	    (define-key org-mode-map (kbd "<C-tab>") nil)
	    )
)

;; settings
(setq
 org-agenda-files (list "~/.emacs.d/org/todo.org")
 org-default-notes-file "~/.emacs.d/org/notes.org"
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
 org-remember-store-without-prompt t
 ;; TODO what it this?
 ;; org-remember-templates (quote ((116 "* TODO %?" "~/.emacs.d/org/todo.org" "Tasks")
 ;; 				(110 "* %?" "~/.emacs.d/org/notes.org" "Notes")))
 remember-annotation-functions (quote (org-remember-annotation))
 remember-handler-functions (quote (org-remember-handler))
 org-todo-keywords '((sequence "TODO(t)" "IDEA(i)" "|" "DONE(d)" "WONTDO(w)"))
 )

(add-hook 'org-load-hook
	  (lambda ()
	    (setq shift-select-mode nil) ;; to modify dates
	    ))

;; (setq org-clock-persist 'history)
;; (org-clock-persistence-insinuate)

;; activate narrow-to-region
;; C-x n n to narrow region
;; C-x n s in org-mode to narrow current bullet point
;; C-x n w to un-narrow
(put 'narrow-to-region 'disabled nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Google-weather (and mode for org)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'google-weather)
(require 'org-google-weather)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Compilation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'compile)
;;make compile window disappear after successful compilation
(setq compilation-finish-function
      (lambda (buf str)
	(if (string-match "*Compilation*" (buffer-name buf))
	    (unless (string-match "abnormally" str)
	      ;;no errors, make the compilation window go away
	      (message "*Compilation* OK")
	      (delete-windows-on buf)
	      (bury-buffer buf)))))

;;misc compilation settings
(setq-default
 compile-command "make"
 compilation-read-command nil
 compilation-scroll-output 'first-error
 compilation-ask-about-save nil
 compilation-window-height nil
 compilation-auto-jump-to-first-error t
 compilation-disable-input t)

;;compilation by C-c C-c in modes that don't shadow it
(global-set-key (kbd "C-c C-c") 'compile)
;;(global-set-key (kbd "C-M-c C-M-c") 'compile)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CMake
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(autoload 'cmake-mode "~/.emacs.d/cmake-mode.el" t)
(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.cmake\\'"         . cmake-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") 'kill-current-buffer)
;;like C-x k, but nicer :-)
(global-set-key (kbd "C-x l") 'bury-buffer)
;;could not live without
(global-set-key (kbd "M-q") 'backward-kill-word)
(global-set-key (kbd "C-M-q") 'backward-kill-sexp)
(global-set-key (kbd "C-q") 'backward-delete-char)
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
;;sometimes useful (for query-replace and such)
(global-set-key (kbd "C-c C-SPC") 'transient-mark-mode)
;;easy window management for azerty keyboards
(global-set-key (kbd "M-é") 'split-window-vertically)
(global-set-key (kbd "M-\"") 'split-window-horizontally)
(global-set-key (kbd "M-&") 'delete-other-windows)
(global-set-key (kbd "M-à") 'delete-window)

;;browse kill ring to look for forgotten copy/paste
(require 'browse-kill-ring)
(global-set-key (kbd "C-c k") 'browse-kill-ring)

;; kill-whitespace seems more usefull than kill-sentence
(global-set-key (kbd "M-k") 'kill-whitespace)
;; Join this line to previous and fix up whitespace at join.
(global-set-key (kbd "C-c j") 'join-line)

;;make use of that useless ^2 key to do something useful. This can fail on some terminals,
;;so protect
(condition-case err
    (progn
      ;;normal
      (global-set-key (kbd "²") (lambda () (interactive) (insert "\\")))
      ;;isearch
      (define-key isearch-mode-map (kbd "²")
	(lambda ()
	  (interactive)
	  (if current-input-method
	      (isearch-process-search-multibyte-characters ?\\)
	    (isearch-process-search-char ?\\)))))
  (error
   (message "Failed to bind key to \\. Live with it.")))

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
;; some packages, e.g. gnus-summary, don't define deletechar but only delete. Fix that by aliasing
(global-set-key (kbd "<deletechar>") (kbd "<delete>"))


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
(defun note ()
  (interactive)
  (find-file "~/.emacs.d/org/notes.org"))
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
(define-key isearch-mode-map (kbd "C-o")
  (lambda ()
    (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string
	       (regexp-quote isearch-string))))))

;; isearch ends at the beginning of word
(add-hook 'isearch-mode-end-hook 'my-goto-match-beginning)
(defun my-goto-match-beginning ()
  (when (and isearch-forward
			 (not isearch-mode-end-hook-quit))
    (goto-char isearch-other-end)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dictionnaries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'flyspell)
(setq my-languages '("british" "francais"))
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
;;; Auto Completion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; auto-complete mode : dropdown menu
;; see http://cx4a.org/software/auto-complete/manual.html
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/el-get/auto-complete/dict")
(setq ac-comphist-file "~/.emacs.d/el-get/auto-complete/ac-comphist.dat")
(ac-config-default)
(setq ac-delay 0.1)

;;(add-hook 'c++-mode (lambda () (add-to-list 'ac-sources 'ac-source-semantic)))
(add-hook 'shell-mode-hook (lambda () (setq ac-sources 'ac-source-files-in-current-dir)))
(add-to-list 'ac-modes 'shell-mode)

;;etags for auto-complete
(require 'auto-complete-etags)
(add-hook 'c++-mode (lambda () (add-to-list 'ac-sources 'ac-source-etags)))
(add-hook 'c-mode (lambda () (add-to-list 'ac-sources 'ac-source-etags)))

;; auto-complete for python
;; Initialize Rope (for auto-complete)
;; if this doesn't work, here is how to install this:
;;  sudo aptitude install mercurial
;;  mkdir /tmp/rope && cd /tmp/rope
;;  hg clone http://bitbucket.org/agr/rope
;;  hg clone http://bitbucket.org/agr/ropemacs
;;  hg clone http://bitbucket.org/agr/ropemode
;;  sudo easy_install rope
;;  ln -s ../ropemode/ropemode ropemacs/
;;  sudo easy_install ropemacs
(ac-ropemacs-initialize)
(add-hook 'python-mode-hook
          (lambda ()
                 (add-to-list 'ac-sources 'ac-source-ropemacs)))




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
	(let ((focus-name (shell-command-to-string "getInputFocus"))
		  (frame-name (concat (get-frame-name frame) "\n")))
	  (string= focus-name frame-name))))

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
(defun notify (title message)
  "Notify user by graphical display"
  (unless do-not-disturb
    (shell-command-to-string (format
			      "notify-send '%s' '%s' --icon=%s"
				  title message "emacs"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ERC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "~/.emacs.d/erc.el")
;;read personal info (mainly ERC stuff)
(load "~/.emacs.d/perso.el" t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Customs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)


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
