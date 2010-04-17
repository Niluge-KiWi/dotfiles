;; -*- coding: utf-8 -*-

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(inhibit-startup-echo-area-message (user-login-name))
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(menu-bar-mode nil)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(transient-mark-mode nil)
 '(global-font-lock-mode t))

(setq inhibit-splash-screen t)
;;pas de curseur clignotant
(blink-cursor-mode -1)

;;library path : used for require, load-library, autoload ...
;; search in each subdir of ~/.emacs.d/
(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (let* ((my-lisp-dir "~/.emacs.d/")
	   (default-directory my-lisp-dir))
      (setq load-path (cons my-lisp-dir load-path))
      (normal-top-level-add-subdirs-to-load-path)))
;;TODO: débugger ça pcq ça marche pas... en attendant ça marche mieux avec la suite:
(add-to-list 'load-path (expand-file-name "~/.emacs.d/"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/auto-complete/"))

;; default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" system-name))


;; Color Themes
;;(require 'color-theme)
;;(color-theme-initialize)
(require 'zenburn)
(color-theme-zenburn)



;; font
;;TODO: do it when loading emacsclient, because currently we have to reload the config on the first emacsclient launch...
;;TODO: trouver autrechose, car freeze emacs 5s au démarrage...
;; (if (eq window-system 'x)
;;   (set-default-font "Monospace-10"))
;; do this in shell:
;;echo "Emacs.font: Monospace-10" >> ~/.Xresources
;;xrdb -merge ~/.Xresources


;;byte-recompile elisp files if they need to be
(byte-recompile-directory "~/.emacs.d" 0)
(kill-buffer "*Compile-Log*")


;; remote access
(require 'tramp)


;;mode ido
(require 'ido)
(setq ido-create-new-buffer 'always
      ido-enable-flex-matching t
      ;;ido-max-prospects 12
      ido-max-window-height 1)
(ido-mode 'both) ;; for buffers and files
(ido-everywhere 1)
;;ido M-x mode #http://www.emacswiki.org/emacs/InteractivelyDoThings#toc6
(setq ido-execute-command-cache nil)
(defun ido-execute-command ()
  (interactive)
  (let ((ido-enable-flex-matching nil)) ;; too disturbing with too much results
       (call-interactively
	(intern
	 (ido-completing-read
	  "M-x "
	  (progn
	    (unless ido-execute-command-cache
	      (mapatoms (lambda (s)
			  (when (commandp s)
			    (setq ido-execute-command-cache
				  (cons (format "%S" s) ido-execute-command-cache))))))
	    ido-execute-command-cache))))))
;;not used by default
;; (add-hook 'ido-setup-hook
;; 	  (lambda ()
;; 	    (global-set-key (kbd "M-x") 'ido-execute-command)))

;;icomplete : completion for commands that don't use ido (like help)
(icomplete-mode 1)



;; pour gérer les lignes trop longues
;;  "<smeuuh> lll pour local longlines, et ll pour global long lines"
(define-globalized-minor-mode ll
  longlines-mode
  (lambda () (longlines-mode t)))
;;local longlines
(defalias 'lll 'longlines-mode)
;;adapt filling to window size
(setq longlines-wrap-follows-window-size t)

;; Se limiter à des lignes de 80 caractères dans les modes textes (y
;; compris le mode LaTeX) :
;; cf. http://www-verimag.imag.fr/~moy/emacs/#autofill
(add-hook 'text-mode-hook 'turn-on-auto-fill)




;; ess Emacs Speaks Statistics
;;(require 'ess-site)

;; sqlplus mode pour oracle
(require 'sqlplus)
(add-to-list 'auto-mode-alist '("\\.sqp\\'" . sqlplus-mode))




;;paredit :: not used, deactivated
;; (autoload 'paredit-mode "paredit"
;;   "Minor mode for pseudo-structurally editing Lisp code."
;;   t)
;; ;;undefine keys I use
;; (eval-after-load 'paredit
;;   '(progn
;;      (define-key paredit-mode-map (kbd "M-<down>")
;;        nil)
;;      (define-key paredit-mode-map (kbd "M-<up>")
;;        nil)
;;      (define-key paredit-mode-map (kbd "M-\"")
;;        nil)
;;      (define-key paredit-mode-map (kbd "M-q")
;;        'paredit-backward-kill-word)))
;; ;;toggle paredit with f6
;; (global-set-key (kbd "<f6>") 'paredit-mode)


;;-------Latex mode
;;don't ask to cache preamble
(setq preview-auto-cache-preamble t)
;;indent when pressing RET
(setq TeX-newline-function 'newline-and-indent)

(autoload 'whizzytex-mode "whizzytex"
  "WhizzyTeX, a minor-mode WYSIWIG environment for LaTeX"
  t)



;;-------C/C++ mode
(setq c-default-style "linux")
(setq-default c-basic-offset 4)

(defun my-c-indent-setup ()
  (c-set-style "linux")
  (setq-default c-basic-offset 4))
  ;;(setq-default c-basic-offset 8))
(add-hook 'c-mode-hook 'my-c-indent-setup)
(add-hook 'c++-mode-hook 'my-c-indent-setup)

;;'electric' indentation : indent on newline
(add-hook 'c-mode-common-hook (lambda ()
                                (define-key c-mode-base-map "\C-m"
                                  'c-context-line-break)))

;;------java mode
(defun my-java-indent-setup ()
  (c-set-style "java")
  (setq c-basic-offset 4))
(add-hook 'java-mode-hook 'my-java-indent-setup t)


;;-------python mode
(defadvice run-python (after run-python-revert-patch)
  "revert patch which removes '' from sys.path"
  (python-send-string "import sys
sys.path.insert(0, '')"))
(ad-activate 'run-python)


;;-------php mode
(require 'php-mode)
;; Toggle between PHP & HTML mode.  Useful when working on
;; php files, that can been intertwined with HTML code
(add-hook 'php-mode-hook
	  (lambda ()
	    (global-set-key [f5] 'html-mode)))
(add-hook 'html-mode-hook
	  (lambda ()
	    (global-set-key [f5] 'php-mode)))


;;-------compilation
;;make compile window disappear after successful compilation
(setq compilation-finish-function
      (lambda (buf str)
	(if (string-match "*Compilation*" (buffer-name buf))
	    (if (string-match "abnormally" str)
		(message "There were errors :-(")
	      ;;no errors, make the compilation window go away in 2 second
	      (run-at-time 2 nil
			   (lambda (buf)
			     (delete-windows-on buf)
			     (bury-buffer buf))
			   buf)
	      (message "No errors :-)")))))

;;my-compile is smarter about how to display the new buffer
(defun display-buffer-by-splitting-largest (buffer force-other-window)
  "Display buffer BUFFER by splitting the largest buffer vertically, except if
  there is already a window for it."
  (or (get-buffer-window buffer)
      (let ((new-win
	     (with-selected-window (get-largest-window)
	       (split-window-vertically))))
	(set-window-buffer new-win buffer)
	new-win)))

(defun my-compile ()
  "Ad-hoc display of compilation buffer."
  (interactive)
  (let ((display-buffer-function 'display-buffer-by-splitting-largest))
    (call-interactively 'compile)))

;;misc compilation settings
(setq-default
 compile-command "make"
 compilation-read-command nil
 compilation-scroll-output 'first-error
 compilation-ask-about-save nil
 compilation-window-height 5
 compilation-auto-jump-to-first-error t)

;;compilation by C-M-c C-M-c
(global-unset-key (kbd "C-M-c"))
(global-set-key (kbd "C-M-c C-M-c") 'my-compile)




;; dictionnaires francais et anglais


;;(ispell-change-dictionary "francais")
;;(ispell-change-dictionary "american")

;;M-x flyspell-auto-correct-word: automatically correct word.
;;M-x flyspell-correct-word (or mouse-2): popup correct words.

;;(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checking" t)
;;(autoload 'global-flyspell-mode "flyspell" "On-the-fly spelling" t)
;;(global-flyspell-mode t)
(require 'flyspell)
(defun flyspell-french ()
  (interactive)
  (flyspell-mode t)
  (ispell-change-dictionary "francais")
  (flyspell-buffer))
(defun flyspell-english ()
  (interactive)
  (flyspell-mode t)
  (ispell-change-dictionary "british")
  (flyspell-buffer))

;;  (flyspell-buffer))
;;(autoload 'flyspell-danish)




;;supprimer la sélection quand on tape
(delete-selection-mode 1)

;;qwerty du pauvre
(global-set-key (kbd "M-é") 'split-window-vertically)
(global-set-key (kbd "M-\"") 'split-window-horizontally)
(global-set-key (kbd "M-&") 'delete-other-windows)
(global-set-key (kbd "M-à") 'delete-window)


;;make tab the ultimate completion key
(defmacro ad-add-advice-to-key (key expr)
  "Around advice the key KEY with expression EXPR. KEY should be
a key in the format accepted by key-binding and such, and EXPR an
expression of the same type as those required by around advices"
  `(add-hook 'pre-command-hook
	     (lambda ()
	       (when (equal (this-command-keys-vector) ,key)
		 (ad-add-advice this-command
				'(azerrswdf ;arbitrary advice name
				  nil	    ;not protected
				  t	    ;activated
				  (lambda ()
				    ,expr
				    (ad-unadvise this-command)))
				'around
				'last)
		 (ad-activate this-command)))))

(ad-add-advice-to-key [9]
		      (let ((p (point)))
			ad-do-it
			(when (and (not (minibuffer-window-active-p (minibuffer-window)))
				   (= p (point))
				   (not (bolp))
				   (looking-at "\\_>"))
			  (dabbrev-expand nil))))


;;Emacs is a text editor, make sure your text files end in a newline
(setq require-final-newline 't)

;;scrolling
;;scroll one line at a time
(setq scroll-conservatively 100000000)
;;keep cursor at current position when scrolling
(setq scroll-preserve-screen-position t)
 
;;just type y/n instead of yes/no RET. this should be default
(fset 'yes-or-no-p 'y-or-n-p)


;;browse kill ring to look for forgotten copy/paste
(require 'browse-kill-ring)(global-set-key (kbd "C-c k") 'browse-kill-ring)



;--------raccourcis claviers en plus
(defun reload-config ()
  (interactive)
  (load-file "~/.emacs"))
(global-set-key "\C-x\C-r" 'reload-config)

;;C-< et M-< pour aller au début et à la fin d'un buffer
(global-set-key [(meta <)] 'beginning-of-buffer)
(global-set-key [(control <)] 'end-of-buffer)


;;I just want C-x k to kill the buffer instead of just prompting me
;;for it like ido does
(global-set-key (kbd "C-x k") (lambda ()
                                (interactive) (kill-buffer (current-buffer))))
;;like C-x k, but nicer :-)
(global-set-key (kbd "C-x l") 'bury-buffer)
;;could not live without
(global-set-key (kbd "M-q") 'backward-kill-word)

;;rebind previous M-q binding to M-s
(global-set-key (kbd "M-s") 'fill-paragraph)
;;nice to have, coherent with other keybindings, and bound to nothing
;;by default, so ...
(global-set-key (kbd "M-n") 'scroll-up)
(global-set-key (kbd "M-p") 'scroll-down)
;;M-g defaults to a prefix, I just rebind next/previous error and bind
;;M-g to goto
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-c n") 'next-error)
(global-set-key (kbd "C-c p") 'previous-error)
;;TODO : change c-mode horrible commenting method
(global-set-key (kbd "C-c u") 'uncomment-region)
(global-set-key (kbd "C-c c") 'comment-region)
;;handy, but buggy on terminals
(global-set-key (kbd "C-,") 'undo)

;;C-c left/right to undo/redo changes in window configuration
(winner-mode 1)

;;move between windows with meta-keypad
(windmove-default-keybindings 'meta)


;; pour éviter que ada-mode me pique le raccourci clavier
(add-hook 'ada-mode-hook (lambda ()
			   (local-unset-key (kbd "<C-tab>"))))

;;fast switching between two buffers
(global-set-key [\s-tab] (lambda ()
                           (interactive)
                           (switch-to-buffer (other-buffer))))
(global-set-key [\C-tab] 'next-buffer)
(global-set-key [\C-\S-iso-lefttab] 'previous-buffer)
;;make use of that useless key to do something useful. This can fail,
;;so protect
(condition-case err
    (global-set-key (kbd "²") (lambda () (interactive) (insert "\\")))
  (error
   (message "Failed to bind key to \\. Live with it.")))

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

(global-set-key (kbd "M-z") 'zap-up-to-char)

;;zap to isearch
(defun zap-to-isearch ()
  (interactive)
  (kill-region isearch-opoint isearch-other-end)
  (isearch-done)
  (if (> isearch-other-end isearch-opoint)
      (backward-word)
    (forward-word)))

(define-key isearch-mode-map (kbd "M-z") 'zap-to-isearch)

;;misc functions
(defun indent-whole-buffer ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;macros on alt-gr + some key
;;use M-x insert-kbd-macro to insert the definition of the macro
;;you just typed
(setq latex-macros
      '(
	;;F : frac
	("đ" "\\frac{}{}\C-b\C-b\C-b")
	;;Q : section
	("@" "\\section{}\C-b")
	;;S : subsection
	("ß" "\\subsection{}\C-b")
	;;D : subsubsection
	("ð" "\\subsubsection{}\C-b")
	;;C : cite
	("¢" "\\cite{}\C-b")
	;;R : ref
	("¶" "\\ref{}\C-b")
	;;E : end
	("€" "\\end{}\C-b")
	;;B : begin
	("”" "\\begin{}\C-b")
	;;I : item
	("→" "\\item ")
	))

(setq c-common-macros
      '(
	;;F : for
	("‘" [?f ?o ?r ?\( ?i ?  ?= ? ?0 ?\; ?  ?i ?  ?< ?  ?n
		 ?\; ?  ?i ?+ ?+ ?\) ?  ?\{ return tab return ?\} ?\C-p
		 ?\C-p ?\C-a ?\C-f ?\C-f ?\C-f ?\C-f ?\C-f ?\C-f ?\C-f
		 ?\C-f ?\C-f ?\C-f ?\C-f ?\C-f ?\C-f ?\C-f ?\C-f ?\C-d])
	))

(defun apply-macro-binding (list)
  "Takes a list ((key macro) ...) and binds macro to key.
key is any argument that can be given to global-set-key"
  (mapcar (lambda (el)
	    (local-set-key (nth 0 el) (nth 1 el)))
	  list))

(add-hook 'LaTeX-mode-hook
	  (lambda ()
	    (interactive)
	    (apply-macro-binding latex-macros)))
(add-hook 'c-mode-common-hook
	  (lambda ()
	    (interactive)
	    (apply-macro-binding c-common-macros)))

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)



;; widen window : widen selected window
;; (require 'widen-window)
;; (global-widen-window-mode t)
;; ;; et on ajoute les fonctions utilisées par windmove, comme ça on a tout ce qu'il faut!
;; (add-to-list 'ww-advised-functions 'windmove-up)
;; (add-to-list 'ww-advised-functions 'windmove-down)
;; (add-to-list 'ww-advised-functions 'windmove-right)
;; (add-to-list 'ww-advised-functions 'windmove-left)




;;;;;;;;;;;;;
;; ERCCCCC ;;
;;;;;;;;;;;;;

(require 'erc)


;;--------------------
;; Helper functions
;;--------------------

;;notification
(setq do-not-disturb nil)
;;set this if you don't want to be disturbed by notifications
;;(setq do-not-disturb t)
(defun notify (title message)
  "Notify user by graphical display"
  (unless do-not-disturb
    (shell-command-to-string (format
			      "notify-send %s %s"
			      (shell-quote-argument (concat "" title))
			      (shell-quote-argument (concat "" message))))))

;;ERC tray. Needs tray_daemon, http://smeuuh.free.fr/tray_daemon/
;;defined in emacs_perso : list of regexps for which we don't blink
;;the tray icon
(setq erc-tray-inhibit-one-activation nil)
(setq erc-tray-ignored-channels nil)
(setq erc-tray-state nil)
(defun erc-tray-change-state-aux (arg)
  "Enables or disable blinking, depending on arg (non-nil or nil)"
  (unless (eq erc-tray-state arg)
    (shell-command-to-string
     (concat "echo " (if arg "B" "b") " > /tmp/tray_daemon_control"))
    (setq erc-tray-state arg)))
(defun erc-tray-change-state (arg)
  "Enables or disable blinking, depending on arg (t or nil).
Additional support for inhibiting one activation (quick hack)"
  (if erc-tray-inhibit-one-activation
      (setq erc-tray-inhibit-one-activation nil)
    (erc-tray-change-state-aux arg)))


;;--------------------
;;Settings
;;--------------------
(setq erc-modules '(autojoin button completion fill
			     irccontrols list
			     log match menu move-to-prompt
			     netsplit networks noncommands
			     readonly ring scrolltobottom
			     services stamp spelling track
			     autoaway truncate))

;;301 : "x is away"
;;305 306 : away messages
(setq ;;erc-hide-list '("301" "305" "306")
      erc-input-line-position -1
      erc-server-reconnect-attempts t
      erc-prompt ">"
      erc-minibuffer-ignored t
      erc-query-display 'buffer
      erc-auto-query 'bury
      erc-current-nick-highlight-type 'all
      erc-interpret-mirc-color t
      erc-log-channels-directory "~/.erclogs"
      erc-log-write-after-insert t
      erc-log-write-after-send t
      erc-log-file-coding-system 'utf-8
      erc-prompt-for-nickserv-password nil
      erc-prompt-for-password nil
      erc-track-enable-keybindings nil
      erc-track-exclude-server-buffer t
      erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "305" "306" "333" "353")
      erc-track-position-in-mode-line t
      erc-track-showcount t
      erc-track-switch-direction 'leastactive
      erc-track-visibility 'visible
      erc-timestamp-only-if-changed-flag nil
      erc-timestamp-format "%h %d %H:%M:%S "
      erc-insert-timestamp-function 'erc-insert-timestamp-left
      erc-insert-away-timestamp-function 'erc-insert-timestamp-left
      erc-hide-timestamps nil
      erc-pcomplete-order-nickname-completions t
      ;;erc-header-line-format "%n on %t (%m,%l) %o"
      erc-auto-discard-away nil
      erc-autoaway-idle-seconds (* 60 30)
      erc-autoaway-message "Away"
      erc-fill-variable 'erc-fill-static
      erc-fill-static-center 13
      erc-fill-column 10000000 ;; long lines are well displayed by emacs, no need to cut by hand
      erc-truncate-buffer-on-save t)

(add-to-list 'auto-coding-alist '("\\.erclogs/.*\\.log" . utf-8))

;;TODO: check if erc-add-scroll-to-bottom is well called...
;; (setq erc-mode-hook
;;       (cons erc-mode-hook
;; 	    '(erc-add-scroll-to-bottom ;; very important : keep prompt at the last line
;; 	      erc-move-to-prompt-setup
;; 	      pcomplete-erc-setup
;; 	      erc-munge-invisibility-spec
;; 	      erc-button-setup
;; 	      erc-setup-my-commands
;; 	      erc-imenu-setup)))

;;--------------------
;;Unread messages bar
;;--------------------
(eval-after-load 'erc-track
  '(progn
     (defun erc-bar-move-back (n)
       "Moves back n message lines. Ignores wrapping, and server messages."
       (interactive "nHow many lines ? ")
       (re-search-backward "^.*<.*>" nil t n))

     (defun erc-bar-update-overlay ()
       "Update the overlay for current buffer, based on the content of
erc-modified-channels-alist. Should be executed on window change."
       (interactive)
       (let* ((info (assq (current-buffer) erc-modified-channels-alist))
	      (count (cadr info)))
	 (if (and info (> count erc-bar-threshold))
	     (save-excursion
	       (end-of-buffer)
	       (when (erc-bar-move-back count)
		 (let ((inhibit-field-text-motion t))
		   (move-overlay erc-bar-overlay
				 (line-beginning-position)
				 (line-end-position)
				 (current-buffer)))))
	   (delete-overlay erc-bar-overlay))))

     (defvar erc-bar-threshold 1
       "Display bar when there are more than erc-bar-threshold unread messages.")
     (defvar erc-bar-overlay nil
       "Overlay used to set bar")
     (setq erc-bar-overlay (make-overlay 0 0))
     (overlay-put erc-bar-overlay 'face '(:underline "black"))
     ;;put the hook before erc-modified-channels-update
     (defadvice erc-track-mode (after erc-bar-setup-hook
				      (&rest args) activate)
       ;;remove and add, so we know it's in the first place
       (remove-hook 'window-configuration-change-hook 'erc-bar-update-overlay)
       (add-hook 'window-configuration-change-hook 'erc-bar-update-overlay))
     (add-hook 'erc-send-completed-hook (lambda (str)
					  (erc-bar-update-overlay)))))


;;--------------------
;;channel change commands
;;--------------------
(defun irc-dwim (arg)
  "Runs IRC (by function irc, to be written for your particular servers)
  if it is not running, use erc-track to switch to last modified
  chan if it is."
  (interactive "p")
  (require 'erc-track)
  (if (erc-buffer-list)
      (my-track-switch-buffer arg)
    (irc)))
(global-set-key [f9] 'irc-dwim)

(defun my-track-switch-buffer (arg)
  "If there are unread messages, switch to them. Else, switch to latest seen non-erc buffer.
Differs a bit from erc's implementation : robust to buffer kills and stuff like that"
  (interactive "p")
  (if erc-modified-channels-alist
      (erc-track-switch-buffer arg)
    (let ((blist (buffer-list)))
      (while blist
	(unless (or (eq 'erc-mode (buffer-local-value 'major-mode (car blist)))
		    (minibufferp (car blist)))
	  (switch-to-buffer (car blist))
	  (setq blist nil))
	(setq blist (cdr blist))))))


;;--------------------
;; Tray control
;;--------------------
;; (defun erc-buffer-visible (buffer)
;;   "Returns nil when the buffer `buffer' is not visible.
;; Currently returns nil when the selected window does not display the buffer."
;;   ;;(not (eq t (frame-visible-p (selected-frame)))))
;;   ;;TODO: need to parse the window-tree and return nil when the buffer is not displayed in any window
;;   (eq (get-buffer-window buffer) (selected-window)))
;;useless : erc-buffer-visible already exists... to test
(defun erc-tray-update-state ()
  "Update the state of the tray icon. Blink when some new event
appears when you're not looking. Events are changes to
erc-modified-channels-alist, filtered by erc-tray-ignored-channels."
  (interactive)
  ;;stop blinking tray when there're no channels in list
  (unless erc-modified-channels-alist
    (erc-tray-change-state nil))
  ;;maybe make tray blink
  ;;filter list according to erc-tray-ignored-channels, 
  ;;and to displayed buffers
  (let ((filtered-list erc-modified-channels-alist))
    (mapc (lambda (el)
	    (mapc (lambda (reg)
		    (when (string-match reg (buffer-name (car el)))
		      (setq filtered-list
			    (remove el filtered-list))))
		  erc-tray-ignored-channels)
	    (when (erc-buffer-visible (car el))
	      (setq filtered-list
		    (remove el filtered-list))))
	  filtered-list)
    (when filtered-list
      (erc-tray-change-state t))))

;;blink if away and activity
(add-hook 'erc-track-list-changed-hook 'erc-tray-update-state)

;;stop blinking whenever frame is set visible
(add-hook 'erc-mode-hook (lambda ()
			   (interactive)
			   (define-key special-event-map [make-frame-visible]
			     (lambda () (interactive)
			       (erc-bar-update-overlay)
			       (erc-tray-change-state nil)
			       (erc-modified-channels-update)))))


;;--------------------
;; Notification control
;;--------------------
(defun erc-notify-if-hl (matched-type nick msg)
  "Notify whenever someone highlights you and you're away"
  (when (and (eq matched-type 'current-nick)
	     (not (erc-buffer-visible nick)))
    (notify (format "HL de %s" (erc-extract-nick nick))
	    msg)))
;;notify if away and highlighted
(add-hook 'erc-text-matched-hook 'erc-notify-if-hl)


;; not used yet
(defun my-notify-JOIN (proc parsed)
  "Display notification of user connections on bitlbee"
  (let ((nick (erc-extract-nick (erc-response.sender parsed)))
	(chan (erc-response.contents parsed)))
    (when (string= chan "&bitlbee")
      (notify nick "Connexion MSN")))
  nil)
;;notify if someone joins on bitlbee
(add-hook 'erc-server-JOIN-functions 'my-notify-JOIN)

(defun my-notify-PRIVMSG (proc parsed)
  "Popup whenever someone privmsgs you and you're not seeing it"
  (let ((nick (car (erc-parse-user (erc-response.sender parsed))))
	(target (car (erc-response.command-args parsed)))
	(msg (erc-response.contents parsed)))
    (when (and (string= target (erc-current-nick))
	       (not (erc-buffer-visible nick))
	       (not (erc-is-message-ctcp-and-not-action-p msg)))
      ;;prevents from blinking on messages for which there is already
      ;;a notification
      ;; (setq erc-tray-inhibit-one-activation t)
      (notify (format "PM de %s" nick)
	      msg)))
  nil)
;;notify if away and pmed
(add-hook 'erc-server-PRIVMSG-functions 'my-notify-PRIVMSG)


;;--------------------
;;notify in query buffers when someone appears/disappears
;;--------------------
(erc-define-catalog
 'english
 '((my_notify_join      . "%n is back")
   (my_notify_quit      . "%n is gone")))

(defun my-notify-in-privmsg-JOIN (proc parsed)
  (let* ((nick (erc-extract-nick (erc-response.sender parsed)))
	 (buff (erc-get-buffer nick proc)))
    (when buff
      (erc-display-message
       parsed 'notice buff
       'my_notify_join ?n nick)))
  nil)
(add-hook 'erc-server-JOIN-functions 'my-notify-in-privmsg-JOIN)

(defun my-notify-in-privmsg-QUIT (proc parsed)
  (let* ((nick (erc-extract-nick (erc-response.sender parsed)))
	 (buff (erc-get-buffer nick proc)))
    (when buff
      (erc-display-message
       parsed 'notice buff
       'my_notify_quit ?n nick)))
  nil)
(add-hook 'erc-server-QUIT-functions 'my-notify-in-privmsg-QUIT)


;;--------------------
;;prompts for commands
;;--------------------
(defun erc-query-prompt ()
  "Prompts for someone to query"
  (interactive)
  (let ((completion-ignore-case t))
    (let ((server (erc-server-buffer))
	  (target (completing-read "Query sur: "
				   (erc-get-server-nickname-alist)
				   nil ;;no predicate, require match
				   t)))
      (erc-query target server))))
(defun erc-whois-prompt ()
  "Prompt for someone to do whois on"
  (interactive)
  (let ((completion-ignore-case t))
    (let ((target (completing-read "Whois sur: "
				   (erc-get-server-nickname-alist)
				   nil ;;no predicate, require match
				   t)))
      (erc-cmd-WHOIS target))))

(defun erc-names-prompt ()
  "Get names of channel, either using /names or blist if using bitlbee"
  (interactive)
  (if (string-match "&bitlbee" (buffer-name))
      (erc-send-message "root: blist")
    (erc-channel-names)))

;;--------------------
;; Setting away
;;--------------------
(require 'erc-autoaway)
(defun erc-toggle-away ()
  "Toggles away status in ERC."
  (interactive)
  (if (erc-away-time)
      (erc-autoaway-set-back)
    (erc-autoaway-set-away erc-autoaway-idle-seconds)))

;;--------------------
;; Toggle tracking
;;--------------------
(defvar erc-track-exclude '())
(defun toggle-channel-track ()
  "Toggle exclude status of current channel"
  (interactive)
  (let ((name (buffer-name (current-buffer))))
    (if (member name
		erc-track-exclude)
	(progn
	  (setq erc-track-exclude (remove name erc-track-exclude))
	  (message "Tracking on"))
      (progn
	(add-to-list 'erc-track-exclude name)
	(message "Tracking off")))))


;;--------------------
;; browse url before point with just a keystroke
;;--------------------
(require 'thingatpt)
(defun browse-url-before-point ()
  (interactive)
  (save-excursion
    (save-match-data
      (if (re-search-backward thing-at-point-url-regexp 0 t)
	  (browse-url (match-string 0))
	(message "Pas d'URL dans le buffer")))))

;;--------------------
;; logs & view logs
;;--------------------

(defun erc-generate-log-file-name-like-xchat (buffer target nick server port)
  "Generates a log-file name like one generated by xchat.
This results in a file name of the form network-(#channel|nick).log.
This function is a possible value for `erc-generate-log-file-name-function'."
  (require 'erc-networks)
  (let* ((network (or (with-current-buffer buffer (erc-network-name)) server))
	 (file (concat
		network
		"-"
		(or target network)
		".log")))
    ;; we need a make-safe-file-name function.
    (convert-standard-filename file)))
(setq erc-generate-log-file-name-function 'erc-generate-log-file-name-like-xchat)

(defun erc-browse-log ()
  (interactive)
  (find-file (erc-current-logfile))
  (end-of-buffer))


;;--------------------
;; Setup keys
;;--------------------
(defun erc-setup-my-commands ()
  (interactive)
  (global-set-key [escape] 'irc-dwim)
  (local-set-key (kbd "C-c C-a") 'erc-toggle-away)
  (local-set-key (kbd "C-c C-u") 'browse-url-before-point)
  (local-set-key (kbd "C-c C-q") 'erc-query-prompt)
  (local-set-key (kbd "C-c C-n") 'erc-names-prompt)
  (local-set-key (kbd "C-c C-w") 'erc-whois-prompt)
  (local-set-key (kbd "C-c C-l") 'erc-browse-log))
(add-hook 'erc-mode-hook 'erc-setup-my-commands)

;; I don't know why, something messes up with erc-bol, so I'm redefining it
(defun erc-bol ()
  "Move `point' to the beginning of the current line.

This places `point' just after the prompt, or at the beginning of the line."
  (interactive)
  ;;was (forward-line 0)
  (beginning-of-line)
  (when (get-text-property (point) 'erc-prompt)
    (goto-char erc-input-marker))
  (point))


;;read personal info (ERC stuff)
(load-file "~/.emacs.d/perso.el")

;;--------------------
;; ERC end
;;--------------------






;;Highlights spaces at the end of lines
(setq show-trailing-whitespace t)


;; un peu de mercurial 
;;(require 'mercurial)


;; kde

;;cmake
; Add cmake listfile names to the mode list.
(setq auto-mode-alist
	  (append
	   '(("CMakeLists\\.txt\\'" . cmake-mode))
	   '(("\\.cmake\\'" . cmake-mode))
	   auto-mode-alist))

(autoload 'cmake-mode "~/.emacs.d/cmake-mode.el" t)



;; Join this line to previous and fix up whitespace at join.
(global-set-key (kbd "C-c j") 'join-line)


;;--------------------
;; Auto Completion
;;--------------------


;; auto-complete mode : dropdown menu
;; see http://cx4a.org/software/auto-complete/manual.html
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete/ac-dict")
(setq ac-comphist-file "~/.emacs.d/auto-complete/ac-comphist.dat")
(ac-config-default)
(setq ac-delay 0.2)

;;(add-hook 'c++-mode (lambda () (add-to-list 'ac-sources 'ac-source-semantic)))
(add-hook 'shell-mode-hook (lambda () (setq ac-sources 'ac-source-files-in-current-dir)))
(add-to-list 'ac-modes 'shell-mode)

;;etags for auto-complete
(require 'auto-complete-etags)
(add-hook 'c++-mode (lambda () (add-to-list 'ac-sources 'ac-source-etags)))
(add-hook 'c-mode (lambda () (add-to-list 'ac-sources 'ac-source-etags)))


;;--------------------
;; Window management
;;--------------------

;;
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; Cancel and redo windows configurations
;; allows to "undo" (and "redo") changes in the window configuration with the key commands 'C-c left' and 'C-c right'
(require 'winner)
(setq winner-dont-bind-my-keys t) ;; default bindings conflict with org-mode

(global-set-key (kbd "<C-s-left>") 'winner-undo)
(global-set-key (kbd "<C-s-right>") 'winner-redo)
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


;; uniquify.el is a helper routine to help give buffer names a better unique name.
(when (load "uniquify" 'NOERROR)
  (require 'uniquify)
  (setq uniquify-buffer-name-style 'forward)
  ;(setq uniquify-buffer-name-style 'post-forward)
  )



;;--------------------
;; Shells in emacs
;;--------------------

(setq comint-scroll-to-bottom-on-input 'all)
(setq comint-move-point-for-output t)

;;shell

;; clean
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



;;--------------------
;; org-mode
;;--------------------

(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
;; (add-hook 'org-mode-hook 'org-indent-mode)
(require 'remember)
(add-hook 'remember-mode-hook 'org-remember-apply-template)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c r") 'remember)
					;bindings
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
					;settings
(setq
 org-agenda-files (list "~/.emacs.d/org/todo.org")
 org-default-notes-file "~/.emacs.d/org/notes.org"
 org-log-done 'time
)
(add-hook 'org-load-hook
	  (lambda ()
	    (setq shift-select-mode nil) ;; to modify dates
	    )
)


(setq org-clock-persist 'history)
     (org-clock-persistence-insinuate)


;;--------------------
;; session management
;;--------------------

;;save the minibuffer input
(savehist-mode 1)

;;save last edit place in files
(require 'saveplace)
(setq-default save-place t)


;;;;;;;;;;;
;; TESTS ;;
;;;;;;;;;;;


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
