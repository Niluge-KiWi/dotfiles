;; -*- coding: utf-8 -*-

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(global-font-lock-mode t)
 '(inhibit-startup-echo-area-message (user-login-name))
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(menu-bar-mode nil)
 '(scroll-bar-mode nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(transient-mark-mode nil))

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
(add-to-list 'load-path (expand-file-name "~/.emacs.d/erc-5.3/"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Package Management (ELPA)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))



;; default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" system-name))


;; Color Themes
;;(require 'color-theme)
;;(color-theme-initialize)
(require 'zenburn)
(color-theme-zenburn)



;; font
(setq font-use-system-font t) ;; since emacs 23.2
;;old way:
;; do this in shell:
;;echo "Emacs.font: Monospace-10" >> ~/.Xresources
;;xrdb -merge ~/.Xresources


;;byte-recompile elisp files if they need to be
;; (byte-recompile-directory "~/.emacs.d" 0)
;; (kill-buffer "*Compile-Log*")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interactively Do Things
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;-------ido
;; interactively do things with buffers and files.
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

;;-------icomplete
;; completion for commands that don't use ido (like help)
(icomplete-mode 1)

;;-------smex
;; super M-x : ido + frequency
(require 'smex)
(setq smex-history-length 32)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Word wrapping
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pour gérer les lignes trop longues
;; amazing new variable in e23. No need to worry about longlines any more
(setq-default word-wrap t)
;; ... but still use ll sometimes for reading dense text
(defalias 'll 'longlines-mode)


;; Se limiter à des lignes de 80 caractères dans les modes textes (y
;; compris le mode LaTeX) :
;; cf. http://www-verimag.imag.fr/~moy/emacs/#autofill
(add-hook 'text-mode-hook 'turn-on-auto-fill)




;; ess Emacs Speaks Statistics
;;(require 'ess-site)

;; sqlplus mode pour oracle
;; (require 'sqlplus)
;; (add-to-list 'auto-mode-alist '("\\.sqp\\'" . sqlplus-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Diff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;-------ediff mode
;;vertical split (terminology is confusing)
(setq ediff-split-window-function 'split-window-horizontally)
;;no separate frame
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; this is a huge hack


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

;;-------YAML mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(add-hook 'yaml-mode-hook
	  '(lambda ()
	     (define-key yaml-mode-map "\C-m" 'newline-and-indent)))
(add-hook 'yaml-mode-hook
	  '(lambda ()
	     (setq indent-tabs-mode nil)
	     (setq c-indent-level 2)))

;;-------apache mode
(autoload 'apache-mode "apache-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.htaccess\\'"   . apache-mode))
(add-to-list 'auto-mode-alist '("httpd\\.conf\\'"  . apache-mode))
(add-to-list 'auto-mode-alist '("srm\\.conf\\'"    . apache-mode))
(add-to-list 'auto-mode-alist '("access\\.conf\\'" . apache-mode))
(add-to-list 'auto-mode-alist '("sites-\\(available\\|enabled\\)/" . apache-mode))



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
      ;; start flyspell
      (progn
	(flyspell-mode t)
	(ispell-change-dictionary language)
	(flyspell-buffer)))))



;;autopair
(require 'autopair)
(autopair-global-mode) ;; enable autopair in all buffers

(setq autopair-blink nil) ;; no blink
(setq autopair-autowrap t) ;; wrap region with character to insert

(add-hook 'erc-mode-hook
	  (lambda () (setq autopair-dont-activate t)))
(add-hook 'LaTeX-mode-hook
          (lambda ()
	    (modify-syntax-entry ?$ "\"")))


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



;;-------raccourcis claviers en plus
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ERC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load "~/.emacs.d/erc.el")
;;read personal info (ERC stuff)
(load "~/.emacs.d/perso.el" t)






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




;;--------------------
;; Window management
;;--------------------

;;move between windows with meta-keypad
(windmove-default-keybindings 'meta)


;; resize windows
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


;;--------------------
;; divers
;;--------------------

;; pastebin from emacs
(require 'pastebin)

;; iBuffer : dired for buffers
(require 'ibuffer)
(setq ibuffer-default-sorting-mode 'recency)

(setq ibuffer-always-show-last-buffer t)
(setq ibuffer-view-ibuffer t)

(global-set-key (kbd "C-x C-b") 'ibuffer-other-window)



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
