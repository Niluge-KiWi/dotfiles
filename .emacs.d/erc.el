(require 'erc)
;;; ERC conf of Thomas Riccardi. Homepage : http://github.com/Niluge-KiWi/dotfiles

;; Can be viewed in outline mode


;;;--------------------
;;; Modify erc-track-mode
;;;--------------------

(defun patch-erc-track-mode ()
  "Add erc-track-visibility 'focus that uses my buffer-focus-p:
  handles X focus"

  (defcustom erc-track-visibility t
	"Where do we look for buffers to determine their visibility?
The value of this variable determines, when a buffer is considered
visible or invisible.  New messages in invisible buffers are tracked,
while switching to visible buffers when they are tracked removes them
from the list.  See also `erc-track-when-inactive'.

Possible values are:

t                - all frames
visible          - all visible frames
nil              - only the selected frame
selected-visible - only the selected frame if it is visible
focus            - check if buffer has emacs focus and X focus

Activity means that there was no user input in the last 10 seconds."
	:group 'erc-track
	:type  '(choice (const :tag "All frames" t)
					(const :tag "All visible frames" visible)
					(const :tag "Only the selected frame" nil)
					(const :tag "Only the selected frame if it was active" selected-visible)
					(const :tag "Emacs and X focus for the buffer" focus)))


  (defun erc-track-get-buffer-window (buffer frame-param)
	(cond ((eq frame-param 'focus)
		   (if (buffer-focus-p buffer)
			   (get-buffer-window buffer t)
			 nil))
		  ((eq frame-param 'selected-visible)
		   (if (eq (frame-visible-p (selected-frame)) t)
			   (get-buffer-window buffer nil)
			 nil))
		  (t
		   (get-buffer-window buffer frame-param)))))

;; (eval-after-load "erc-track"
;; ;;  (message (format "test %s" (featurep 'erc-track))))
;;   (patch-erc-track-mode))
;; above code doesn't work: it prints "test nil" so the redefinition doesn't work either
(require 'erc-track)
(patch-erc-track-mode)

;;;--------------------
;;; Settings
;;;--------------------
; specific settings for IM gateways : minbif or bitlbee
(setq im-gateway-channel-name "&friends")
; erc general conf
(setq erc-modules '(autojoin button completion
			     irccontrols list
			     log match menu move-to-prompt
			     netsplit networks noncommands
			     readonly ring scrolltobottom
			     services stamp spelling track
			     autoaway truncate))

;;301 : "x is away"
;;305 306 : away messages
(setq ;;erc-hide-list '("301" "305" "306" "324" "329" "333")
      erc-input-line-position -1
      erc-server-reconnect-attempts 2
      erc-server-send-ping-timeout 30
      erc-prompt ">"
      erc-minibuffer-ignored t
      erc-query-display 'buffer
	  erc-join-buffer 'bury
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
      ;;301 : "x is away"
      ;;305 306 : away messages
      ;;329 : chan created on
      ;;324 : chan modes
      ;;333 : X set the topic
	  ;;332 : welcome/topic messages
      erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "305" "306" "333" "353" "324" "329" "MODE" "TOPIC" "332")
      erc-track-position-in-mode-line t
      erc-track-showcount t
      erc-track-switch-direction 'leastactive
      erc-track-visibility 'focus
      ;; only fontify indicator on HLs
      erc-track-faces-priority-list
      '((erc-nick-default-face erc-current-nick-face)
	erc-current-nick-face erc-keyword-face
	erc-default-face)
      erc-timestamp-only-if-changed-flag nil
      erc-timestamp-format "%h %d %H:%M:%S "
      erc-pcomplete-order-nickname-completions t
      erc-hide-timestamps nil
      erc-insert-timestamp-function 'erc-insert-timestamp-left
      erc-insert-away-timestamp-function 'erc-insert-timestamp-left
      erc-header-line-format "%a%n on %t (%m,%l) %N %o"
      erc-auto-discard-away t
      erc-autoaway-idle-seconds (* 60 30)
      erc-autoaway-message "Away"
      erc-truncate-buffer-on-save t
      )


;;;--------------------
;;; Auto completion
;;;--------------------
;;-------nicks then dabbrev
;; modify return value of erc-pcomplete: return t if completed
;; something, nil otherwise
(require 'erc-pcomplete)
(defun erc-pcomplete ()
  "Complete the nick before point."
  (interactive)
  (let ((pointbefore (point)))
    (when (> (point) (erc-beg-of-input-line))
      (let ((last-command (if (eq last-command 'erc-complete-word)
			      'pcomplete
			    last-command)))
	(call-interactively 'pcomplete)
	(if (> (point) pointbefore)
	    t
	  nil)))))
(setq erc-complete-functions '(erc-pcomplete my-dabbrev-expand))

;; TODO setup a source for auto-complete?


;;;--------------------
;;; Colorize nicks
;;;--------------------
(require 'erc-nick-color)
(add-hook 'erc-insert-modify-hook 'erc-put-color-on-nick)
;; special colors for some people
(setq erc-nick-color-alist '(("Nick1" . "blue")
			     ("Nick2" . "green")
			     ))


;;;--------------------
;;; Logs
;;;--------------------
;;-------colors
(require 'erc-view-log)
(setq erc-view-log-nickname-face-function 'erc-get-face-for-nick)
(add-to-list 'auto-mode-alist '("\\.erclogs/.*\\.log" . erc-view-log-mode))

(add-to-list 'auto-coding-alist '("\\.erclogs/.*\\.log" . utf-8))

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

;;;--------------------
;;; Unread messages bar
;;;--------------------
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


;;;--------------------
;;; Easier navigation
;;;--------------------
(defun erc-goto-last ()
  "Set cursor to last read line."
  (interactive)
  (when (eq 'erc-mode (buffer-local-value 'major-mode (current-buffer)))
	(let ((last-read-pos (overlay-start erc-bar-overlay)))
	  (if last-read-pos
		  (goto-char last-read-pos)
		(goto-char erc-input-marker)))))

(define-key erc-mode-map (kbd "C-c C-c") 'erc-goto-last)


;;;--------------------
;;; Connect
;;;--------------------
(defun assoc-regexp (key list)
  "Like assoc-string, but returns whenever there is a match"
  (if (null list) nil
    (if (string-match (caar list) key)
	(car list)
      (assoc-regexp key (cdr list)))))

(defvar my-irc-server-nick-alist '()
  "alist irc server nick")
(defvar my-irc-server-port-alist '()
  "alist irc server port")
(defvar my-irc-server-pass-alist '()
  "alist irc server pass")
(defvar my-irc-servers '()
  "list irc server")

(defun get-nick-for-server (serv)
  (or (cadr (assoc-regexp serv my-irc-server-nick-alist))
      erc-nick))
(defun get-port-for-server (serv)
  (or (cadr (assoc-regexp serv my-irc-server-port-alist))
      6667))
(defun get-pass-for-server (serv)
  (or (cadr (assoc-regexp serv my-irc-server-pass-alist))
      ""))

(defun irc ()
  "Connect to IRC."
  (interactive)
  (mapcar (lambda (x) (erc :server x :port (get-port-for-server x) :nick (get-nick-for-server x) :password (get-pass-for-server x))) my-irc-servers))

(defun irc-deco ()
  "Kill all server buffers"
  (interactive)
  (mapcar (lambda (x) (kill-buffer (format "%s:%d" x (get-port-for-server x)))) my-irc-servers))


(defun irc-reco ()
  "Kill all server buffers, and connect again"
  (interactive)
  (irc-deco)
  (irc))

;;;--------------------
;;; Channel change commands
;;;--------------------
(defun irc-dwim (arg)
  "Runs IRC (by function irc) if it is not running,
  use erc-track to switch to last modified chan if it is."
  (interactive "p")
  (require 'erc-track)
  (if (erc-channel-list nil)
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

(defvar erc-track-default-switch-buffer nil
  "The buffer name to switch to when there is no more modified
  ERC buffer.")
(defun my-track-switch-buffer-default (arg)
  "If there are unread messages, switch to them. Else, switch to erc-default-switch-buffer."
  (interactive "p")
  (if erc-modified-channels-alist
	  ;; jump to the next active channel
	  (switch-to-buffer (erc-track-get-active-buffer arg))
	;; if no active channels, switch to default channel
	(switch-to-buffer erc-track-default-switch-buffer)))
(global-set-key [f10] 'my-track-switch-buffer-default)


;;;--------------------
;;; Switch ERC buffers, with ido
;;;--------------------
(defun erc-ido-switch-buffer (&optional arg)
  "Use `ido-completing-read' to switch between ERC buffers."
  (interactive)
  (eval-when-compile
    (require 'ido))
  (let ((buffer-list (mapcar 'buffer-name (erc-buffer-list))))
	(switch-to-buffer (ido-completing-read
					   "ERC Buffer: "
					   buffer-list
					   nil t))))
;; TODO set-key for erc-ido-switch-buffer


;;;--------------------
;;; Tray control
;;;--------------------
(defun erc-tray-update-state ()
  "Update the state of the tray icon.
Red if new event new event appears when you're not
looking. Events are changes to erc-modified-channels-alist.
Blinking, if in erc-tray-blink-channels."
  (interactive)
  ;;stop blinking tray when there're no channels in list
  (unless erc-modified-channels-alist
    (erc-tray-change-state 'default))
  ;;maybe make tray blink-
  ;;filter list according to erc-tray-ignored-channels
  (let ((modified-notify-list erc-modified-channels-alist)
		(modified-blink-list '()))
	(mapc (lambda (el)
			(let* ((buf (car el))
				   (focus (buffer-focus-p buf)))
			  ;; remove buf from modified-notify-list if channel has the focus
			  (if focus
				  (setq modified-notify-list (remove el modified-notify-list))
				;; no focus buffers
				;; remove buf from modified-notify-list if channel ignored
				(mapc (lambda (reg)
						(when (string-match reg (buffer-name buf)
											(setq modified-notify-list
												  (remove el modified-notify-list)))))
						erc-tray-ignored-channels)
				;; add buff to modified-blink-list if channel can blink
				(mapc (lambda (reg)
						(when (string-match reg (buffer-name buf))
						  (setq modified-blink-list 
								(cons el modified-blink-list))))
					  erc-tray-blink-channels))))
		  erc-modified-channels-alist)
	(erc-tray-change-state
	 (cond ((car modified-blink-list) 'blink)
		   ((car modified-notify-list) 'red)
		   (t 'default)))))


;;blink if away and activity
(add-hook 'erc-track-list-changed-hook 'erc-tray-update-state)


;;;--------------------
;;; Notification control
;;;--------------------
(defun erc-notify-if-hl (matched-type nick msg)
  "Notify whenever someone highlights you and you're away"
  (when (and (eq matched-type 'current-nick)
	     (not (buffer-focus-p)))
    (notify (format "HL \<%s\>" (erc-extract-nick nick)) msg)))
;;notify if away and highlighted
(add-hook 'erc-text-matched-hook 'erc-notify-if-hl)


;; not used yet
(defun my-notify-JOIN (proc parsed)
  "Display notification of user connections on bitlbee"
  (let ((nick (erc-extract-nick (erc-response.sender parsed)))
	(chan (erc-response.contents parsed)))
    (when (string= chan im-gateway-channel-name)
      (notify (format "%s s'est connecté" nick))))
  nil)
;;notify if someone joins on bitlbee
;(add-hook 'erc-server-JOIN-functions 'my-notify-JOIN)

(defun my-notify-PRIVMSG (proc parsed)
  "Popup whenever someone privmsgs you and you're not seeing it"
  (let ((nick (car (erc-parse-user (erc-response.sender parsed))))
	(target (car (erc-response.command-args parsed)))
	(msg (erc-response.contents parsed)))

    (when (and (string= target (erc-current-nick))
	       (not (buffer-focus-p))
	       (not (erc-is-message-ctcp-and-not-action-p msg)))
      (notify (format "PM \<%s\>" nick) msg)))
  nil)
;;notify if away and pmed
(add-hook 'erc-server-PRIVMSG-functions 'my-notify-PRIVMSG)


;;;--------------------
;;; Notify in query buffers when someone appears/disappears
;;;--------------------
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


;;;--------------------
;;; Prompts for commands
;;;--------------------
(defun erc-query-prompt ()
  "Prompts for someone to query"
  (interactive)
  (let ((completion-ignore-case t))
    (let ((server (erc-server-buffer))
	  (target (completing-read "Query sur: "
				   (erc-get-server-nickname-alist)
				   nil ;;no predicate, require match
				   t))
	  (erc-join-buffer 'buffer))
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
  (if (or (string-match im-gateway-channel-name (buffer-name))
	  (string-match "&bitlbee" (buffer-name)))
      (erc-send-message "root: blist")
    (erc-channel-names)))

;;;--------------------
;;; Setting away
;;;--------------------
(require 'erc-autoaway)
(defun erc-toggle-away ()
  "Toggles away status in ERC."
  (interactive)
  (if (erc-away-time)
      (erc-autoaway-set-back)
    (erc-autoaway-set-away erc-autoaway-idle-seconds)))

;;;--------------------
;;; Toggle tracking
;;;--------------------
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


;;;--------------------
;;; Browse url before point with just a keystroke
;;;--------------------
(require 'thingatpt)
(defun browse-url-before-point ()
  (interactive)
  (save-excursion
    (save-match-data
      (if (re-search-backward thing-at-point-url-regexp 0 t)
	  (browse-url (match-string 0))
	(message "Pas d'URL dans le buffer")))))


;;;--------------------
;;; Setup keys
;;;--------------------
(defun erc-setup-my-commands ()
  (interactive)
  (global-set-key [escape] 'irc-dwim)
  (global-set-key (kbd "C-c C-&") (lambda () (interactive) (switch-to-buffer im-gateway-channel-name)))
  (local-set-key (kbd "C-c C-a") 'erc-toggle-away)
  (local-set-key (kbd "C-c C-u") 'browse-url-before-point)
  (local-set-key (kbd "C-c C-g") 'google-search-region)
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


;;;--------------------
;;; System tray
;;;--------------------
;;-------ERC tray
;; Needs tray_daemon, http://smeuuh.free.fr/tray_daemon/
;; set in emacs_perso:
(defvar erc-tray-ignored-channels nil
  "List of regexps for which we don't modify the tray icon.")
(defvar erc-tray-blink-channels nil
  "List of regexps for which we blink the tray icon.")
(defvar erc-tray-enable t
  "Enable tray notifications if non nil.")
(defun erc-tray-change-state-aux (arg)
  "Change the state of tray_daemon, depending on arg (D: default, R: red, B: blinking)"
  (shell-command-to-string
   (concat "echo " arg " > /tmp/tray_daemon_control")))
(defun erc-tray-change-state (arg)
  "Enables or disable blinking, depending on arg (default, red, blinking)."
  (when erc-tray-enable
	(erc-tray-change-state-aux (case arg
								 ('default "D")
								 ('red "R")
								 ('blink "B")))))


;;;--------------------
;;; Hide passwords
;;;--------------------
(defun erc-dont-display-passwords ()
  "Hide passwords when login.
Use this defun with `erc-insert-modify-hook'."
  (save-excursion
    (goto-char (point-min))
	(while (re-search-forward "/msg NickServ identify \\(.*\\)$" nil t)
	  (replace-match "********" nil t nil 1))))

(add-hook 'erc-send-modify-hook 'erc-dont-display-passwords)

;;;--------------------
;;; SPC DWIM scroll & insert SPC
;;;--------------------
(defun my-erc-space-dwim ()
  "SPC DWIM:
- scrolls up if we are not at the bottom of the buffer,
- else insert SPC."
  (interactive)
  (condition-case nil (insert " ")
    (text-read-only
     (condition-case nil (scroll-up)
       (end-of-buffer (recenter 0))))))
(define-key erc-mode-map (kbd "SPC") 'my-erc-space-dwim)
