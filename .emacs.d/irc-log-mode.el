;;; irc-log-mode.el --- Minor mode for viewing irc logs

;; Copyright (C) 2010 Antoine Levitt
;; Copyright (C) 2010 Thomas Riccardi

;; Author: Antoine Levitt
;;         Thomas Riccardi <riccardi.thomas@gmail.com>
;; Keywords: irc erc logs colors

;; This program is free software. It comes without any warranty, to
;; the extent permitted by applicable law. You can redistribute it
;; and/or modify it under the terms of the Do What The Fuck You Want
;; To Public License, Version 2, as published by Sam Hocevar. See
;; http://sam.zoy.org/wtfpl/COPYING for more details.

;;; Commentary:
;; Set colors on an irc/erc log file

;;; Installation:
;;    (require 'irc-log-mode)
;;    (add-to-list 'auto-mode-alist '("\\.erclogs/.*\\.log" . irc-log-mode))

;;; Options:
;; Faces for: timestamp, wrap nickname (<>), nickname, message, notice.
;; For nickname: can be a function that takes the nickname as argument, and returns a face

;;; TODO:
;; - handle \me messages: erc-action-face
;; - handle commands like "'timestamp' > /whois nickname"" : erc-command-indicator-face and erc-prompt-face
;; - handle priv messages?: erc-direct-msg-face
;; - handle own message?: erc-input-face (use erc-nick or server-nick-alist?)
;; - handle own nick?: erc-my-nick-face (use erc-nick or server-nick-alist?)
;; - handle nick for private messages?: erc-nick-msg-face
;; - handle "*** Users on"?: erc-current-nick-face

(require 'erc)
(require 'erc-nick-color)

(defgroup irc-log nil
  "IRC log mode"
  :group 'languages
  :prefix "irc-log-")

(defcustom irc-log-timestamp-face
  'erc-timestamp-face
  "Face for timestamps."
  :type 'face
  :group 'irc-log-faces)

(defcustom irc-log-wrap-nickname-face
  'erc-default
  "Face for nicknames wrappers <>."
  :type 'face
  :group 'irc-log-faces)

(defcustom irc-log-nickname-face
  ;;'erc-nick-default-face
  'erc-get-face-for-nick
  "Face for nicknames."
  :type '(choice (face :tag "A face")
		 (function :tag "A function that returns a face, nick as argument"))
  :group 'irc-log-faces)

(defcustom irc-log-message-face
  'erc-default-face
  "Face for messages."
  :type 'face
  :group 'irc-log-faces)

(defcustom irc-log-notice-face
  'erc-notice-face
  "Face for notices."
  :type 'face
  :group 'irc-log-faces)




(defun erc-log-nick-get-face ()
  "Returns a face for the matched nick, given "
  (if (facep irc-log-nickname-face)
      irc-log-nickname-face
    (apply irc-log-nickname-face (list (match-string 1)))))



;; warning: only works if erc-timestamp-format doesn't contains the character '<'
(setq irc-log-keywords
      (list
       ;; first regexp apply the face
       `(,(format "^\\(.*\\) \\(<\\)%s\\(>\\) \\(.*\\)$" erc-valid-nick-regexp)
	 (1 irc-log-timestamp-face)
	 (2 irc-log-wrap-nickname-face)
	 (3 irc-log-wrap-nickname-face)
	 (4 irc-log-message-face)
	 )
       `(,(format "^.* <\\(%s\\)> .*$" erc-valid-nick-regexp)
	 1 (erc-log-nick-get-face)
	 )
       `("\\(.*\\) \\(\\*\\*\\* .*\\)"
	 (1 irc-log-timestamp-face)
	 (2 irc-log-notice-face)
	 )))


;; undefine some syntax that's messing up with our coloring (for instance, "")
(defvar irc-log-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\" ".   " st)
    (modify-syntax-entry ?\\ ".   " st)
    st)
  "Syntax table used while in `irc-log-mode'.")

(define-derived-mode irc-log-mode fundamental-mode
  (setq font-lock-defaults '(irc-log-keywords))
  (setq mode-name "IRC Log"))


(provide 'irc-log-mode)

;;; irc-log-mode.el ends here
