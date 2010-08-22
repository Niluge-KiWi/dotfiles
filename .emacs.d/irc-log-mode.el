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

;;; Description:
;; Set colors on an irc/erc log file

;;; Installation:
;;    (require 'irc-log-mode)
;;    (add-to-list 'auto-mode-alist '("\\.erclogs/.*\\.log" . irc-log-mode))

;;; Options:
;; Faces for: timestamp, wrap nickname (<>), nickname, own nickname, message, own message, notice, action, command.
;; For nickname: can be a function that takes the nickname as argument, and returns a face
;; Regexps for: timestamps, nicknames, own nickname, messages, notices, actions, prompt

;;; TODO:
;; - r to reload the log
;; - my-nick custom : a list and not a regexp (regexp-opt)
;; - rename to erc-view-log-mode
;; - handle priv messages?: erc-direct-msg-face
;; - handle nick for private messages?: erc-nick-msg-face
;; - handle "*** Users on"?: erc-current-nick-face
;; - use vlf.el for large logs? has to be adapted (no more major mode, and handle full lines...)

(require 'erc)
(require 'erc-nick-color)


(defcustom irc-log-nickname-face-function
  'erc-get-face-for-nick
  "A function that returns a face, given the nick."
  :type 'function)


(defvar irc-log-timestamp-regexp
  ".*"
  "Regexp to match timestamps (no group match).")

(defvar irc-log-nickname-regexp
  erc-valid-nick-regexp
  "Regexp to match nicknames (no group match).")

(defvar irc-log-my-nickname-regexp
  erc-nick
  "Regexp to match my nickname (no group match).")

(defvar irc-log-message-regexp
  ".*"
  "Regexp to match messages (no group match).")

(defvar irc-log-notice-regexp
  "\\*\\*\\* .*"
  "Regexp to match notices (no group match).")

(defvar irc-log-action-regexp
  (format "\\* %s .*" erc-valid-nick-regexp)
  "Regexp to match actions (no group match).")

(defvar irc-log-prompt-regexp
  erc-prompt
  "Regexp to match prompts (no group match).")


(defun erc-log-nick-get-face (nick)
  "Returns a face for the given nick."
  (if irc-log-nickname-face-function
      (apply irc-log-nickname-face-function (list nick))
    'erc-nick-default-face))



;; warning: only works if erc-timestamp-format doesn't contains the pattern "<a_nickname>"
(defun irc-log-get-keywords ()
  "Returns the font-lock-defaults."
      (list
       ;; own message line
       `(,(format "^\\(%s\\) \\(<\\)\\(%s\\)\\(>\\)[ \t]\\(%s\\)$" irc-log-timestamp-regexp irc-log-my-nickname-regexp irc-log-message-regexp)
	 (1 'erc-timestamp-face)
	 (2 'erc-default-face)
	 (3 'erc-my-nick-face)
	 (4 'erc-default-face)
	 (5 'erc-input-face) ;; my message
	 )
       ;; standard message line
       `(,(format "^\\(%s\\) \\(<\\)\\(%s\\)\\(>\\)[ \t]\\(%s\\)$" irc-log-timestamp-regexp irc-log-nickname-regexp irc-log-message-regexp)
	 (1 'erc-timestamp-face)
	 (2 'erc-default-face)
	 (3 (erc-log-nick-get-face (match-string 3)))
	 (4 'erc-default-face)
	 (5 'erc-default-face) ;; other message
	 )
       ;; notice line
       `(,(format "\\(%s\\) \\(%s\\)" irc-log-timestamp-regexp irc-log-notice-regexp)
	 (1 'erc-timestamp-face)
	 (2 'erc-notice-face)
	 )
       ;; action line
       `(,(format "\\(%s\\) \\(%s\\)" irc-log-timestamp-regexp irc-log-action-regexp)
	 (1 'erc-timestamp-face)
	 (2 'erc-action-face)
	 )
       ;; command line
       `(,(format "\\(%s\\) \\(%s\\) \\(/.*\\)" irc-log-timestamp-regexp irc-log-prompt-regexp)
	 (1 'erc-timestamp-face)
	 (2 'erc-prompt-face)
	 (3 'erc-command-indicator-face)
	 )
       ))


;; undefine some syntax that's messing up with our coloring (for instance, "")
(defvar irc-log-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?\" ".   " st)
    (modify-syntax-entry ?\\ ".   " st)
    st)
  "Syntax table used while in `irc-log-mode'.")


(define-derived-mode irc-log-mode fundamental-mode
  "IRC Log"
  (setq font-lock-defaults `(,(irc-log-get-keywords)))
  (setq buffer-read-only t))


(provide 'irc-log-mode)

;;; irc-log-mode.el ends here
