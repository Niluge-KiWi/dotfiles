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

(defcustom irc-log-my-nickname-face
  'erc-my-nick-face
  "Face for my nickname."
  :type 'face
  :group 'irc-log-faces)

(defcustom irc-log-message-face
  'erc-default-face
  "Face for messages."
  :type 'face
  :group 'irc-log-faces)

(defcustom irc-log-my-message-face
  'erc-input-face
  "Face for my messages."
  :type 'face
  :group 'irc-log-faces)

(defcustom irc-log-notice-face
  'erc-notice-face
  "Face for notices."
  :type 'face
  :group 'irc-log-faces)

(defcustom irc-log-action-face
  'erc-action-face
  "Face for actions (like /ME)."
  :type 'face
  :group 'irc-log-faces)

(defcustom irc-log-prompt-face
  'erc-prompt-face
  "Face for prompts."
  :type 'face
  :group 'irc-log-faces)

(defcustom irc-log-command-indicator-face
  'erc-command-indicator-face
  "Face for command-indicators (like /whois nickname)."
  :type 'face
  :group 'irc-log-faces)


(defcustom irc-log-timestamp-regexp
  ".*"
  "Regexp to match timestamps (no group match)."
  :type 'regexp
  :group 'irc-log-regexps)

(defcustom irc-log-nickname-regexp
  erc-valid-nick-regexp
  "Regexp to match nicknames (no group match)."
  :type 'regexp
  :group 'irc-log-regexps)

(defcustom irc-log-my-nickname-regexp
  erc-nick
  "Regexp to match my nickname (no group match)."
  :type 'regexp
  :group 'irc-log-regexps)

(defcustom irc-log-message-regexp
  ".*"
  "Regexp to match messages (no group match)."
  :type 'regexp
  :group 'irc-log-regexps)

(defcustom irc-log-notice-regexp
  "\\*\\*\\* .*"
  "Regexp to match notices (no group match)."
  :type 'regexp
  :group 'irc-log-regexps)

(defcustom irc-log-action-regexp
  (format "\\* %s .*" erc-valid-nick-regexp)
  "Regexp to match actions (no group match)."
  :type 'regexp
  :group 'irc-log-regexps)

(defcustom irc-log-prompt-regexp
  ">"
  "Regexp to match prompts (no group match)."
  :type 'regexp
  :group 'irc-log-regexps)


(defun erc-log-nick-get-face (n)
  "Returns a face for the matched nick, given the match number."
  (if (facep irc-log-nickname-face)
      irc-log-nickname-face
    (apply irc-log-nickname-face (list (match-string n)))))



;; warning: only works if erc-timestamp-format doesn't contains the pattern "<a_nickname>"
(defun irc-log-get-keywords ()
  "Returns the font-lock-defaults."
      (list
       ;; own message line
       `(,(format "^\\(%s\\) \\(<\\)\\(%s\\)\\(>\\) \\(%s\\)$" irc-log-timestamp-regexp irc-log-my-nickname-regexp irc-log-message-regexp)
	 (1 irc-log-timestamp-face)
	 (2 irc-log-wrap-nickname-face)
	 (3 irc-log-my-nickname-face)
	 (4 irc-log-wrap-nickname-face)
	 (5 irc-log-my-message-face)
	 )
       ;; standard message line
       `(,(format "^\\(%s\\) \\(<\\)\\(%s\\)\\(>\\) \\(%s\\)$" irc-log-timestamp-regexp irc-log-nickname-regexp irc-log-message-regexp)
	 (1 irc-log-timestamp-face)
	 (2 irc-log-wrap-nickname-face)
	 (3 (erc-log-nick-get-face 3))
	 (4 irc-log-wrap-nickname-face)
	 (5 irc-log-message-face)
	 )
       ;; notice line
       `(,(format "\\(%s\\) \\(%s\\)" irc-log-timestamp-regexp irc-log-notice-regexp)
	 (1 irc-log-timestamp-face)
	 (2 irc-log-notice-face)
	 )
       ;; action line
       `(,(format "\\(%s\\) \\(%s\\)" irc-log-timestamp-regexp irc-log-action-regexp)
	 (1 irc-log-timestamp-face)
	 (2 irc-log-action-face)
	 )
       ;; command line
       `(,(format "\\(%s\\) \\(%s\\) \\(/.*\\)" irc-log-timestamp-regexp irc-log-prompt-regexp)
	 (1 irc-log-timestamp-face)
	 (2 irc-log-prompt-face)
	 (3 irc-log-command-indicator-face)
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
  (toggle-read-only))


(provide 'irc-log-mode)

;;; irc-log-mode.el ends here
