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

;;; TODO:
;; - replace defcustom faces to faces or function...
;; - less flashy colors by default
;; - handle \me messages : nick in bold, and message as a standard message
;; - handle own messages : special color (use erc-nick or server-nick-alist?)

(require 'erc)
(require 'erc-nick-color)



(defcustom irc-log-timestamp-face
  '((:foreground "red"))
  "Face for timestamps."
  :type 'face
  :group 'irc-log-faces)

(defcustom irc-log-wrap-nickname-face
  '((:foreground "black"))
  "Face for nicknames wrappers <>."
  :type 'face
  :group 'irc-log-faces)

(defcustom irc-log-nickname-face
  '((:foreground "blue"))
  "Face for nicknames."
  :type 'face
  :group 'irc-log-faces)

(defcustom irc-log-message-face
  '((:foreground "green"))
  "Face for messages."
  :type 'face
  :group 'irc-log-faces)

(defcustom irc-log-info-line-face
  '((:foreground "yellow"))
  "Face for info lines."
  :type 'face
  :group 'irc-log-faces)




;; warning: only works if erc-timestamp-format doesn't contains the character '<'
(setq irc-log-keywords
      (list
       ;; first regexp apply the face
       `(,(format "^\\(.*\\) \\(<\\)\\(%s\\)\\(>\\) \\(.*\\)$" erc-valid-nick-regexp)
	 (1 irc-log-timestamp-face)
	 (2 irc-log-wrap-nickname-face)
	 (3 irc-log-nickname-face)
	 (4 irc-log-wrap-nickname-face)
	 (5 irc-log-message-face)
	 )
       `(".* \\*\\*\\* .*"
	 (0 irc-log-info-line-face)
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
