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
;; - colorize exactly like an erc buffer: not only nicks, but also msg and other texts

(require 'erc-nick-color)

;; Colorise logs
(setq irc-log-keywords
      `((,(format "%s" erc-nick-color-match-regexp) 1 (erc-nick-colorize)))) ;; TODO remove format, but it doesn't work with just erc-nick-color-match-regexp...

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
