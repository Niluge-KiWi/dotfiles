;;; erc-nick-color.el --- Colors on nick for erc

;; parts from http://www.emacswiki.org/emacs/ErcNickColors and from smeuuh

;; Copyright (C) 2010 Thomas Riccardi

;; This program is free software. It comes without any warranty, to
;; the extent permitted by applicable law. You can redistribute it
;; and/or modify it under the terms of the Do What The Fuck You Want
;; To Public License, Version 2, as published by Sam Hocevar. See
;; http://sam.zoy.org/wtfpl/COPYING for more details.

;;; Commentary:
;; Set colors nicknames in erc buffers:
;; - random colors based on md5, directly mapped to #rrggbb values,
;; - hardcoded colors for known nicknames

;;; Installation:
;;    (require 'erc-nick-color)
;;    (add-hook 'erc-insert-modify-hook 'erc-put-color-on-nick)
;;    (setq erc-nick-color-alist '(("Nick1" . "blue")
;;    			     ("Nick2" . "green")
;;    			     ))

;;; Todo
;; - colorize nicks everywhere : http://github.com/antoine-levitt/perso/commit/eae7232f9c9a1fa9201061c8aeffedbd76d6d036 but with advice on erc-button-add-button

(require 'erc)

(defcustom erc-nick-color-alist '()
  "alist of nick and color."
  ;;:type '(alist :key-type (string :tag "nick") :value-type 'color) ;; TODO revoir le type
  :group 'erc)

(defvar erc-nick-color-match-regexp
  (format "<\\(%s\\)>" erc-valid-nick-regexp)
  "Regexp to match nicks.")

(defun erc-get-color-for-nick (nick)
  "Returns the color for the given nick, by random md5 or hardcoded value."
  (or (cdr (assoc nick erc-nick-color-alist))
      (concat "#" (substring (md5 nick) 0 6)))) ;;TODO better contrast with background color?

(defun erc-get-face-for-nick (nick)
  "Returns the face for the given nick."
  `((:foreground ,(erc-get-color-for-nick nick))
    (:weight bold)))

(defun erc-nick-colorize ()
  "Colorise the matched nick with the appropriate face"
  (put-text-property
   (match-beginning 1) (match-end 1)
   'face (erc-get-face-for-nick (match-string 1))))

(defun erc-put-color-on-nick ()
  "This insert-modify hook looks for nicks in new messages and
colorize with erc-nick-colorize"
  (save-excursion
    (goto-char (point-min))
    (if (looking-at erc-nick-color-match-regexp)
	(erc-nick-colorize))))

(provide 'erc-nick-color)
