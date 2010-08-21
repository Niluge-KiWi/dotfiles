;;; erc-nick-color.el --- Colors on nick for erc

;; parts from http://www.emacswiki.org/emacs/ErcNickColors and from smeuuh

;; Copyright (C) 2010 Thomas Riccardi

;;; Commentary:
;; Set colors nicknames in erc buffers:
;; - random colors based on md5, directly mapped to #rrggbb values,
;; - hardcoded colors for known nicknames

;;; Installation:
;;    (require 'erc-nick-color)
;;    (add-hook 'erc-insert-modify-hook 'erc-put-color-on-nick)
;;    (setq erc-nick-color-alist
;;    (list (cons "NickName1" green)
;;          (cons "NickName2" blue)
;;          ))

(require 'erc)

(setq erc-nick-color-alist '())
(setq erc-nick-color-match-regexp (format "<\\(%s\\)>" erc-valid-nick-regexp))

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
