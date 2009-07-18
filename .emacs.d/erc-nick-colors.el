;; Use this elisp to color IRC nicks according to a list of usable colors. 
;; Note that the colors are determined by the MD5 sum of a nick string, 
;; and if the nick-colors-list changes, so will the color indexes. 
;; Enjoy! 
;; Put this in your emacs load path and add (require 'erc-nick-colors) to your .emacs. 
;; If you have any improvements or suggestions, 
;; contact me at cygnus at cprogrammer dot org.


;; modified by Niluge_KiWi
;; - random colors based on md5, directly mapped to #rrggbb values, 
;; instead of a set of colors.
;; - force colors for known nicknames

(setq erc-nick-colors-alist '())


(defun erc-nick-colors-hook ()
  "This insert-modify hook looks for nicks in new messages, 
computes md5(nick) and uses substring(md5_value, 0, 6) as the #rrggbb color.
There is also a forced (nick color) list for known nicknames."
  (save-excursion
    (goto-char (point-min))
    (if (looking-at "<\\([^>]*\\)>")
        (let* ((nick (match-string 1)) 
	       (color (cdr (assoc nick erc-nick-colors-alist))))
	  (if color
	      (put-text-property (match-beginning 1) (match-end 1)
				 'face 
				 (list :foreground color))
	    
	    (put-text-property (match-beginning 1) (match-end 1)
			       'face (list :foreground 
					   (concat "#" 
						   (substring (md5 nick) 0 6)))
			       )
	    )))))

;; This adds the ERC message insert hook.                                                                                        
(add-hook 'erc-insert-modify-hook 'erc-nick-colors-hook)

(provide 'erc-nick-colors)
