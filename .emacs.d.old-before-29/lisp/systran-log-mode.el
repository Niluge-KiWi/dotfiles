;; Systran log mode
;; Usage:
;; (require 'systran-log-mode)
;; (add-to-list 'auto-mode-alist '("/log-[^/]*$" . systran-log-mode))



(defface systran-log-date-face
  '((t (:inherit erc-timestamp)))
  "Systran log date face."
  :group 'systran-log-faces)

(defface systran-log-domain-face
  '((t (:inherit erc-keyword)))
  "Systran log domain face."
  :group 'systran-log-faces)

(defface systran-log-level-face
  '((t (:inherit erc-fool)))
  "Systran log level face."
  :group 'systran-log-faces)

(defface systran-log-level-error-face
  '((t (:inherit font-lock-warning)))
  "Systran log level error face."
  :group 'systran-log-faces)

(defface systran-log-message-face
  '()
  "Systran log message face."
  :group 'systran-log-faces)


(defun systran-log-get-level-face (level)
  "Returns a level face for the given level."
  (cond ((or (string= level "FATAL")
             (string= level "ERROR")
             (string= level "WARN"))
         'systran-log-level-error-face)
        (t
         'systran-log-level-face)))

;; Warning: do not use group constructions ("\\(some regexp\\)") inside the following regexps
(defvar systran-log-date-regexp
  "[0-9]\\{4\\}-[a-zA-Z]\\{3\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}.[0-9]\\{6\\}\\(?: UTC\\)"
  "Regexp to match date.")

(defvar systran-log-keywords
  `(
    (,(format "\\(%s\\) \\[\\([^\]]*\\)\\] \\([A-Z]*\\) \\(.*\\)" systran-log-date-regexp)
     (1 'systran-log-date-face)
     (2 'systran-log-domain-face)
     (3 (systran-log-get-level-face (match-string 3)))
     (4 'systran-log-message-face)
     )
    )
  "Returns the font-lock-defaults.")

(defun systran-log-previous-entry ()
  "Move point to previous log entry (skips multi-lines log entries lines)"
  (interactive)
  (beginning-of-line)
  (re-search-backward (format "^%s \\[[^\]]*\\] [A-Z]* [^\t]" systran-log-date-regexp))
  (forward-char (- (length (match-string 0)) 1))
  )

(defun systran-log-next-entry ()
  "Move point to next log entry (skips multi-lines log entries lines)"
  (interactive)
  (end-of-line)
  (re-search-forward (format "^%s \\[[^\]]*\\] [A-Z]* [^\t]" systran-log-date-regexp))
  (backward-char))

;; Create the keymap for this mode.
(defvar systran-log-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "p" 'systran-log-previous-entry)
    (define-key map "n" 'systran-log-next-entry)
    map)
  "Keymap for `systran-log-mode'.")

;;;###autoload
(define-derived-mode systran-log-mode fundamental-mode
  "Systran Log"
  "Major mode for viewing systran logs.

Special commands:

\\{systran-view-log-mode-map}

Turning on `systran-log-mode' runs the hook `systran-log-mode-hook'."
  (setq font-lock-defaults '(systran-log-keywords))
  (setq buffer-read-only t))


(provide 'systran-log-mode)
