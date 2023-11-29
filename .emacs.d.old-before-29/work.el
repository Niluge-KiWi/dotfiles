;; -*- coding: utf-8 -*-
;;; Emacs config for work


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Style
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default indent-tabs-mode nil)       ; space indentation
(setq-default c-basic-offset 2)
(setq-default tab-width 8)
(setq-default espresso-indent-level 3)
(setq-default groovy-indent-offset 4)
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

;; C-c C-s on position to get the syntactic context; or set c-echo-syntactic-information-p; C-c C-o to set
(c-set-offset 'substatement-open 0)       ; The brace that opens a substatement block.
(c-set-offset 'inline-open 0)             ; Brace that opens an in-class inline method.
(c-set-offset 'case-label '+)             ; A "case" or "default" label.
(c-set-offset 'innamespace '+)            ; indent in namespaces


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Xmllint
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun xmllint ()
  "Prettify xml buffer"
  (interactive)
  (xmllint-region (point-min) (point-max))
  (message "Buffer has been xmllint-ed."))

(defun xmllint-region (pmin pmax)
  "Prettify xml region"
  (interactive "r")
  (save-excursion
    (shell-command-on-region pmin pmax
                             "xmllint --format --recover -"
                             (current-buffer) 'replace
                             (get-buffer-create "*Xmllint Errors*")
                             'display-error-buffer)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; jsonlint
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun jsonlint ()
  "Prettify json buffer"
  (interactive)
  (jsonlint-region (point-min) (point-max))
  (message "Buffer has been jsonlint-ed."))


(defun jsonlint-region (pmin pmax)
  "Prettify json region"
  (interactive "r")
  (save-excursion
    (shell-command-on-region pmin pmax
                             "python -m json.tool"
                             (current-buffer) 'replace
                             (get-buffer-create "*jsonlint errors*")
                             'display-error-buffer)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sqllint
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun sqllint ()
  "Prettify sql buffer"
  (interactive)
  (sqllint-region (point-min) (point-max))
  (message "Buffer has been sqllint-ed."))


(defun sqllint-region (pmin pmax)
  "Prettify sql region"
  (interactive "r")
  (save-excursion
    (shell-command-on-region pmin pmax
                             "sqlformat --reindent_aligned -"
                             (current-buffer) 'replace
                             (get-buffer-create "*sqllint errors*")
                             'display-error-buffer)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; YASnippet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (add-to-list 'yas/snippet-dirs "~/.emacs.d/work-snippets")
;; (yas/reload-all)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Auto Complete Clang
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq ac-clang-executable "/usr/local/bin/clang")
(defadvice ac-clang-build-complete-args (around my-ac-clang-flags (pos))
  "Set clang-flags using CMake compile db."
  (let* ((flags (shell-command-to-string
                 (format "~/scripts/clang-args.sh %s %s" my-clang-build-dir buffer-file-name)))
         (ac-clang-flags (split-string flags "\n")))
    ad-do-it))
(ad-activate 'ac-clang-build-complete-args)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Misc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; force c++-mode for .h files and .l files (flex)
(add-to-list 'auto-mode-alist '("\\.\\(h\\|l\\)\\'" . c++-mode))

;; fast open main doc.org file
(defun doc ()
  (interactive)
  (find-file "~/doc/doc.org"))
