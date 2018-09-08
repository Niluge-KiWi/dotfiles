;; -*- coding: utf-8 -*-
;;; Emacs config for work


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Style
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; delete trailing white-spaces before save
;; TODO toggle function
;; TODO minor-mode
(defun turn-on-delete-trailing-whitespace ()
  (interactive)
  (add-hook 'before-save-hook 'delete-trailing-whitespace nil t))
;; hack to run this on all buffers
(add-hook 'after-change-major-mode-hook 'turn-on-delete-trailing-whitespace)
(defun turn-off-delete-trailing-whitespace ()
  (interactive)
  (remove-hook 'before-save-hook 'delete-trailing-whitespace t))
(add-hook 'markdown-mode-hook 'turn-off-delete-trailing-whitespace)

(setq-default indent-tabs-mode nil)       ; space indentation
(setq-default c-basic-offset 2)
(setq-default tab-width 8)
(setq-default espresso-indent-level 3)
(setq-default js2-basic-offset 2)
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
                             (get-buffer-create "*python errors*")
                             'display-error-buffer)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; js2-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; from http://nodejs.org/api/globals.html
(require 'js2-mode)
(setq my-js2-global-externs-nodejs
      '("global"
        "process"
        "console"
        "Buffer"
        "require"
        "__filename"
        "__dirname"
        "module"
        "exports"
        "setTimeout"
        "clearTimeout"
        "setInterval"
        "clearInterval"
        ))
(setq js2-global-externs (append js2-global-externs my-js2-global-externs-nodejs))
;; mocha globals
(setq my-js2-global-externs-mocha
      '("after"
        "afterEach"
        "before"
        "beforeEach"
        "describe"
        "it"
        "setup"
        "suite"
        "teardown"
        "test"
        ))
(setq js2-global-externs (append js2-global-externs my-js2-global-externs-mocha))

;; from http://emacswiki.org/emacs/Js2Mode
;; After js2 has parsed a js file, we look for jslint globals decl comment ("/* global Fred, _, Harry */") and
;; add any symbols to a buffer-local var of acceptable global vars
;; Note that we also support the "symbol: true" way of specifying names via a hack (remove any ":true"
;; to make it look like a plain decl, and any ':false' are left behind so they'll effectively be ignored as
;; you can;t have a symbol called "someName:false"
(defun my-js2-globals-comment ()
  (when (> (buffer-size) 0)
    (let ((btext (replace-regexp-in-string
                  ": *true" " "
                  (replace-regexp-in-string "[\n\t ]+" " " (buffer-substring-no-properties 1 (buffer-size)) t t))))
      (mapc (apply-partially 'add-to-list 'js2-additional-externs)
            (split-string
             (if (string-match "/\\* *global *\\(.*?\\) *\\*/" btext) (match-string-no-properties 1 btext) "")
             " *, *" t)))))
(add-hook 'js2-post-parse-callbacks 'my-js2-globals-comment)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Grep
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq grep-command "grep -nriIH --exclude=*~ --exclude \".#*\" --exclude-dir=.git ")


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
