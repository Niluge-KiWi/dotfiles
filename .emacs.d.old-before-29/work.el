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
;;; YASnippet
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (add-to-list 'yas/snippet-dirs "~/.emacs.d/work-snippets")
;; (yas/reload-all)
