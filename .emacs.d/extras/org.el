;;; Emacs Bedrock
;;;
;;; Extra config: Org-mode starter config

;;; Usage: Append or require this file from init.el for some software
;;; development-focused packages.
;;;
;;; Org-mode is a fantastically powerful package. It does a lot of things, which
;;; makes it a little difficult to understand at first.
;;;
;;; We will configure Org-mode in phases. Work with each phase as you are
;;; comfortable.
;;;
;;; YOU NEED TO CONFIGURE SOME VARIABLES! The most important variable is the
;;; `org-directory', which tells org-mode where to look to find your agenda
;;; files.

;;; See "org-intro.txt" for a high-level overview.

;;; Contents:
;;;
;;;  - Critical variables
;;;  - Phase 1: editing and exporting files
;;;  - Phase 2: todos, agenda generation, and task tracking
;;;  - Phase 3: extensions (org-roam, etc.)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Critical variables
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; These variables need to be set for Org-mode's full power to be unlocked!
;;;
;;; You can read the documentation for any variable with `C-h v'. If you have
;;; Consult configured (see the `base.el' file) then it should help you find
;;; what you're looking for.

;;; Phase 1 variables
(setq org-startup-folded nil)

;;; Phase 2 variables

;; Agenda variables
;; TODO review
(setq org-directory "~/Documents/org/") ; Non-absolute paths for agenda and
                                        ; capture templates will look here.

(setq org-agenda-files '("inbox.org" "work.org"))

;; Default tags
(setq org-tag-persistent-alist
      '((:startgrouptag)
        ("priority" . ??) ; don't want 'p' as tag key
        (:startgroup)     ; exclusive tags
        ("p0" . ?0)
        ("p0_5")
        ("p1" . ?1)
        ("p1_5")
        ("p2" . ?2)
        ("p3" . ?3)
        ("p4" . ?4)
        (:endgroup)
        (:endgrouptag)))
;; exit on first tag key; use C-c before to stay on tag selection
(setq org-fast-tag-selection-single-key t)

;; Org-refile: where should org-refile look?
(setq org-refile-targets 'FIXME)

;;; Phase 3 variables

;; Org-roam variables
(setq org-roam-directory "~/Documents/org-roam/")
(setq org-roam-index-file "~/Documents/org-roam/index.org")

;;; Optional variables

;; Advanced: Custom link types
;; This example is for linking a person's 7-character ID to their page on the
;; free genealogy website Family Search.
(setq org-link-abbrev-alist
      '(("family_search" . "https://www.familysearch.org/tree/person/details/%s")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Phase 1: editing and exporting files
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org
  :hook ((org-mode . visual-line-mode)  ; wrap lines at word breaks
         (org-mode . flyspell-mode)     ; spell checking!
         (org-mode . (lambda () (electric-indent-local-mode -1))) ; disable electric-indent in org-mode, so RET on headlines don't indent
         )

  :bind
  (:map global-map
        ("C-c l" . org-store-link) ; use C-c C-l to insert link
        )

  :config
  ;; export
  (add-to-list 'org-export-backends 'md)

  (setq org-export-preserve-breaks t)
  ;; no sub-superscripts on export: I dont use them, and it messes html exports
  (setq org-export-with-sub-superscripts nil)

  ;; edit
  ;; Make org-open-at-point follow file links in the same window
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)

  ;; enable < s TAB to create source code block; see 'org-structure-tempalte-alist'
  (require 'org-tempo)

  ;; indent headings like log notes, but not the rest of the text under the headline
  (setq org-adapt-indentation 'headline-data)

  ;; don't indent text in src blocks
  (setq org-src-preserve-indentation nil) ; the default
  (setq org-edit-src-content-indentation 0) ; maybe alternative: set 'org-src-preserve-indentation' to 't', but it did strange things when trying to indent in the src block

  ;; org-copy-buffer-to-clipboard: uses xclip binary to add Rich Formatted Text to clipboard (really: html)
  ;; inspiration:
  ;;   https://kitchingroup.cheme.cmu.edu/blog/2016/06/16/Copy-formatted-org-mode-text-from-Emacs-to-other-applications/
  ;; issues:
  ;; - xclip & X clipboard requires an alive process to serve the paste action, so use start-process instead of shell-command to not block emacs usage.
  ;; - fork of xclip-set-selection, to support '-t text/html'; stripped down to just xclip support
  (defun xclip-set-selection-html (type data)
    "TYPE is a symbol: primary, secondary and clipboard.
fork of xclip-set-selection, to support '-t text/html'; stripped down to just xclip support."
    (let* ((process-connection-type nil)
           (proc
            (when (getenv "DISPLAY")
              (start-process "xclip" nil xclip-program
                             "-t" "text/html"
                             "-selection" (symbol-name type)))))
      (when proc
        (process-send-string proc data)
        (process-send-eof proc))
      data))

  (defun org-copy-buffer-to-clipboard ()
    "Export region to HTML, and copy it to the clipboard."
    (interactive)
    (let* ((org-export-show-temporary-export-buffer nil))
      ;;                           async subtreep visible-only body-only ext-plist
      (org-export-to-buffer
          'html "*Org Formatted Copy*" nil nil t t nil
          (lambda ()
            (xclip-set-selection-html 'clipboard (buffer-string))
            (kill-buffer (current-buffer))
            (message "org formatted copied to clipboard")))))
  )

(use-package ox-pandoc
  :ensure t
  ;; TODO :ensure-system-package
  :after org
  :config
  (setq org-pandoc-options
        '(
          (standalone . t)
          (atx-headers . t)
          (wrap . "none")
          (tab-stop . 2)
          ))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Phase 2: todos, agenda generation, and task tracking
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Yes, you can have multiple use-package declarations. It's best if their
;; configs don't overlap. Once you've reached Phase 2, I'd recommend merging the
;; config from Phase 1. I've broken it up here for the sake of clarity.
(use-package org
  :config
  ;; Instead of just two states (TODO, DONE) we set up a few different states
  ;; that a task can be in.
  (setq org-todo-keywords
        '((sequence "TODO(t)" "MAYBE(m)" "|" "PR(p)" "DONE(d)" "WONTDO(w)" "INISSUE(i)" "LATER(l)")))

  (setq org-log-done 'note)

  ;; Refile configuration
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-use-outline-path 'file)

  ;; TODO review
  (setq org-capture-templates
        '(("c" "Default Capture" entry (file "inbox.org")
           "* TODO %?\n%U\n%i")
          ;; Capture and keep an org-link to the thing we're currently working with
          ("r" "Capture with Reference" entry (file "inbox.org")
           "* TODO %?\n%U\n%i\n%a")
          ;; Define a section
          ("w" "Work")
          ("wm" "Work meeting" entry (file+headline "work.org" "Meetings")
           "** TODO %?\n%U\n%i\n%a")
          ("wr" "Work report" entry (file+headline "work.org" "Reports")
           "** TODO %?\n%U\n%i\n%a")))

  ;; TODO review
  (setq org-agenda-custom-commands
        '(("n" "Agenda and All Todos"
           ((agenda)
            (todo)))
          ("w" "Work" agenda ""
           ((org-agenda-files '("work.org")))))))

(use-package org-journal
  :ensure t
  :after org
  :config
  (setq org-journal-dir "~/.emacs.d/journal/")
  (setq org-journal-file-format "%Y-%m-%d.org")
  (setq org-journal-tag-alist '(("manger" . ?m) ("home" . ?h) ("codereview" . ?r) ("ops" . ?o) ("meeting" . ?m) ("veille" . ?v)))
  (setq org-journal-date-prefix "* ")
  (setq org-journal-date-format "%Y-%m-%d, %A")
  (setq org-journal-time-format "%R ")
  (setq org-journal-find-file 'find-file)
  (setq org-journal-carryover-items "")
  ;; link org-journal with org-agenda
  (add-to-list 'org-agenda-files org-journal-dir)
  )

(use-package journal-daily-summary
  :after org-journal
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Phase 3: extensions (org-roam, etc.)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(use-package org-roam
;  :ensure t
;  :config
;  (org-roam-db-autosync-mode)
;  ;; Dedicated side window for backlinks
;  (add-to-list 'display-buffer-alist
;               '("\\*org-roam\\*"
;                 (display-buffer-in-side-window)
;                 (side . right)
;                 (window-width . 0.4)
;                 (window-height . fit-window-to-buffer))))

;; Pretty web interface for org-roam
;(use-package org-roam-ui
;  :ensure t
;  :after org-roam
;  :config
;  (setq org-roam-ui-sync-theme t
;        org-roam-ui-follow t
;        org-roam-ui-update-on-save t
;        org-roam-ui-open-on-start t))
