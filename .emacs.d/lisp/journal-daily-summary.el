;; journal-daily-summary: compute daily work duration from org-journal entries

(require 'org)

(defun jds:human-time-to-seconds (human-time)
  "input: '03:14', output: 11640"
  (let* ((time (parse-time-string human-time))
         (seconds
          (+
           (nth 0 time)
           (* 60 (nth 1 time))
           (* 60 60 (nth 2 time)))))
    seconds))
(defun jds:seconds-to-human-time (seconds)
  "input: 11640, output: '03:14'"
  (format-seconds "%.2h:%.2m" seconds))
(defun jds:get-date ()
  "Read org-journal date at entry"
  (buffer-substring-no-properties (+ (point) 2) (+ (point) 12)))
(defun jds:get-timestamp (&optional pos)
  "Read org-journal timestamp at entry"
  (let ((pos (or pos (point))))
    (buffer-substring-no-properties (+ pos 3) (+ pos 8))))
;; TODO dynamic block for weekly/monthly summary
;; TODO support :pause: tags for long pause during the day
;; TODO better error when missing tag or other entry, for now get "date-to-time: Invalid date: 2020-02-02 "
(defun jds:get-summary ()
  (let* (
         (date (car (org-map-entries 'jds:get-date "LEVEL=1")))
         (times-human
          (list
           :arrival
           (car (org-map-entries 'jds:get-timestamp "LEVEL=2"))
           :lunch-start
           ;;(or
            (car (org-map-entries 'jds:get-timestamp "manger+LEVEL=2"))
            ;;"12:15")
           :lunch-end
           ;;(or
            (car (org-map-entries (lambda ()
                                    (jds:get-timestamp (org-get-next-sibling))) "manger+LEVEL=2"))
            ;;"13:20")
           :departure
           ;;(or
            (car (org-map-entries 'jds:get-timestamp "home+LEVEL=2"))
            ;;"19:00")
           ))
         (times
          (cl-loop for (key value) on times-human by 'cddr
                   append (list key (date-to-time (concat "2020-02-02 " value)))))
         (durations-minutes
          (list
           :morning
           (/ (time-to-seconds (time-subtract (plist-get times :lunch-start) (plist-get times :arrival))) 60)
           :lunch
           (/ (time-to-seconds (time-subtract (plist-get times :lunch-end) (plist-get times :lunch-start))) 60)
           :afternoon
           (/ (time-to-seconds (time-subtract (plist-get times :departure) (plist-get times :lunch-end))) 60)
           ))
         (durations-minutes
          (plist-put
           durations-minutes
           :work
           (+ (plist-get durations-minutes :morning) (plist-get durations-minutes :afternoon))))
         (durations-human
          (cl-loop for (key value) on durations-minutes by 'cddr
                   append (list key (jds:seconds-to-human-time (* 60 value)))))
         )
    ;; debug messages
    (message "jds: date: %s" date)
    (message "jds: times: %s"
                    (string-join
                     (cl-loop for (key value) on times-human by 'cddr collect value)
                     " "))
    (message "jds: durations-minutes: %s"
                    (string-join
                     (cl-loop for (key value) on durations-minutes by 'cddr collect (format "%s" value))
                     " "))
    (message "jds: durations: %s"
                    (string-join
                     (cl-loop for (key value) on durations-human by 'cddr collect value)
                     " "))
    ;; return nested plist
    (list
     :date date
     :times-human times-human
     :times times
     :durations-minutes durations-minutes
     :durations-human durations-human
     )))


;; TODO support (=ignore?) nil days in avg
;; TODO split interactive function and files list as input and maybe computation?
;; TODO dump csv
;; TODO dblock with weekly and monthly summary?
;; TODO update all old journal files
;; TODO selet date by calendar? or by file mark in dired? or multi file via read-completion?
(defun jds:summary-multi-day (date-regex-prefix)
  "Usage: 2020-03-2[34567]"
  (interactive "sDate prefix? ")
  (let* ((files
          (directory-files (expand-file-name org-journal-dir) t
                           (concat "^" date-regex-prefix ".*\.org$")))
         (works-human
          (org-map-entries
           (lambda ()
             (let* ((org-trust-scanner-tags t))
               (org-entry-get nil "work")))
           "LEVEL=1" files))
         (works-minutes
          (org-map-entries
           (lambda ()
             (let* ((org-trust-scanner-tags t))
               (string-to-number (org-entry-get nil "work-minutes"))))
           "LEVEL=1" files))
         (sum
          (apply '+ works-minutes))
         (len
          (length works-minutes))
         (daily-avg
          (/ (float sum) len))
         (weekly-avg
          (* 5 daily-avg))
         (weekly-avg-human
          (jds:seconds-to-human-time (* 60 weekly-avg)))
         )
    (message "jds: %s: %s %s %s %s %s" works-minutes sum len daily-avg weekly-avg weekly-avg-human)
    (message "jds: weekly-avg: %s" weekly-avg-human)
    ))


(defun jds:update-properties ()
  "Update journal daily summary as top-level org entry properties"
  (interactive)
  (let* ((summary (jds:get-summary))
         (times-human (plist-get summary :times-human))
         (durations-minutes (plist-get summary :durations-minutes))
         (durations-human (plist-get summary :durations-human)))
    (org-map-entries
     (lambda ()
       (org-entry-put (point) "work" (plist-get durations-human :work))
       (org-entry-put (point) "work-minutes" (format "%d" (plist-get durations-minutes :work))))
     "LEVEL=1")))
(defun jds:update-properties-multi-day (date-prefix)
  (interactive "sDate prefix? ")
  (let* ((files
          (directory-files (expand-file-name org-journal-dir) t
                           (concat "^" (regexp-quote date-prefix) "-.*\.org$"))))
    (org-map-entries 'jds:update-properties "LEVEL=1" files)))

;; deprecated, to be repaced by weekly/monthly summary
(defun org-dblock-write:journal-daily-summary (params)
  ;;(jds:get-summary)
  )
(defun org-insert-dblock:journal-daily-summary ()
  "Wizard to interactively insert a journal-daily-summary dynamic block."
  (interactive)
  (let* ((params (list :name "journal-daily-summary")))
    (org-create-dblock params)
    (org-update-dblock)))
(if (fboundp 'org-dynamic-block-define)
    (org-dynamic-block-define "journal-daily-summary" 'org-insert-dblock:journal-daily-summary))

(provide 'journal-daily-summary)
