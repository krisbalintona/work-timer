;;; org-work-timer.el --- Flexible work timers with org -*- lexical-binding: t -*-

;; Copyright (C) 2023 Free Software Foundation, Inc.

;; Author: Kristoffer Balintona <krisbalintona@gmail.com>
;; Maintainer: Kristoffer Balintona <krisbalintona@gmail.com>
;; Created: 2023
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))
;; Homepage: https://github.com/krisbalintona/org-work-timer
;; Keywords: convenience

;; This file is part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Flexible work timers with org.

;;; Code:
;;; Customizable variables
(defgroup org-work-timer nil
  "Org-work-timer customization."
  :tag "Org work timer"
  :group 'org-progress)

(defcustom org-work-timer-debug nil
  "Whether log messages should be printed.
Messages will be printed to the \"*Messages*\" buffer and
`org-work-timer-log-buffer-name' buffer.")

(defcustom org-work-timer-log-buffer-name "*Org-work-timer Logs*"
  "Name of log buffer for org-work-timer.")

(defcustom org-work-timer-time-format "%h:%.2m"
  "Defines the format of the time representation in the modeline.

The acceptable formats are those taken from `format-seconds'."
  :group 'org-work-timer
  :type 'string)

(defcustom org-work-timer-sound "microwave-beep.mp3"
  "Sound played once a timer's duration finishes.
Accepted file types are those that `ffplay' can run."
  :group 'org-work-timer
  :type 'string)

;; TODO 2023-08-15: Make this a sequence of functions run in order until the
;; first non-nil value? This way, there can be fallbacks.
;; TODO 2023-08-16: Perhaps it'd be best if I allow the user to set this to
;; either a function or a number
(defcustom org-work-timer-work-duration-function
  'org-work-timer-work-duration-basic
  "Calculate the duration for work timers (in seconds).

Possible values are `org-work-timer-work-duration-basic',
`org-work-timer-work-duration-fractional',
`org-work-timer-work-duration-pomodoro'and a user-defined
function that returns the duration of a break in seconds."
  :group 'org-work-timer
  :type 'symbol)

(defcustom org-work-timer-break-duration-function
  'org-work-timer-break-duration-basic
  "Calculate the duration for work timers (in seconds).

Possible values are `org-work-timer-break-duration-basic',
`org-work-timer-break-duration-fractional',
`org-work-timer-break-duration-pomodoro' and a user-defined
function that returns the duration of a break in seconds."
  :group 'org-work-timer
  :type 'symbol)

(defcustom org-work-timer-default-work-duration 30
  "Default number of minutes for work timers."
  :group 'org-work-timer
  :type 'number)

(defcustom org-work-timer-default-break-duration 10
  "Default number of minutes for break timers."
  :group 'org-work-timer
  :type 'number)

(defcustom org-work-timer-pomodoro-work-duration 25
  "Number of minutes for Pomodoro work timers."
  :group 'org-work-timer
  :type 'number)

(defcustom org-work-timer-pomodoro-break-duration-short 5
  "Number of minutes for short (i.e. regular) Pomodoro timers."
  :group 'org-work-timer
  :type 'number)

(defcustom org-work-timer-pomodoro-break-duration-long 20
  "Number of minutes for long (after four cycles) Pomodoro timers."
  :group 'org-work-timer
  :type 'number)

(defcustom org-work-timer-fractional-work-duration 25
  "Number of minutes for fractional work timers."
  :group 'org-work-timer
  :type 'number)

(defcustom org-work-timer-fractional-break-duration-fraction 0.2
  "Fraction of work time used to determine break timer."
  :group 'org-work-timer
  :type 'number)

(defcustom org-work-timer-cycle-finished-hook nil
  "Hook run before `org-work-timer-cycle-finish' is called."
  :type 'hook
  :group 'org-work-timer)

;;; Faces
(defface org-work-timer-mode-line
  '((t (:foreground "DarkOrange" :inherit bold)))
  "Face used for timer display in mode line."
  :group 'org-work-timer)

;;; Internal variables
(defvar org-work-timer-mode-line-string ""
  "Mode line string for the current work timer.")
(put 'org-work-timer-mode-line-string 'risky-local-variable t)

(defvar org-work-timer-overrun-p nil
  "Whether running time has exceeded expected duration.")
(put 'org-work-timer-overrun-p 'risky-local-variable t)

(defvar org-work-timer-current-timer nil
  "The current work timer.")

(defvar org-work-timer-type nil
  "The type of the current timer (e.g. work, break).")

(defvar org-work-timer-pauses nil
  "A list of conses, each being a start and end time of a pause.")

(defvar org-work-timer-duration nil
  "Duration of the current work timer.")

(defvar org-work-timer-start-time nil
  "The start time of the current work timer.")

(defvar org-work-timer-pause-time nil
  "The time of the last pause in the current work timer.")

(defvar org-work-timer-end-time nil
  "The end time of the current work timer.")

(defvar org-work-timer-history nil
  "Mode line string for the current work timer.")

;;; Functions
;;;; Processing timer history
(defun org-work-timer-process-history (function predicate &optional history)
  ;; REVIEW 2023-08-23: Write a better docstring
  "Process all entries in `org-work-timer-history'.
Returns a list whose elements are the return value of FUNCTION
applied to each entry in `org-work-timer-history'. Only operate
on elements that satisfy PREDICATE. Both FUNCTION and PREDICATE
take one argument, the current entry in `org-work-timer-history'.

If HISTORY is provided, operate on that instead of
`org-work-timer-history'."
  (cl-loop for entry in (or history org-work-timer-history)
           when (funcall (or predicate 'identity) entry)
           collect (funcall function entry)))

(defun org-work-timer-elapsed-without-pauses (timer-entry)
  "Given TIMER-ENTRY, return seconds elapsed excluding pauses."
  (let ((total-elapsed
         (- (plist-get timer-entry :end)
            (plist-get timer-entry :start)))
        (time-paused
         (apply #'+ (mapcar (lambda (elt)
                              (- (or (plist-get elt :pause-end)
                                     ;; FIXME 2023-08-22: Avoid having this
                                     ;; ad-hoc solution for when :pause-end is
                                     ;; nil (i.e. we're currently in a pause)
                                     ;; (used primarily for mode line running
                                     ;; time)
                                     (float-time (current-time)))
                                 (plist-get elt :pause-start)))
                            (plist-get timer-entry :pauses)))))
    (- total-elapsed time-paused)))

(defun org-work-timer-overrun (timer-entry)
  "Given TIMER-ENTRY, return seconds overran."
  ;; REVIEW 2023-08-23: Add user option to choose whether this already returns
  ;; 0, instead?
  (let ((duration (plist-get timer-entry :expected-duration))
        (elapsed (org-work-timer-elapsed-without-pauses timer-entry)))
    (- elapsed duration)))

(defun org-work-timer-process-history-overrun (&optional predicate history)
  "Return total overrun time in `org-work-timer-history'.
Only process that satisfy PREDICATE, if supplied. If HISTORY is
provided, process that list instead."
  (apply #'+
         (org-work-timer-process-history 'org-work-timer-overrun
                                         predicate
                                         history)))

;;;; Duration functions
;;;;; Basic
(defun org-work-timer-work-duration-basic ()
  "Return `org-work-timer-default-work-duration' in seconds."
  (* 60 org-work-timer-default-work-duration))

(defun org-work-timer-break-duration-basic ()
  "Return `org-work-timer-default-break-duration' in seconds."
  (* 60 org-work-timer-default-break-duration))

;;;;; Pomodoro
(defun org-work-timer-work-duration-pomodoro ()
  "Work duration in seconds according to the Pomodoro method."
  (* 60 org-work-timer-pomodoro-work-duration))

(defun org-work-timer-break-duration-pomodoro ()
  "Break duration in seconds according to the Pomodoro method.
Also add total overrun time (which can be negative or positive)."
  (let* ((long-p (zerop (mod
                         (length
                          (org-work-timer-process-history 'identity
                                                          (lambda (entry) (eq (plist-get entry :type) 'work))))
                         4)))
         (overrun-sum (org-work-timer-process-history-overrun))
         duration)
    (setq break (+ overrun-sum
                   (if long-p
                       (* 60 org-work-timer-pomodoro-break-duration-long)
                     (* 60 org-work-timer-pomodoro-break-duration-short))))
    (org-work-timer-log "(org-work-timer-break-duration-pomodoro) Overrun: %s" overrun-sum)
    (org-work-timer-log "(org-work-timer-break-duration-pomodoro) Break duration: %s" duration)))

;;;;; Fractional
(defun org-work-timer-work-duration-fractional ()
  "\"Fractional\" work duration in seconds.
Returns the value of `org-work-timer-fractional-work-duration' in
seconds."
  (* 60 org-work-timer-fractional-work-duration))

(defun org-work-timer-break-duration-fractional ()
  "\"Fractional\" break duration in seconds.
Return, in seconds, a fraction of the time worked in the preview
work timer. This fraction is determined by the value of
`org-work-timer-fractional-break-duration-fraction'.

Also add total overrun time (which can be negative or positive)."
  (let* ((elapsed
          (org-work-timer-elapsed-without-pauses (car (last org-work-timer-history))))
         (overrun-sum (org-work-timer-process-history-overrun))
         duration)
    (setq duration (+ overrun-sum
                      (max
                       (* 60 org-work-timer-default-break-duration) ; Minimum duration
                       (* elapsed org-work-timer-fractional-break-duration-fraction))))
    (org-work-timer-log "(org-work-timer-break-duration-fractional) Overrun: %s" overrun-sum)
    (org-work-timer-log "(org-work-timer-break-duration-fractional) Break duration: %s" duration)))

;;;; Timers
(defun org-work-timer-tick ()
  "A function invoked by `org-work-timer-current-timer' each second.
Updates the mode line and plays a sound if the the duration of
the current timer is reached."
  (org-work-timer-update-mode-line)
  (let ((elapsed
         (floor (- org-work-timer-duration
                   (org-work-timer-elapsed-without-pauses
                    (list :start org-work-timer-start-time
                          :end (float-time (current-time))
                          :pauses org-work-timer-pauses))))))
    (when (and (not org-work-timer-overrun-p)
               (<= elapsed 0))
      (org-work-timer-play-sound)
      (setq org-work-timer-overrun-p t))))

(defun org-work-timer-set-timer (type duration)
  "Create a timer and set the appropriate variables.
TYPE is a symbol representing the type of the timer. DURATION is
a number representing the duration of the timer in seconds."
  (when (timerp org-work-timer-current-timer)
    (cancel-timer org-work-timer-current-timer))
  (setq org-work-timer-type type
        org-work-timer-start-time (float-time (current-time))
        org-work-timer-duration duration
        org-work-timer-end-time (float-time (time-add (current-time) duration))
        org-work-timer-pauses nil
        org-work-timer-pause-time nil
        org-work-timer-overrun-p nil
        org-work-timer-current-timer (run-with-timer t 1 'org-work-timer-tick))
  (org-work-timer-update-mode-line))

;;;; Mode line
(defun org-work-timer-update-mode-line ()
  "Set `org-work-timer-mode-line-string' appropriately."
  (let* ((type
          (capitalize (symbol-name org-work-timer-type)))
         (elapsed
          (org-work-timer-elapsed-without-pauses
           (list :start org-work-timer-start-time
                 :end (float-time (current-time))
                 :pauses org-work-timer-pauses)))
         (running
          (concat (when (< elapsed 0) "-")
                  (format-seconds org-work-timer-time-format (abs elapsed))))
         (duration
          (concat (when (< org-work-timer-duration 0) "-")
                  (format-seconds org-work-timer-time-format (abs org-work-timer-duration))))
         (mode-line-string
          (concat "[" (format "%s: %s/%s" type running duration) "] ")))
    (setq org-work-timer-mode-line-string
          (propertize mode-line-string 'face 'org-work-timer-mode-line)))
  (force-mode-line-update t))

;;;; Sound
(defun org-work-timer-play-sound ()
  "Play audio for a timer's end."
  (when-let ((sound (expand-file-name org-work-timer-sound))
             ((file-exists-p sound)))
    (unless (executable-find "ffplay")
      (user-error "Cannot play %s without `ffplay'" sound))
    (call-process-shell-command
     (format "ffplay -nodisp -autoexit %s >/dev/null 2>&1" sound) nil 0)))

;;;; Log messages
(defun org-work-timer-log (format-string &rest objects)
  "Pass FORMAT-STRING and OBJECTS to `format' and log result to log buffer.
The log buffer's name is set by `org-work-timer-log-buffer-name'."
  (when org-work-timer-debug
    (with-current-buffer (get-buffer-create org-work-timer-log-buffer-name)
      (goto-char (point-max))
      (let* ((face '(:weight bold :inherit warning))
             (timestamp (format-time-string "[%F %T] " (current-time)))
             (str (concat (propertize timestamp 'face face)
                          (apply 'format format-string objects)))
             (propertized-str
              (propertize str 'face '(:weight bold)))
             (prefix
              (propertize "[org-work-timer] " 'face face)))
        (message (concat prefix str))
        (insert propertized-str "\n")))))

;;; Commands
;;;; Timers
;;;###autoload
(defun org-work-timer-start ()
  "Start a work timer."
  (interactive)
  (org-work-timer-set-timer 'work (funcall org-work-timer-work-duration-function))
  (unless global-mode-string (setq global-mode-string '("")))
  (unless (memq 'org-work-timer-mode-line-string global-mode-string)
    (setq global-mode-string
          (append global-mode-string '(org-work-timer-mode-line-string))))
  (org-work-timer-log "(org-work-timer-start) Timer started"))

;;;###autoload
(defun org-work-timer-pause-or-continue (&optional pause-or-continue)
  "Pause or continue the current timer.

If PAUSE-OR-CONTINUE is `'pause' or `'continue', then force doing
that action."
  (interactive)
  (when (not (timerp org-work-timer-current-timer))
    (user-error "[org-work-timer] No timer running!"))
  (setq pause-or-continue (or pause-or-continue
                              (if org-work-timer-pause-time
                                  'continue
                                'pause)))
  (let (pauses-modified-p)
    (pcase pause-or-continue
      ('continue
       (when org-work-timer-pause-time    ; Do nothing if not currently paused
         (org-work-timer-log "(org-work-timer-pause-or-continue) Timer continued")
         ;; Move back `org-work-timer-end-time' for how long timer was paused
         (setq org-work-timer-end-time (float-time
                                        (time-add org-work-timer-end-time
                                                  (time-since org-work-timer-pause-time))))

         ;; Set :pause-end of pause in `org-work-timer-pauses'
         (setf (plist-get (car (last org-work-timer-pauses)) :pause-end)
               (float-time (current-time)))
         (setq org-work-timer-pause-time nil
               pauses-modified-p t)))
      ('pause
       (unless org-work-timer-pause-time  ; Do nothing if already paused
         (org-work-timer-log "(org-work-timer-pause-or-continue) Timer paused")
         (setq org-work-timer-pause-time (float-time (current-time))
               org-work-timer-pauses
               (append org-work-timer-pauses
                       (list (list :pause-start org-work-timer-pause-time :pause-end nil)))
               pauses-modified-p t))))
    ;; If actually paused or continued, it is useful to return
    ;; `org-work-timer-pauses'
    (when pauses-modified-p org-work-timer-pauses)))

;;;###autoload
(defun org-work-timer-cycle-finish ()
  "Finish the current timer cycle."
  (interactive)
  (unless (timerp org-work-timer-current-timer)
    (user-error "[org-work-timer] No timer running!"))
  (run-hooks 'org-work-timer-cycle-finished-hook)
  ;; If the user wants to end a cycle amidst a pause, then end pause first
  (when org-work-timer-pause-time
    (org-work-timer-pause-or-continue 'continue))
  (setq org-work-timer-history
        (append org-work-timer-history
                (list (list :type org-work-timer-type
                            :expected-duration org-work-timer-duration
                            :start org-work-timer-start-time
                            :end (float-time (current-time))
                            :pauses org-work-timer-pauses))))
  (pcase org-work-timer-type
    ('break
     (org-work-timer-set-timer 'work (funcall org-work-timer-work-duration-function)))
    (t
     (org-work-timer-set-timer 'break (funcall org-work-timer-break-duration-function))))
  (org-work-timer-log "(org-work-timer-cycle-finish) Cycle finished"))

;;;###autoload
(defun org-work-timer-end ()
  "End the current timer."
  (interactive)
  (when (timerp org-work-timer-current-timer)
    (cancel-timer org-work-timer-current-timer))
  (setq org-work-timer-current-timer nil
        org-work-timer-type nil
        org-work-timer-start-time nil
        org-work-timer-end-time nil
        org-work-timer-pause-time nil
        org-work-timer-pauses nil
        org-work-timer-history nil
        global-mode-string (remove 'org-work-timer-mode-line-string global-mode-string))
  (force-mode-line-update t)
  (org-work-timer-log "(org-work-timer-end) Timer ended"))

;;;###autoload
(defun org-work-timer-start-or-finish ()
  "Conditionally start a timer or finish a cycle."
  (interactive)
  (if (timerp org-work-timer-current-timer)
      (org-work-timer-cycle-finish)
    (org-work-timer-start)))

;;;; Convenience
(defun org-work-timer-report ()
  "Print the statistics of this series of timers."
  (interactive)
  (unless (timerp org-work-timer-current-timer)
    (user-error "[org-work-timer] No timer running!"))
  (let* ((timer-entries
          (append org-work-timer-history
                  (list (list :type org-work-timer-type
                              :start org-work-timer-start-time
                              :end (float-time (current-time))
                              :pauses org-work-timer-pauses))))
         (elapsed-total
          (- (plist-get (car (last timer-entries)) :end)
             (plist-get (first timer-entries) :start)))
         (work-count (length
                      (org-work-timer-process-history 'identity
                                                      (lambda (elt) (eq (plist-get elt :type) 'work)))))
         (work-sum (apply #'+
                          (org-work-timer-process-history 'org-work-timer-elapsed-without-pauses
                                                          (eq (plist-get entry :type) 'work))))
         (break-count (length
                       (org-work-timer-process-history 'identity
                                                       (lambda (elt) (eq (plist-get elt :type) 'break)))))
         (break-sum (apply #'+
                           (org-work-timer-process-history 'org-work-timer-elapsed-without-pauses
                                                           (eq (plist-get entry :type) 'break)))))
    (message "In the last %s, you had %s work sessions and %s breaks, and worked for %s and took breaks for %s."
             (format-seconds "%.2h hours and %.2m minutes" elapsed-total)
             work-count
             break-count
             (format-seconds "%.2h:%.2m:%.2s" work-sum)
             (format-seconds "%.2h:%.2m:%.2s" break-sum))))

;;;; Keymap
(defvar-keymap org-work-timer-prefix-map
  :doc "Prefix map for `org-work-timer' commands."
  "s" #'org-work-timer-start
  "p" #'org-work-timer-pause-or-continue
  "f" #'org-work-timer-cycle-finish
  "e" #'org-work-timer-end
  "w" #'org-work-timer-start-or-finish
  "r" #'org-work-timer-report)

;;; Org-clock integration
(defun org-work-timer-org-clock-in ()
  "Function added to `org-clock-in-hook'.
Either start a timer or continue an existing one if the current
timer is a work one."
  (cond
   ((not (eq org-work-timer-type 'work)))
   ((timerp org-work-timer-current-timer)
    (org-work-timer-pause-or-continue 'continue)
    (org-work-timer-log "(org-work-timer-org-clock-in) Timer continued"))
   (t
    (org-work-timer-start)
    (org-work-timer-log "(org-work-timer-org-clock-in) Timer started"))))

(defun org-work-timer-org-clock-out ()
  "Function added to `org-clock-out-hook'.
Pause the current timer if it is a work one."
  (cond
   ((not (eq org-work-timer-type 'work)))
   ((timerp org-work-timer-current-timer)
    (org-work-timer-pause-or-continue 'pause)
    (org-work-timer-log "(org-work-timer-org-clock-out) Timer paused"))))

;;;###autoload
(define-minor-mode org-work-timer-with-org-clock-mode
  "Global minor mode that integrates with org-work-timer with org-clock."
  :global t
  :group 'org-work-timer
  (cond
   (org-work-timer-with-org-clock-mode
    (add-hook 'org-clock-in-hook 'org-work-timer-org-clock-in)
    (add-hook 'org-clock-out-hook 'org-work-timer-org-clock-out)
    (add-hook 'org-clock-cancel-hook 'org-work-timer-org-clock-out))
   (t
    (remove-hook 'org-clock-in-hook 'org-work-timer-org-clock-in)
    (remove-hook 'org-clock-out-hook 'org-work-timer-org-clock-out)
    (remove-hook 'org-clock-cancel-hook 'org-work-timer-org-clock-out))))

(provide 'org-work-timer)
;;; org-work-timer.el ends here
