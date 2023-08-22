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

(defcustom org-work-timer-time-format "%h:%.2m"
  "Defines the format of the time representation in the modeline.

The acceptable formats are those taken from `format-seconds'."
  :group 'org-work-timer
  :type 'string)

(defcustom org-work-timer-sound "microwave-beep.mp3"
  "Sound played once a timer's duration finishes."
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

(defcustom org-work-timer-cycle-finished-hook '(org-work-timer-early-or-overrun-to-bank)
  "Hook run before `org-work-timer-cycle-finish' is called."
  :type 'hook
  :group 'org-work-timer)

;;; Faces
(defface org-work-timer-mode-line '((t (:foreground "DeepSkyBlue")))
  "Face used for timer display in mode line."
  :group 'org-work-timer)

;;; Internal variables
(defvar org-work-timer-mode-line-string ""
  "Mode line string for the current work timer.")
(put 'org-work-timer-mode-line-string 'risky-local-variable t)

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

(defvar org-work-timer-time-bank 0
  "Seconds held in a \"bank\".")

;;; Functions
;;;; Duration functions
(defun org-work-timer-elapsed-without-pauses (timer-entry)
  "Given TIMER-ENTRY, return time elapsed excluding pauses."
  (let ((total-elapsed
         (- (plist-get timer-entry :end)
            (plist-get timer-entry :start)))
        (time-paused
         (apply #'+ (mapcar (lambda (elt)
                              (- (or (plist-get elt :pause-end)
                                     ;; REVIEW 2023-08-22: Is it okay to ad-hoc
                                     ;; use `current-time' if :pause-end is nil
                                     ;; (i.e. we're currently in a pause)?
                                     (float-time (current-time)))
                                 (plist-get elt :pause-start)))
                            (plist-get timer-entry :pauses)))))
    (- total-elapsed time-paused)))

;;;;; Basic
(defun org-work-timer-work-duration-basic ()
  "Return `org-work-timer-default-work-duration' in seconds."
  (* 60 org-work-timer-default-work-duration))

(defun org-work-timer-break-duration-basic ()
  "Return `org-work-timer-default-break-duration' in seconds."
  (+ org-work-timer-time-bank
     (* 60 org-work-timer-default-break-duration)))

;;;;; Pomodoro
(defun org-work-timer-work-duration-pomodoro ()
  "Work duration in seconds according to the Pomodoro method."
  (* 60 org-work-timer-pomodoro-work-duration))

(defun org-work-timer-break-duration-pomodoro ()
  "Break duration in seconds according to the Pomodoro method."
  (+ org-work-timer-time-bank
     (if (zerop (mod (cl-count-if
                      (lambda (elt) (eq (plist-get elt :type) 'work))
                      org-work-timer-history)
                     4))
         (* 60 org-work-timer-pomodoro-break-duration-long)
       (* 60 org-work-timer-pomodoro-break-duration-short))))

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
`org-work-timer-fractional-break-duration-fraction'."
  (let* ((work-period (car (last org-work-timer-history)))
         (elapsed-sum (- (plist-get work-period :end)
                         (plist-get work-period :start)))
         (pause-sum
          (apply #'+ (cl-loop for pause in (plist-get work-period :pauses)
                              collect (- (plist-get pause :pause-end)
                                         (plist-get pause :pause-start))))))
    (+ org-work-timer-time-bank
       (* (- elapsed-sum pause-sum) org-work-timer-fractional-break-duration-fraction))))

;;;; Timers
(defun org-work-timer-tick ()
  "A function invoked by `org-work-timer-current-timer' each second.
Updates the mode line and plays a sound if the the duration of
the current timer is reached."
  (org-work-timer-update-mode-line)
  (when (equal (floor (- org-work-timer-duration
                         (org-work-timer-elapsed-without-pauses
                          (list :start org-work-timer-start-time
                                :end (float-time (current-time))
                                :pauses org-work-timer-pauses))))
               0)
    (org-work-timer-play-sound)))

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
        org-work-timer-current-timer (run-with-timer t 1 'org-work-timer-tick))
  (org-work-timer-update-mode-line))

;;;; Mode line
(defun org-work-timer-update-mode-line ()
  "Set `org-work-timer-mode-line-string' appropriately."
  (let* ((type
          (capitalize (symbol-name org-work-timer-type)))
         (running
          (format-seconds org-work-timer-time-format
                          (org-work-timer-elapsed-without-pauses
                           (list :start org-work-timer-start-time
                                 :end (float-time (current-time))
                                 :pauses org-work-timer-pauses))))
         (duration
          (format-seconds org-work-timer-time-format org-work-timer-duration))
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

;;;; Other
(defun org-work-timer-early-or-overrun-to-bank ()
  "Add time or subtract to bank if early or overrunning in break."
  (when (eq org-work-timer-type 'break)
    (let ((bank
           (org-work-timer-elapsed-without-pauses
            (list :start (float-time (current-time))
                  :end org-work-timer-end-time
                  :pauses org-work-timer-pauses))))
      (message "[org-work-timer] Adding %s seconds to bank."
               (format-seconds "%.2m:%.2s" bank))
      (setq org-work-timer-time-bank bank))))

;;; Commands
;;;; Timers
;;;###autoload
(defun org-work-timer-start ()
  "Start a work timer."
  (interactive)
  (setq org-work-timer-time-bank 0)
  (org-work-timer-set-timer 'work (funcall org-work-timer-work-duration-function))
  (setq global-mode-string (append global-mode-string '(org-work-timer-mode-line-string))))

;;;###autoload
(defun org-work-timer-pause-or-continue ()
  "Pause or continue the current timer."
  (interactive)
  (cond
   ((not (timerp org-work-timer-current-timer)) (user-error "No timer running!"))
   (org-work-timer-pause-time
    ;; Move back `org-work-timer-end-time' for how long timer was paused
    (setq org-work-timer-end-time (float-time
                                   (time-add org-work-timer-end-time
                                             (time-since org-work-timer-pause-time))))

    ;; Add pause period to `org-work-timer-pauses'
    (setf (plist-get (car (last org-work-timer-pauses)) :pause-end)
          (float-time (current-time)))
    (setq org-work-timer-pause-time nil))
   (t
    (setq org-work-timer-pause-time (float-time (current-time)))
    (setq org-work-timer-pauses
          (append org-work-timer-pauses
                  (list (list :pause-start org-work-timer-pause-time :pause-end nil)))))))

;;;###autoload
(defun org-work-timer-cycle-finish ()
  "Finish the current timer cycle."
  (interactive)
  (run-hooks 'org-work-timer-cycle-finished-hook)
  ;; If the user wants to end a cycle amidst a pause, then call
  ;; `org-work-timer-pause-or-continue' to end pause (setting variables
  ;; accordingly)
  (when org-work-timer-pause-time
    (org-work-timer-pause-or-continue))
  (setq org-work-timer-history
        (append org-work-timer-history
                (list (list :type org-work-timer-type
                            :start org-work-timer-start-time
                            :end (float-time (current-time))
                            :pauses org-work-timer-pauses))))
  (pcase org-work-timer-type
    ('break
     (org-work-timer-set-timer 'work (funcall org-work-timer-work-duration-function)))
    (t
     (org-work-timer-set-timer 'break (funcall org-work-timer-break-duration-function)))))

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
        org-work-timer-time-bank 0
        global-mode-string (remove 'org-work-timer-mode-line-string global-mode-string))
  (force-mode-line-update t))

;;;; Convenience
(defun org-work-timer-statistics ()
  "Print the statistics of this series of timers."
  (interactive)
  (let* ((timer-entries
          (append org-work-timer-history
                  (list (list :type org-work-timer-type
                              :start org-work-timer-start-time
                              :end (float-time (current-time))
                              :pauses org-work-timer-pauses))))
         (elapsed
          (- (plist-get (car (last timer-entries)) :end)
             (plist-get (first timer-entries) :start)))
         (work-count
          (cl-count-if (lambda (elt) (eq (plist-get elt :type) 'work))
                       timer-entries))
         (work-sum
          (apply #'+ (cl-loop for entry in timer-entries
                              if (eq (plist-get entry :type) 'work)
                              collect (org-work-timer-elapsed-without-pauses entry))))
         (break-count
          (cl-count-if (lambda (elt) (eq (plist-get elt :type) 'break))
                       timer-entries))
         (break-sum
          (apply #'+ (cl-loop for entry in timer-entries
                              if (eq (plist-get entry :type) 'break)
                              collect (org-work-timer-elapsed-without-pauses entry)))))
    (message "In the last %s, you had %s work sessions and %s breaks, and worked for %s and took breaks for %s."
             (format-seconds "%.2h hours and %.2m mintues" elapsed)
             work-count
             break-count
             (format-seconds "%.2h:%.2m:%.2s" work-sum)
             (format-seconds "%.2h:%.2m:%.2s" break-sum))))

;;; Keymap
(defvar-keymap org-work-timer-prefix-map
  :doc "Prefix map for `org-work-timer' commands."
  "s" #'org-work-timer-start
  "p" #'org-work-timer-pause-or-continue
  "f" #'org-work-timer-cycle-finish
  "e" #'org-work-timer-end)

(provide 'org-work-timer)
;;; org-work-timer.el ends here
