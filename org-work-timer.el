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

;; TODO 2023-08-16: Change to be more sensible
(defcustom org-work-timer-time-format "%M:%S"
  "Defines the format of the time representation in the modeline."
  :group 'org-work-timer
  :type 'string)

(defcustom org-work-timer-audio-player (or (executable-find "aplay")
                                           (executable-find "afplay"))
  "Music player used to play sounds."
  :group 'org-work-timer
  :type 'string)

;; TODO 2023-08-15: Make this a sequence of functions run in order until the
;; first non-nil value? This way, there can be fallbacks.
;; TODO 2023-08-16: Perhaps it'd be best if I allow the user to set this to
;; either a function or a number
(defcustom org-work-timer-work-duration-function
  'org-work-timer-work-duration-fractional
  "This function calculates the duration for work timers (in seconds).

Possible values are `org-work-timer-work-duration-basic' and a
user-defined function that returns the duration of a break in
seconds."
  :group 'org-work-timer
  :type 'symbol)

(defcustom org-work-timer-break-duration-function
  'org-work-timer-break-duration-fractional
  "This function calculates the duration for work timers (in seconds).

Possible values are `org-work-timer-break-duration-basic',
`org-work-timer-break-duration-fractional', and a user-defined
function that returns the duration of a break in seconds."
  :group 'org-work-timer
  :type 'symbol)

;; TODO 2023-08-16: Change to be more sensible
(defcustom org-work-timer-default-work-duration 0.25
  "Default number of minutes for timers."
  :group 'org-work-timer
  :type 'number)

;; TODO 2023-08-16: Change to be more sensible
(defcustom org-work-timer-default-break-duration 0.17
  "Default number of minutes for timers."
  :group 'org-work-timer
  :type 'number)

;; TODO 2023-08-16: Change to be more sensible
(defcustom org-work-timer-pomodoro-work-duration 1
  "Number of minutes for Pomodoro timers."
  :group 'org-work-timer
  :type 'number)

;; TODO 2023-08-16: Change to be more sensible
(defcustom org-work-timer-pomodoro-break-duration-short 0.25
  "Number of minutes for short (regular) Pomodoro timers."
  :group 'org-work-timer
  :type 'number)

;; TODO 2023-08-16: Change to be more sensible
(defcustom org-work-timer-pomodoro-break-duration-long 0.5
  "Number of minutes for long (after four cycles) Pomodoro timers."
  :group 'org-work-timer
  :type 'number)

;; TODO 2023-08-16: Change to be more sensible
(defcustom org-work-timer-fractional-work-duration 0.5
  "Number of minutes for fractional work timers."
  :group 'org-work-timer
  :type 'number)

;; TODO 2023-08-16: Change to be more sensible
(defcustom org-work-timer-fractional-break-duration-fraction 0.25
  "Fraction of work time used to determine break timer."
  :group 'org-work-timer
  :type 'number)

;;; Internal variables

;; FIXME 2023-08-21: Make variable names consistent

(defvar org-work-timer-current-timer nil
  "The current work timer.")

(defvar org-work-timer-type nil
  "The type of the current timer (e.g. work, break).")

(defvar org-work-timer-pauses nil
  "A list of conses, each being a start and end time of a pause.")

(defvar org-work-timer-current-timer-duration nil
  "Duration of the current work timer.")

(defvar org-work-timer-start-time nil
  "The start time of the current work timer.")

(defvar org-work-timer-pause-time nil
  "The time of the last pause in the current work timer.")

(defvar org-work-timer-end-time nil
  "The end time of the current work timer.")

(defvar org-work-timer-mode-line-string ""
  "Mode line string for the current work timer.")
(put 'org-work-timer-mode-line-string 'risky-local-variable t)

(defvar org-work-timer-history nil
  "Mode line string for the current work timer.")

;;; Functions
;;;; Duration functions

(defun org-work-timer-elapsed-without-pauses (history-entry)
  "Given HISTORY-ENTRY, return time elapsed excluding pauses."
  (let ((total-elapsed
         (- (plist-get history-entry :end)
            (plist-get history-entry :start)))
        (time-paused
         (apply #'+ (mapcar (lambda (elt)
                              (- (or (plist-get elt :pause-end)
                                     ;; REVIEW 2023-08-22: Is it okay to ad-hoc
                                     ;; use `current-time' if :pause-end is nil
                                     ;; (i.e. we're currently in a pause)?
                                     (float-time (current-time)))
                                 (plist-get elt :pause-start)))
                            (plist-get history-entry :pauses)))))
    (- total-elapsed time-paused)))

;;;;; Basic
(defun org-work-timer-work-duration-basic ()
  "Simply return `org-work-timer-default-work-duration' in seconds."
  (* 60 org-work-timer-default-work-duration))

(defun org-work-timer-break-duration-basic ()
  "Simply return `org-work-timer-default-break-duration' in seconds."
  (* 60 org-work-timer-default-break-duration))

;;;;; Pomodoro
(defun org-work-timer-work-duration-pomodoro ()
  "Work duration according to the Pomodoro method."
  (* 60 org-work-timer-pomodoro-work-duration))

(defun org-work-timer-break-duration-pomodoro ()
  "Break duration according to the Pomodoro method."
  (if (zerop (mod (cl-count-if
                   (lambda (elt) (eq (plist-get elt :type) 'work))
                   org-work-timer-history)
                  4))
      (* 60 org-work-timer-pomodoro-break-duration-long)
    (* 60 org-work-timer-pomodoro-break-duration-short)))

;;;;; Fractional
(defun org-work-timer-work-duration-fractional ()
  "Work duration according to the Pomodoro method."
  (* 60 org-work-timer-fractional-work-duration))

(defun org-work-timer-break-duration-fractional ()
  "Break duration according to the Pomodoro method."
  (let* ((work-period (car (last org-work-timer-history)))
         (elapsed-sum (- (plist-get work-period :end) (plist-get work-period :start)))
         (pause-sum
          (apply #'+ (cl-loop for pause in (plist-get work-period :pauses)
                              collect (- (plist-get pause :pause-end)
                                         (plist-get pause :pause-start))))))
    (* (- elapsed-sum pause-sum) org-work-timer-fractional-break-duration-fraction)))

;;;; Timers
(defun org-work-timer-tick ()
  "A callback that is invoked by the running timer each second.
It checks whether we reached the duration of the current phase,
when 't it invokes the handlers for finishing."
  (org-work-timer-update-mode-line)
  (when (equal (floor (- org-work-timer-current-timer-duration
                         (org-work-timer-elapsed-without-pauses
                          (list :start org-work-timer-start-time
                                :end (float-time (current-time))
                                :pauses org-work-timer-pauses))))
               0)
    (org-work-timer-play-sound)))

(defun org-work-timer-set-timer (type duration)
  "Create a timer and set the appropriate variables.
TYPE is a symbol representing the type of the timer. DURATION is
in seconds.

If the optional arguments START and END are provided,
`org-work-timer-start-time' and `org-work-timer-end-time' will be
set manually."
  (when (timerp org-work-timer-current-timer)
    (cancel-timer org-work-timer-current-timer))
  (setq org-work-timer-type type
        org-work-timer-start-time (float-time (current-time))
        org-work-timer-current-timer-duration duration
        org-work-timer-end-time (float-time (time-add (current-time) duration))
        org-work-timer-pauses nil
        org-work-timer-pause-time nil
        org-work-timer-current-timer (run-with-timer t 1 'org-work-timer-tick))
  (org-work-timer-update-mode-line))

;;;; Mode line
(defun org-work-timer-update-mode-line ()
  "Set `org-work-timer-mode-line-string'."
  (let ((running (org-work-timer-elapsed-without-pauses
                  (list :start org-work-timer-start-time
                        :end (float-time (current-time))
                        :pauses org-work-timer-pauses)))
        (duration
         (format-time-string org-work-timer-time-format org-work-timer-current-timer-duration)))
    (setq org-work-timer-mode-line-string
          (concat "[" (format "%s: %s/%s"
                              org-work-timer-type
                              (format-time-string org-work-timer-time-format running)
                              duration)
                  "] ")))
  (force-mode-line-update t))

;;;; Sound
(defun org-work-timer-play-sound ()
  "Play audio for a timer's end."
  ;; FIXME 2023-08-16: Change this sound and add an user option for it
  (let ((sound "/home/krisbalintona/.emacs.d/elpaca/builds/org-pomodoro/resources/bell.wav")
        (args nil))                     ; FIXME 2023-08-15: Remove ARGS?
    (cond ((and (fboundp 'sound-wav-play) sound)
           (sound-wav-play sound))
          ((and org-work-timer-audio-player sound)
           (start-process-shell-command
            "org-work-timer-audio-player" nil
            (mapconcat 'identity
                       `(,org-work-timer-audio-player
                         ,@(delq nil (list args (shell-quote-argument (expand-file-name sound)))))
                       " "))))))

;;; Commands
;;;###autoload
(defun org-work-timer-start ()
  "Start a work timer."
  (interactive)
  (org-work-timer-set-timer 'work (funcall org-work-timer-work-duration-function))
  ;; Add to `global-mode-string'
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
        global-mode-string (remove 'org-work-timer-mode-line-string global-mode-string)))

(defvar-keymap org-work-timer-prefix-map
  :doc "Prefix map for `org-work-timer' commands."
  "s" #'org-work-timer-start
  "p" #'org-work-timer-pause-or-continue
  "f" #'org-work-timer-cycle-finish
  "e" #'org-work-timer-end)

(provide 'org-work-timer)
;;; org-work-timer.el ends here
