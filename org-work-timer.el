;;; org-work-timer.el --- Flexible work timers with org -*- lexical-binding: t -*-

;; Copyright (C) 2023 Free Software Foundation, Inc.

;; Author: Kristoffer Balintona <krisbalintona@gmail.com>
;; Maintainer: Kristoffer Balintona <krisbalintona@gmail.com>
;; Created: 2023
;; Version: 0.1
;; Package-Requires: ((emacs "24.1"))
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

;;;; Customizable variables
(defgroup org-work-timer nil
  "Org-work-timer customization."
  :tag "Org work timer"
  :group 'org-progress)

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
(defcustom org-work-timer-work-duration-function
  'org-work-timer-work-duration-basic
  "This function calculates the duration for work timers (in seconds)."
  :group 'org-work-timer
  :type 'symbol)

(defcustom org-work-timer-break-duration-function
  'org-work-timer-break-duration-basic
  "This function calculates the duration for work timers (in seconds)."
  :group 'org-work-timer
  :type 'symbol)

(defcustom org-work-timer-default-work-duration
  0.25
  "Default number of minutes for timers."
  :group 'org-work-timer
  :type 'number)

(defcustom org-work-timer-default-break-duration
  0.17
  "Default number of minutes for timers."
  :group 'org-work-timer
  :type 'number)

(defcustom org-work-timer-duration-fraction
  0.12
  "Fraction of work time that turns into break time."
  :group 'org-work-timer
  :type 'number)

;;;; Internal variables

(defvar org-work-timer-current-timer nil
  "The current work timer.")

(defvar org-work-timer-current-timer-type 'none
  "The current work timer's type (e.g. work, break).")

(defvar org-work-timer-current-timer-duration nil
  "Duration of the current work timer.")

(defvar org-work-timer-start-time nil
  "The start time of the current work timer.")

(defvar org-work-timer-pause-time nil
  "The time of the last pause in the current work timer.")

(defvar org-work-timer-end-time nil
  "The end time of the current work timer.")

(defvar org-work-timer-mode-line ""
  "Mode line string for the current work timer.")
(put 'org-work-timer-mode-line 'risky-local-variable t)

;;;; Functions

(defun org-work-timer-play-sound-end ()
  "Play audio for a timer's end."
  (let ((sound "/home/krisbalintona/.emacs.d/elpaca/builds/org-pomodoro/resources/bell.wav")
        (args nil))                     ; FIXME 2023-08-15: Remove ARGS
    (cond ((and (fboundp 'sound-wav-play) sound)
           (sound-wav-play sound))
          ((and org-pomodoro-audio-player sound)
           (start-process-shell-command
            "org-work-timer-audio-player" nil
            (mapconcat 'identity
                       `(,org-work-timer-audio-player
                         ,@(delq nil (list args (shell-quote-argument (expand-file-name sound)))))
                       " "))))))

(defun org-work-timer-tick ()
  "A callback that is invoked by the running timer each second.
It checks whether we reached the duration of the current phase, when 't it
invokes the handlers for finishing."
  (org-work-timer-update-mode-line)
  (when (equal (floor (org-work-timer-remaining-seconds)) 0)
    (org-work-timer-play-sound-end)))

(defun org-work-timer-work-duration-basic ()
  "Simply return `org-work-timer-default-work-duration' in seconds."
  (* 60 org-work-timer-default-work-duration))

(defun org-work-timer-break-duration-basic ()
  "Simply return `org-work-timer-default-break-duration' in seconds."
  (* 60 org-work-timer-default-break-duration))

(defun org-work-timer-calculate-duration-fractional ()
  "Simply return pp`org-work-timer-default-work-duration' in seconds."
  (* 60 org-work-timer-default-work-duration))

(defun org-work-timer-remaining-seconds ()
  "Return the number of seconds remaining in the current timer."
  (float-time (time-since org-work-timer-end-time)))

(defun org-work-timer-update-mode-line ()
  "Set the modeline string."
  (let ((running (time-since org-work-timer-start-time))
        (duration org-work-timer-current-timer-duration))
    (setq org-work-timer-mode-line
          (concat "[" (format "%s: %s/%s"
                              org-work-timer-current-timer-type
                              (format-time-string org-work-timer-time-format running)
                              (format-time-string org-work-timer-time-format duration))
                  "] ")))
  (force-mode-line-update t))

(defun org-work-timer-set-timer (type duration &optional start end)
  "Create a timer and sets the appropriate variables.
TYPE is a symbol representing the type of the timer. DURATION is in seconds.

If the optional arguments START and END are provided, `org-work-timer-start-time' and `org-work-timer-end-time' will be set manually."
  (when (timerp org-work-timer-current-timer)
    (cancel-timer org-work-timer-current-timer))
  (setq org-work-timer-start-time (or start (current-time))
        org-work-timer-pause-time nil
        org-work-timer-current-timer-duration duration
        org-work-timer-end-time (or end (time-add (current-time) duration))
        org-work-timer-current-timer (run-with-timer t 1 'org-work-timer-tick)
        org-work-timer-current-timer-type type)
  (org-work-timer-update-mode-line))

;;;; Commands

(defun org-work-timer-start ()
  "Start a work timer."
  (interactive)
  (org-work-timer-set-timer 'work (funcall org-work-timer-work-duration-function))
  ;; Add to `global-mode-string'
  (setq global-mode-string (append global-mode-string '(org-work-timer-mode-line))))

(defun org-work-timer-pause-or-continue ()
  "Pause or continue the current timer."
  (interactive)
  (if org-work-timer-pause-time
      (let ((time-since-pause (time-since org-work-timer-pause-time)))
        (org-work-timer-set-timer org-work-timer-current-timer-type
                                  (funcall org-work-timer-work-duration-function)
                                  (time-add org-work-timer-start-time time-since-pause)
                                  (time-add org-work-timer-end-time time-since-pause)))
    (when (timerp org-work-timer-current-timer)
      (cancel-timer org-work-timer-current-timer))
    (setq org-work-timer-pause-time (current-time))))

(defun org-work-timer-cycle-finish ()
  "Finish the current timer cycle."
  (interactive)
  (pcase org-work-timer-current-timer-type
    ('break
     (org-work-timer-set-timer 'work (funcall org-work-timer-work-duration-function)))
    (t
     (org-work-timer-set-timer 'break (funcall org-work-timer-break-duration-function)))))

(defun org-work-timer-end ()
  "End the current timer."
  (interactive)
  (when (timerp org-work-timer-current-timer)
    (cancel-timer org-work-timer-current-timer))
  (setq org-work-timer-start-time nil
        org-work-timer-end-time nil
        org-work-timer-pause-time nil
        org-work-timer-current-timer-type nil
        global-mode-string (remove 'org-work-timer-mode-line global-mode-string)))

(provide 'org-work-timer)
;;; org-work-timer.el ends here
