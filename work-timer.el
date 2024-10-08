;;; work-timer.el --- Flexible work timers  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Kristoffer Balintona

;; Author: Kristoffer Balintona <krisbalintona@gmail.com>
;; Version: 1.0
;; Package-Requires: ((emacs "28.1") (compat "29.1.3.0"))
;; Keywords: convenience, tools
;; URL: https://github.com/krisbalintona/org-work-timer

;; This program is free software; you can redistribute it and/or modify
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

;; Work-timer is a flexible package for displaying a timer in the mode line. The
;; timer counts up, and when it reaches its set duration, a sound is played.
;; This package stands out by automatically determining the timer duration based
;; on simple, user-defined functions (with functions for popular use cases
;; built-in). Notably, these functions can take into account the history of
;; completed timers, such as their durations and total running time. This makes
;; for a means to create timers in a DWIM fashion whose durations reliably
;; conform to your expectations (while allowing for the freedom to deviate when
;; things don't go according to play).

;; As such, this work-timer is particularly useful for users who want to manage
;; their work and break periods effectively, such as students aiming to be more
;; productive. For example, the package makes it easy to work for a target of 25
;; minutes by creating a "work" timer, then, when taking a break, creating a
;; "break" timer for 25% of the work time. Moreover, if the user works for only
;; 15 minutes or even 50 minutes, the break timer can still be 25% of the time
;; worked (or not, if the user configures the appropriate function to do so).

;;; Code:
(require 'notifications)
(require 'transient)

;;; Customizable variables
(defgroup work-timer nil
  "Work-timer customization."
  :group 'environment
  :link '(url-link :tag "Homepage" "https://github.com/krisbalintona/work-timer"))

(defcustom work-timer-debug nil
  "Whether log messages should be printed.
Messages will be printed to the \"*Messages*\" buffer and
`work-timer-log-buffer-name' buffer."
  :group 'work-timer
  :type 'boolean)

(defcustom work-timer-log-buffer-name "*Work-timer Logs*"
  "Name of log buffer for work-timer."
  :group 'work-timer
  :type 'string)

(defcustom work-timer-time-format "%h:%.2m"
  "Defines the format of the time representation in the modeline.

The acceptable formats are those taken from `format-seconds'."
  :group 'work-timer
  :type 'string)

(defcustom work-timer-sound
  (expand-file-name "simple-notification.mp3" (file-name-directory (locate-library "work-timer")))
  "Sound played once a timer's duration finishes.
Accepted file types are those that `ffplay' can run."
  :group 'work-timer
  :type 'file)

(defcustom work-timer-notifications-p t
  "Whether notifications are sent on timer's expected end."
  :group 'work-timer
  :type 'boolean)

;; TODO 2023-08-15: Make this a sequence of functions run in order until the
;; first non-nil value? This way, there can be fallbacks.
;; TODO 2023-08-16: Perhaps it'd be best if I allow the user to set this to
;; either a function or a number
(defcustom work-timer-work-duration-function
  'work-timer-work-duration-basic
  "Calculate the duration for work timers (in seconds).
Work-timer provides several built-in options, or users can provide their
own function that returns the duration of a break in seconds."
  :group 'work-timer
  :type '(choice
          (function :tag "Basic" work-timer-work-duration-basic)
          (function :tag "Pomodoro" work-timer-work-duration-pomodoro)
          (function :tag "Fractional" work-timer-work-duration-fractional)
          (function :Other)))

(defcustom work-timer-break-duration-function
  'work-timer-break-duration-basic
  "Calculate the duration for work timers (in seconds).
Work-timer provides several built-in options, or users can provide their
own function that returns the duration of a break in seconds."
  :group 'work-timer
  :type '(choice
          (function :tag "Basic" work-timer-break-duration-basic)
          (function :tag "Pomodoro" work-timer-break-duration-pomodoro)
          (function :tag "Fractional" work-timer-break-duration-fractional)
          (function :Other)))

(defcustom work-timer-default-work-duration 30
  "Default number of minutes for work timers."
  :group 'work-timer
  :type 'natnum)

(defcustom work-timer-default-break-duration 10
  "Default number of minutes for break timers."
  :group 'work-timer
  :type 'natnum)

(defcustom work-timer-pomodoro-work-duration 25
  "Number of minutes for Pomodoro work timers."
  :group 'work-timer
  :type 'natnum)

(defcustom work-timer-pomodoro-break-duration-short 5
  "Number of minutes for short (i.e. regular) Pomodoro timers."
  :group 'work-timer
  :type 'natnum)

(defcustom work-timer-pomodoro-break-duration-long 20
  "Number of minutes for long (after four cycles) Pomodoro timers."
  :group 'work-timer
  :type 'natnum)

(defcustom work-timer-fractional-work-duration 25
  "Number of minutes for fractional work timers."
  :group 'work-timer
  :type 'natnum)

(defcustom work-timer-fractional-break-duration-fraction 0.2
  "Fraction of work time used to determine break timer."
  :group 'work-timer
  :type 'float)

(defcustom work-timer-break-add-surplus-p t
  "Whether to add surplus time for break timers.
The built-in break duration functions leverage
`work-timer--surplus-prompt' to prompt then add the surplus time if this
value is non-nil. User-defined break functions can choose to call
`work-timer--surplus-prompt' or incorporate surplus time another way."
  :type 'boolean
  :group 'work-timer)

(defcustom work-timer-break-force-surplus-p nil
  "Whether to skip prompting for surplus time for break timers.
Currently,the built-in break duration functions leverage
`work-timer--surplus-prompt' to add surplus time (if
`work-timer-break-add-surplus-p' is non-nil). By default,
`work-timer--surplus-prompt' prompts the user to confirm to add the
surplus time. When this variable is non-nil, the user will not be
prompted and instead the surplus time immediately added."
  :type 'boolean
  :group 'work-timer)

(defcustom work-timer-cycle-finish-hook nil
  "Hook run after `work-timer-cycle-finish' is called."
  :group 'work-timer
  :type 'hook)

(defcustom work-timer-dwim-hook nil
  "Hook run after `work-timer-dwim' is called."
  :group 'work-timer
  :type 'hook)

;;; Faces
;; TODO 2024-01-24: Consider, for the instances in which this face is used,
;; calling a variable instead. That variable can either have this face as a
;; value or a function which determines that face. This way, I can build in an
;; accessible entry point for my `kb/work-timer-set-faces'
(defface work-timer-mode-line
  '((t (:foreground "DarkOrange" :inherit bold)))
  "Face used for timer display in mode line."
  :group 'work-timer)

;;; Internal variables
(defvar work-timer-mode-line-string ""
  "Mode line string for the current work timer.")
(put 'work-timer-mode-line-string 'risky-local-variable t)

(defvar work-timer-overrun-p nil
  "Whether running time has exceeded expected duration.")
(put 'work-timer-overrun-p 'risky-local-variable t)

(defvar work-timer-current-timer nil
  "The current work timer.")

(defvar work-timer-type nil
  "The type of the current timer (e.g. work, break).")

(defvar work-timer-pauses nil
  "A list of conses, each being a start and end time of a pause.")

;; TODO 2023-08-23: Consider having this be either a number, as it is now, or a
;; list of numbers, or both
(defvar work-timer-duration nil
  "Duration of the current work timer.")

(defvar work-timer-start-time nil
  "The start time of the current work timer.")

(defvar work-timer-pause-time nil
  "The time of the last pause in the current work timer.")

(defvar work-timer-end-time nil
  "The end time of the current work timer.")

(defvar work-timer-history nil
  "Mode line string for the current work timer.")

;;; Functions
;;;; Log messages
(defun work-timer--log (format-string &rest objects)
  "Pass FORMAT-STRING and OBJECTS to `format' and log result to log buffer.
The log buffer's name is set by `work-timer-log-buffer-name'."
  (when work-timer-debug
    (with-current-buffer (get-buffer-create work-timer-log-buffer-name)
      (goto-char (point-max))
      (let* ((face '(:weight bold :inherit warning))
             (timestamp (format-time-string "[%F %T] " (current-time)))
             (str (concat (propertize timestamp 'face face)
                          (apply #'format format-string objects)))
             (propertized-str
              (propertize str 'face '(:weight bold)))
             (prefix
              (propertize "[work-timer] " 'face face)))
        (message (concat prefix str))
        (insert propertized-str "\n")))))

;;;; Timers
(defun work-timer--set-timer (type duration &optional start)
  "Create a timer and set the appropriate variables.
TYPE is a symbol representing the type of the timer. DURATION is
a number representing the duration of the timer in seconds.

Additionally, the optional argument START can be provided to set
the start time of the timer. Should be in the format returned by
`float-time', i.e., a float number of seconds since the epoch."
  (when (timerp work-timer-current-timer)
    (cancel-timer work-timer-current-timer))
  (setq work-timer-type type
        work-timer-start-time (or start (float-time (current-time)))
        work-timer-duration duration
        work-timer-end-time (float-time (time-add (current-time) duration))
        work-timer-pauses nil
        work-timer-pause-time nil
        work-timer-overrun-p nil
        work-timer-current-timer (run-with-timer t 1 'work-timer--tick))
  (work-timer--update-mode-line))

;;;; Mode line
(defun work-timer--update-mode-line ()
  "Set `work-timer-mode-line-string' appropriately."
  (let* ((type-string
          (capitalize (symbol-name work-timer-type)))
         (elapsed (work-timer--elapsed-without-pauses
                   (list :start work-timer-start-time
                         :end (float-time (current-time))
                         :pauses work-timer-pauses)))
         (running-string
          (concat (when (< elapsed 0) "-")
                  (format-seconds work-timer-time-format (abs elapsed))))
         (duration-string
          (concat (when (< work-timer-duration 0) "-")
                  (format-seconds work-timer-time-format (abs work-timer-duration))))
         (mode-line-string
          (format (propertize "[%s: %s/%s] " 'face 'work-timer-mode-line)
                  type-string
                  (if (< work-timer-duration elapsed)
                      (propertize running-string 'face 'org-mode-line-clock-overrun)
                    running-string)
                  duration-string)))
    (setq work-timer-mode-line-string mode-line-string))
  (force-mode-line-update t))

(defun work-timer--play-sound ()
  "Play audio for a timer's end."
  (when-let ((sound work-timer-sound)
             ((file-exists-p sound)))
    (unless (executable-find "ffplay")
      (user-error "Cannot play %s without `ffplay'" sound))
    (call-process-shell-command
     (format "ffplay -nodisp -autoexit %s >/dev/null 2>&1" sound) nil 0))
  (when work-timer-notifications-p
    (notifications-notify
     :title "Emacs: work-timer"
     :body (format "%s timer with <b>expected duration</b> of <b>%s reached!</b>"
                   (capitalize (symbol-name work-timer-type))
                   ;; duration-string from `work-timer--update-mode-line'
                   (concat (when (< work-timer-duration 0) "-")
                           (format-seconds work-timer-time-format (abs work-timer-duration)))))))

(defun work-timer--tick ()
  "A function invoked by `work-timer-current-timer' each second.
Updates the mode line and plays a sound if the the duration of
the current timer is reached."
  (work-timer--update-mode-line)
  (let ((elapsed
         (floor (- work-timer-duration
                   (work-timer--elapsed-without-pauses
                    (list :start work-timer-start-time
                          :end (float-time (current-time))
                          :pauses work-timer-pauses))))))
    (when (and (not work-timer-overrun-p)
               (<= elapsed 0))
      (work-timer--play-sound)
      (setq work-timer-overrun-p t))))

;;;; User timer duration prompts
(defun work-timer--seconds-to-token (time)
  "Un-parse TIME.
TIME is a number that represents a quantity of seconds. The
resultant token has a format that follows the one described in
the docstring of `work-timer--duration-prompt'.

This function is useful for setting a human-readable default
value for `work-timer--duration-prompt'.

This function is similar to the ones provided by `org-duration'."
  (let* ((time (round time))
         hours mins secs time-str)
    (setq hours (/ time (* 60 60))
          mins (/ (- time (* 60 60 hours)) 60)
          secs (- time (+ (* 60 60 hours)
                          (* 60 mins)))
          time-str (concat
                    (when (cl-plusp hours) (format "%sh " hours))
                    (when (cl-plusp mins) (format "%sm " mins))
                    (when (cl-plusp secs) (format "%ss" secs))))
    (string-trim time-str)))

(defun work-timer--duration-parse-token (token)
  "Parse TOKEN.
Token is a string. It should look something like \"10m\" or
\"100\". Convert that token into a number of seconds. See the
docstring for `work-timer--duration-prompt' for possible token
suffixes.

This function allows strings that represent negative numbers, like
\"-4\", which would be -240 seconds.

This function is similar to the ones provided by `org-duration'."
  (setq token (string-trim token))      ; Don't want whitespace to matter
  (cond ((string-suffix-p "s" token) ; Seconds
         (string-to-number (string-remove-suffix "m" token)))
        ((string-suffix-p "m" token) ; Minutes
         (* 60 (string-to-number  (string-remove-suffix "m" token))))
        ((string-suffix-p "h" token) ; Hours
         (* 60 60 (string-to-number  (string-remove-suffix "h" token))))
        ((string-match-p "^-?[0-9.]+$" token) ; If only numbers, then treat as minutes
         (* 60 (string-to-number token)))
        (t 0)))

(defun work-timer--duration-prompt (&optional prompt default)
  "Prompt the user for a duration.
Parses the user string and returns the duration in seconds.
Parsing is done by splitting the user input by spaces and parsing
each part into a quantity of seconds. The following suffixes are
recognized:
- \"s\" for seconds
- \"m\" for minutes
- \"h\" for hours
So, for example, \"10m 20s\" is equivalent to 10 minutes and 20
seconds, or 620 seconds.

Any part of the user input that does not end in one of the above
suffixes is ignored. For instance, \"1TEST\" and \"1h HELLO
WORLD\" are both effectively read as \"1h\". The only *exception*
will be when a part of the user input is just a number. In such a
case, this part is treated as the number of minutes: \"20\" is
treated as \"20m\".

PROMPT is a string. If given, then use that string as the prompt
instead. If DEFAULT is provided, that will be the default number
of seconds returned. DEFAULT must be a number (duration in
seconds seconds) or a string that is just that number.

This function is similar to `org-set-effort' that uses functions
provided by `org-duration'."
  (let* ((default-num (if (stringp default)
                          (work-timer--duration-parse-token default)
                        default))
         (default-str (if (numberp default)
                          (work-timer--seconds-to-token default)
                        default))
         (default-str (if (string-empty-p default-str)
                          ;; Show "0s" if a default of 0 seconds was provided
                          "0s"
                        default-str))
         (prompt (concat (or prompt "Duration")
                         (when default (format " (default %s)" default-str))
                         ": "))
         (input (read-from-minibuffer prompt))
         (tokens (string-split input))
         (totals)
         (duration))
    (dolist (token tokens)
      (push (work-timer--duration-parse-token token) totals))
    (setq duration (if (string-empty-p input)
                       default-num
                     (apply #'+ totals)))
    (work-timer--log "(work-timer--duration-prompt) Duration in seconds: %s" duration)
    duration))

(defun work-timer--surplus-prompt (&optional prompt-default no-prompt)
  "Prompt user for a surplus duration in seconds.
A surplus duration denotes how much time should be carried over onto the
next timer.

This function will read user input in natural language (e.g. 15 min;
read the docstring of `work-timer--duration-prompt' for acceptable
units) and return that time in seconds. PROMPT-DEFAULT will be the
default prompted duration.

If `work-timer-break-add-surplus-p' is nil, then this function returns
0.

Assuming `work-timer-break-add-surplus-p' is non-nil and NO-PROMPT or
`work-timer-break-force-surplus-p' is non-nil, then the user will not be
prompted for a value and instead the surplus value will be returned
immediately (i.e. PROMPT-DEFAULT if non-nil, otherwise 0)."
  (let ((surplus (cond
                  ((and work-timer-break-add-surplus-p (or work-timer-break-force-surplus-p no-prompt))
                   prompt-default)
                  (work-timer-break-add-surplus-p
                   (work-timer--duration-prompt "Carry over this much time" (or prompt-default 0)))
                  (t 0))))
    (message "[work-timer] Carried over a surplus %s seconds" surplus)
    surplus))

;;;; Processing timer history
(defun work-timer--process-history (function predicate &optional history)
  "Process all entries in `work-timer-history'.
Returns a list whose elements are the return value of FUNCTION
applied to each entry in `work-timer-history'. Only operate
on elements that satisfy PREDICATE. Both FUNCTION and PREDICATE
take one argument, the current entry in `work-timer-history'.

If HISTORY is provided, operate on that instead of
`work-timer-history'."
  (cl-loop for entry in (or history work-timer-history)
           when (funcall (or predicate 'identity) entry)
           collect (funcall function entry)))

(defun work-timer--elapsed-without-pauses (timer-entry)
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

;;;; Duration functions
;;;;; Basic
(defun work-timer-work-duration-basic ()
  "Return `work-timer-default-work-duration' in seconds."
  (* 60 work-timer-default-work-duration))

(defun work-timer-break-duration-basic ()
  "Return `work-timer-default-break-duration' in seconds."
  (let* ((last-break (car (last (seq-filter
                                 (lambda (entry)
                                   (equal 'break (plist-get entry :type)))
                                 work-timer-history))))
         (last-break-elapsed (when last-break
                               (- (plist-get last-break :end)
                                  (plist-get last-break :start))))
         (surplus (work-timer--surplus-prompt
                   (when last-break
                     (- (plist-get last-break :expected-duration) last-break-elapsed))))
         (duration (+ (or surplus 0)
                      (* 60 work-timer-default-break-duration))))
    (work-timer--log "(work-timer-break-duration-basic) Surplus added: %s" surplus)
    (work-timer--log "(work-timer-break-duration-basic) Break duration: %s" duration)
    duration))

;;;;; Pomodoro
(defun work-timer-work-duration-pomodoro ()
  "Work duration in seconds according to the Pomodoro method."
  (* 60 work-timer-pomodoro-work-duration))

(defun work-timer-break-duration-pomodoro ()
  "Break duration in seconds according to the Pomodoro method."
  (let* ((last-break (car (last (seq-filter
                                 (lambda (entry)
                                   (equal 'break (plist-get entry :type)))
                                 work-timer-history))))
         (last-break-elapsed (when last-break
                               (- (plist-get last-break :end)
                                  (plist-get last-break :start))))
         (surplus (work-timer--surplus-prompt
                   (when last-break
                     (- (plist-get last-break :expected-duration) last-break-elapsed))))

         (long-p (zerop (mod
                         (length
                          (work-timer--process-history 'identity
                                                       (lambda (entry) (eq (plist-get entry :type) 'work))))
                         4)))
         (duration (+ (or surplus 0)
                      (if long-p
                          (* 60 work-timer-pomodoro-break-duration-long)
                        (* 60 work-timer-pomodoro-break-duration-short)))))
    (work-timer--log "(work-timer-break-duration-pomodoro) Surplus added: %s" surplus)
    (work-timer--log "(work-timer-break-duration-pomodoro) Break duration: %s" duration)
    duration))

;;;;; Fractional
(defun work-timer-work-duration-fractional ()
  "\"Fractional\" work duration in seconds.
Returns the value of `work-timer-fractional-work-duration' in
seconds."
  (* 60 work-timer-fractional-work-duration))

(defun work-timer-break-duration-fractional ()
  "\"Fractional\" break duration in seconds.
Return, in seconds, a fraction of the time worked in the previous
work timer. This fraction is determined by the value of
`work-timer-fractional-break-duration-fraction'."
  (let* ((work-elapsed (- (float-time (current-time))
                          work-timer-start-time))
         (last-break (car (last (seq-filter
                                 (lambda (entry)
                                   (equal 'break (plist-get entry :type)))
                                 work-timer-history))))
         (last-break-elapsed (when last-break
                               (- (plist-get last-break :end)
                                  (plist-get last-break :start))))
         (surplus (work-timer--surplus-prompt
                   (when last-break
                     (- (plist-get last-break :expected-duration) last-break-elapsed))))
         (duration (+ (or surplus 0)
                      (* work-elapsed work-timer-fractional-break-duration-fraction))))
    (work-timer--log "(work-timer-break-duration-fractional) Surplus added: %s" surplus)
    (work-timer--log "(work-timer-break-duration-fractional) Break duration: %s" duration)
    duration))

;;; Commands
;;;; Timers
;;;###autoload
(defun work-timer-start (&optional start duration type)
  "Start a work timer.
Optionally provide START which is a custom start time. See the
docstring of `work-timer--set-timer' for the acceptable format of
this argument. If none is provided, the current time is used.

Optionally provide DURATION which is the duration of the timer in
seconds.

With `prefix-arg', prompt for the duration of the timer in
seconds. If nothing is provided, then the function defined in
`work-timer-work-duration-function'.

TYPE overrides the default timer type of `work'."
  (interactive)
  (setq duration (or duration (funcall work-timer-work-duration-function)))
  (work-timer--set-timer (or type 'work)
                         (if current-prefix-arg
                             (work-timer--duration-prompt "Set timer's duration" duration)
                           duration)
                         start)
  (unless global-mode-string (setq global-mode-string '("")))
  (unless (memq 'work-timer-mode-line-string global-mode-string)
    (setq global-mode-string
          (append global-mode-string '(work-timer-mode-line-string))))
  (work-timer--log "(work-timer-start) Timer started"))

;;;###autoload
(defun work-timer-pause-or-continue (&optional pause-or-continue)
  "Pause or continue the current timer.

If PAUSE-OR-CONTINUE is `'pause' or `'continue', then force doing
that action."
  (interactive)
  (when (not (timerp work-timer-current-timer))
    (user-error "[work-timer] No timer running!"))
  (setq pause-or-continue (or pause-or-continue
                              (if work-timer-pause-time
                                  'continue
                                'pause)))
  (let (pauses-modified-p)
    (pcase pause-or-continue
      ('continue
       (when work-timer-pause-time    ; Do nothing if not currently paused
         (work-timer--log "(work-timer-pause-or-continue) Timer continued")
         ;; Move back `work-timer-end-time' for how long timer was paused
         (setq work-timer-end-time (float-time
                                    (time-add work-timer-end-time
                                              (time-since work-timer-pause-time))))

         ;; Set :pause-end of pause in `work-timer-pauses'
         (setf (plist-get (car (last work-timer-pauses)) :pause-end)
               (float-time (current-time)))
         (setq work-timer-pause-time nil
               pauses-modified-p t)))
      ('pause
       (unless work-timer-pause-time  ; Do nothing if already paused
         (work-timer--log "(work-timer-pause-or-continue) Timer paused")
         (setq work-timer-pause-time (float-time (current-time))
               work-timer-pauses
               (append work-timer-pauses
                       (list (list :pause-start work-timer-pause-time :pause-end nil)))
               pauses-modified-p t))))
    ;; If actually paused or continued, it is useful to return
    ;; `work-timer-pauses'
    (when pauses-modified-p work-timer-pauses)))

;;;###autoload
(defun work-timer-cycle-finish (&optional manual)
  "Finish the current timer cycle.
If MANUAL is provided, via argument or `prefix-arg', then
manually prompt for a duration in seconds. Prompt also accepts
sexps to calculate the value."
  (interactive "^P")
  (unless (timerp work-timer-current-timer)
    (user-error "[work-timer] No timer running!"))
  (force-mode-line-update)
  (let ((new-history (append work-timer-history
                             (list (list :type work-timer-type
                                         :expected-duration work-timer-duration
                                         :start work-timer-start-time
                                         :end (float-time (current-time))
                                         :pauses work-timer-pauses))))
        (duration (condition-case err
                      (pcase work-timer-type
                        ('break (funcall work-timer-work-duration-function))
                        ('work (funcall work-timer-break-duration-function)))
                    (error
                     "[work-timer] (work-timer-cycle-finish): %s" (error-message-string err)))))
    (work-timer--set-timer (pcase work-timer-type
                             ('break 'work)
                             ('work 'break))
                           (if manual
                               (work-timer--duration-prompt "Set new timer's duration" duration)
                             duration))
    (setq work-timer-history new-history)
    (work-timer--log "(work-timer-cycle-finish) Cycle finished")
    (run-hooks 'work-timer-cycle-finish-hook)))

;;;###autoload
(defun work-timer-end (&optional clear-history)
  "End the current timer.
If CLEAR-HISTORY is non-nil (e.g. command called with `prefix-arg'), also
clear `work-timer-history'."
  (interactive "^P")
  (when (timerp work-timer-current-timer)
    (cancel-timer work-timer-current-timer))
  (setq work-timer-current-timer nil
        work-timer-type nil
        work-timer-start-time nil
        work-timer-duration nil
        work-timer-end-time nil
        work-timer-pause-time nil
        work-timer-pauses nil
        global-mode-string (remove 'work-timer-mode-line-string global-mode-string))
  (when clear-history
    (setq work-timer-history nil)
    (message "[work-timer] Erased `work-timer-history'"))
  (force-mode-line-update t)
  (work-timer--log "(work-timer-end) Timer ended"))

;;;###autoload
(defun work-timer-dwim (&optional manual)
  "Conditionally start a timer or finish a cycle.
If MANUAL is non-nil then prompt for the duration of that timer."
  (interactive "P")
  ;; If the user wants to end a cycle amidst a pause, then end pause first. We
  ;; do this here rather than in `work-timer-cycle-finish' because this prevents
  ;; `work-timer-start' from ever being called in the middle of a pause. This
  ;; can occur when there is no timer but we are in "the middle of a pause,"
  ;; such as when the timer is accidentally cancelled or when the user is using
  ;; variables restored from a previous Emacs session via `savehist' and wants
  ;; to continue where they left off
  (when work-timer-pause-time
    (work-timer-pause-or-continue 'continue))
  (if (timerp work-timer-current-timer)
      (work-timer-cycle-finish manual)
    (work-timer-start work-timer-start-time work-timer-duration work-timer-type))
  (run-hooks 'work-timer-dwim-hook))

;;;; Convenience
(defun work-timer-modify ()
  "Modify the current timer's parameters live."
  (interactive)
  (let* ((ch (save-window-excursion
               (save-excursion
                 (with-output-to-temp-buffer "*Work Timer*"
                   (princ (format-message "Select a variable to modify:

d      Expected duration

r      Running time.")))
                 (fit-window-to-buffer (get-buffer-window "*Work Timer*"))
                 (let (char-pressed)
                   (while (or (null char-pressed)
                              (and (not (memq char-pressed
                                              '(?q ?d ?r)))
                                   (or (ding) t)))
                     (setq char-pressed
                           (read-char-exclusive "[dr]? "
                                                nil 45)))
                   (and (not (memq char-pressed '(?q))) char-pressed))))))
    (cond
     ((memq ch '(?d ?D))
      (let* ((duration (work-timer--duration-prompt "Change expected duration to" work-timer-duration))
             (new-end (- work-timer-end-time work-timer-duration duration)))
        (setq work-timer-duration duration
              work-timer-end-time new-end
              ;; Ensure `work-timer-overrun-p', which tracks whether the sound
              ;; already rang, is not non-nil if the new duration is after the
              ;; elapsed time
              work-timer-overrun-p (<= new-end
                                       (work-timer--elapsed-without-pauses
                                        (list :start work-timer-start-time
                                              :end (float-time (current-time))
                                              :pauses work-timer-pauses))))))
     ((memq ch '(?r ?R))
      (let* ((offset (work-timer--duration-prompt "Offset the current running time by"))
             (new-start (- work-timer-start-time offset)))
        (setq work-timer-start-time new-start
              ;; Ensure `work-timer-overrun-p', which tracks whether the sound
              ;; already rang, is not non-nil if the modified time is before the
              ;; end time
              work-timer-overrun-p (<= work-timer-end-time new-start)))))))

(defun work-timer-report ()
  "Print the statistics of this series of timers."
  (interactive)
  (unless (timerp work-timer-current-timer)
    (user-error "[work-timer] No timer running!"))
  (let* ((work-timer-history
          (append work-timer-history
                  (list (list :type work-timer-type
                              :start work-timer-start-time
                              :end (float-time (current-time))
                              :pauses work-timer-pauses))))
         (elapsed-total
          (- (plist-get (car (last work-timer-history)) :end)
             (plist-get (cl-first work-timer-history) :start)))
         (work-count (length
                      (work-timer--process-history 'identity
                                                   (lambda (entry) (eq (plist-get entry :type) 'work)))))
         (work-sum (apply #'+
                          (work-timer--process-history 'work-timer--elapsed-without-pauses
                                                       (lambda (entry) (eq (plist-get entry :type) 'work)))))
         (break-count (length
                       (work-timer--process-history 'identity
                                                    (lambda (entry) (eq (plist-get entry :type) 'break)))))
         (break-sum (apply #'+
                           (work-timer--process-history 'work-timer--elapsed-without-pauses
                                                        (lambda (entry) (eq (plist-get entry :type) 'break))))))
    (message "In the last %s, you had %s work sessions and %s breaks, and worked for %s and took breaks for %s. Your work efficiency is %s (not including pauses)."
             (format-seconds "%.2h hours and %.2m minutes" elapsed-total)
             work-count
             break-count
             (format-seconds "%.2h:%.2m:%.2s" work-sum)
             (format-seconds "%.2h:%.2m:%.2s" break-sum)
             (format "%f" (/ (- work-sum break-sum) work-sum)))))

;;;; Keymap
;;;###autoload (autoload 'work-timer-prefix-map "work-timer" nil t 'keymap)
(defvar-keymap work-timer-prefix-map
  :doc "Prefix map for `work-timer' commands."
  "s" #'work-timer-start
  "p" #'work-timer-pause-or-continue
  "f" #'work-timer-cycle-finish
  "e" #'work-timer-end
  "w" #'work-timer-dwim
  "r" #'work-timer-report
  "m" #'work-timer-modify)

;;;###autoload (autoload 'work-timer-transient-menu "work-timer" nil t)
(transient-define-prefix work-timer-transient-menu ()
  "Transient menu for work-timer commands."
  ["Managing timers"
   ("s" "Start working timer" work-timer-start)
   ("p" "Pause or continue a timer" work-timer-pause-or-continue)
   ("f" "Cycle timer (end current timer and begin the next)" work-timer-cycle-finish)
   ("e" "End the current timer" work-timer-end)
   ("w" "DWIM (start or cycle)" work-timer-dwim)]
  ["Other"
   ("r" "Print statistics of timer history" work-timer-report)
   ("m" "Modify the parameters of the current timer" work-timer-modify)])

;;; Org-clock integration
(declare-function org-get-at-bol "org-macs" (property))
(declare-function org-agenda-clock-in "org-agenda" (&optional arg))
(declare-function org-agenda-clock-out "org-agenda" (&optional arg))
(defvar org-clock-marker)

(defun work-timer-org-agenda-dwim ()
  "Behaviors in `org-agenda' buffers when finishing a cycle.
When finishing a cycle, clock in if the upcoming timer is a work
one, and out if it's a break one."
  (when (and (equal major-mode 'org-agenda-mode)
             (org-get-at-bol 'org-marker)) ; On an org-agenda item
    (pcase work-timer-type
      ('work
       (org-agenda-clock-in))
      ('break
       (when (marker-buffer org-clock-marker)
         (org-agenda-clock-out))))))

(defun work-timer-org-clock-in ()
  "Function added to `org-clock-in-hook'.
Either start a timer or continue an existing one if the current
timer is a work one."
  (cond
   ((not (timerp work-timer-current-timer)))
   ((eq work-timer-type 'work)
    (work-timer-pause-or-continue 'continue)
    (work-timer--log "(work-timer-org-clock-in) Break continued"))
   ((eq work-timer-type 'break)
    (work-timer-pause-or-continue 'pause)
    (work-timer--log "(work-timer-org-clock-in) Break paused"))))

(defun work-timer-org-clock-out ()
  "Function added to `org-clock-out-hook'.
Continue a timer if current timer is a break one."
  (cond
   ((eq work-timer-type 'break)
    (work-timer-pause-or-continue 'continue)
    (work-timer--log "(work-timer-org-clock-out) Break continued"))))

;;;###autoload
(define-minor-mode work-timer-with-org-clock-mode
  "Global minor mode that integrates with work-timer with `org-agenda'."
  :global t
  :group 'work-timer
  (cond
   (work-timer-with-org-clock-mode
    (add-hook 'org-clock-in-hook #'work-timer-org-clock-in)
    (add-hook 'org-clock-out-hook #'work-timer-org-clock-out)
    (add-hook 'work-timer-dwim-hook #'work-timer-org-agenda-dwim))
   (t
    (remove-hook 'org-clock-in-hook #'work-timer-org-clock-in)
    (remove-hook 'org-clock-out-hook #'work-timer-org-clock-out)
    (remove-hook 'work-timer-dwim-hook #'work-timer-org-agenda-dwim))))

(provide 'work-timer)
;;; work-timer.el ends here
