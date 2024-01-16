(ql:quickload 'ltk)

(defpackage :timer-app
  (:use :common-lisp :ltk)
  (:export #:main))

(in-package :timer-app)

(defparameter *ticks-per-subsecond* (/ internal-time-units-per-second 100))
(defparameter *ticks-per-second* internal-time-units-per-second)
(defparameter *ticks-per-minute* (* *ticks-per-second* 60))
(defparameter *ticks-per-hour* (* *ticks-per-minute* 60))
(defparameter *total-ticks* 0)
(defparameter *start-time* 0)
(defparameter *timer-active-p* nil)

(defun compute-time (ticks)
  (let* ((hours (multiple-value-list (floor ticks *ticks-per-hour*)))
         (minutes (multiple-value-list (floor (cadr hours) *ticks-per-minute*)))
         (seconds (multiple-value-list (floor (cadr minutes) *ticks-per-second*)))
         (subseconds (floor (cadr seconds) *ticks-per-subsecond*)))
    (list (car hours) (car minutes) (car seconds) subseconds)))

(defun update-ticks ()
  (if *timer-active-p*
      (+ *total-ticks* (- (get-internal-real-time) *start-time*))
      *total-ticks*))

(defun start-timer ()
  (when (eq *timer-active-p* nil)
    (setq *start-time* (get-internal-real-time))
    (setq *timer-active-p* 1)))

(defun stop-timer ()
  (when *timer-active-p*
    (setq *total-ticks* (+ *total-ticks* (- (get-internal-real-time) *start-time*)))
    (setq *timer-active-p* nil)))

(defun reset-timer ()
  (setq *total-ticks* 0)
  (setq *start-time* (get-internal-real-time))
  (compute-time 0))

(defun update-timer-value (t1)
  (let ((update (compute-time (update-ticks))))
    (setf (text t1) (format nil "~02,'0d:~02,'0d:~02,'0d.~02,'0d"
                            (nth 0 update)
                            (nth 1 update)
                            (nth 2 update)
                            (nth 3 update))))
  (after 10 #'(lambda () (update-timer-value t1))))

(defun main()
  (with-ltk ()
    (let* ((f (make-instance 'frame))
           (t1 (make-instance 'label :master f :font "Helvetica 64 bold" :foreground 'black :background 'black))
           (mymenu (make-menubar))
           (start-button (make-instance 'button :text "Start" :command #'start-timer))
           (stop-button (make-instance 'button :text "Stop" :command #'stop-timer))
           (reset-button (make-instance 'button :text "Reset" :command #'reset-timer))
           (quit-button (make-instance 'button :text "Quit" :command #'exit-wish)))
      (pack f)
      (pack t1 :padx 20 :pady 20)
      (pack start-button)
      (pack stop-button)
      (pack reset-button)
      (pack quit-button)
      (wm-title *tk* "Timer")
      (update-timer-value t1))))
