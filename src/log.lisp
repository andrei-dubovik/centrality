;; Copyright (c) 2020 Andrey Dubovik <andrei@dubovik.eu>

;; Simple logging facilities

(in-package :centrality)

;; *log* is set when the log process is started and it is used throughout the program
(defvar *log*)

(defclass logger ()
  ((fd :initarg :fd :reader .fd)))

(defun get-formatted-time ()
  "Get current time in ISO format"
  (multiple-value-bind (second minute hour date month year day daylight-p zone) (get-decoded-time)
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d" year month date hour minute second)))

(defun print-line (object &optional (stream *standard-output*))
  "Print object in full, add newline, flush"
  (let (#+sbcl(sb-ext:*print-vector-length* nil))
    (write object :stream stream :pretty nil :length nil :level nil))
  (write-char #\Newline stream)
  (finish-output stream))

(defun log-msg (level &rest msg)
  "Send message to the log thread"
  (if *log*
      (send* *log* :log (get-formatted-time) :level level msg)))

(defcall :log ((state logger) &rest msg &args time &key level &allow-other-keys)
  "Log message to file"
  (declare (ignore time))
  (if (<= level *max-log-level*) (print-line msg (.fd state))))

;; Event loop

(defworker log-loop (file)
  "Receive and log messages"
  (ensure-directories-exist file)
  (let* ((out (open file :direction :output :if-exists :append :if-does-not-exist :create))
         (state (make-instance 'logger :fd out)))
    (unwind-protect
         (progn
           (call state :log (get-formatted-time) :level 1 :event :log-open :max-log-level *max-log-level*)
           (while (msg = (receive))
             (apply #'call state msg)))
      (call state :log (get-formatted-time) :level 1 :event :log-close)
      (close out)))) ; (setq *log* nil) belongs elsewhere

(defun start-log (file)
  "Start logging thread"
  (setq *log* (spawn nil #'log-loop file)))
