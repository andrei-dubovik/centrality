;; Copyright (c) 2020 Andrey Dubovik <andrei@dubovik.eu>

;; Simple logging facilities

(in-package :centrality)

;; *log* is set when the log process is started and it is used throughout the program
(defvar *log*)

;; Settings for how detailed the log should be
(defparameter *level-file* 3)
(defparameter *level-print* 2)

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

(defun log-msg (level &rest plist &key &allow-other-keys)
  "Send message to the log thread"
  (if *log*
      (send-msg *log* (list* (get-formatted-time) :level level plist))))

;; Event loop

(defun log-loop (file)
  "Receive and log messages"
  (macrolet ((print-msg ()
               `(let ((level (third msg)))
                  (if (<= level *level-print*) (print-line msg))
                  (if (<= level *level-file*) (print-line msg out)))))
    (ensure-directories-exist file)
    (let ((out (open file :direction :output :if-exists :append :if-does-not-exist :create)))
      (unwind-protect
           (while (msg = (recv-msg *log*))
             (print-msg))
        (let ((msg (list (get-formatted-time) :level 1 :event :log-close)))
          (print-msg))
        (close out))))) ; (setq *log* nil) belongs elsewhere

(defun open-log (file)
  "Start logging thread"
  (setq *log* (make-mailbox))
  (let ((th (make-thread (lambda () (log-loop file)) :name "centrality-log")))
    (log-msg 1 :event :log-open :level-file *level-file* :level-print *level-print*)
    th))
