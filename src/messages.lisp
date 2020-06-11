;; Copyright (c) 2020 Andrey Dubovik <andrei@dubovik.eu>

;; Functions for inter-thread communication and for handling
;; BitTorrent messages

(in-package :centrality)

(defgeneric call (object method &rest rest)
  (:documentation "A generic function for processing messages"))

(defmacro match-prefix (((prefix test) &rest rest) form &body body)
  "Destructuring-bind with a guard on the first element"
  `(destructuring-bind (,prefix ,@rest)
       (if (,test (car ,form)) (cons (list (car ,form)) (cdr ,form)) (cons nil ,form))
     ,@body))

;; TODO: add support for (defcall :method ((state1 class1) state2 state3 &args ...)
;; TODO: make object explicit (so that tooltips are informative)
(defmacro defcall (method &rest rest)
  "A convenience wrapper for defining methods that process messages"
  (with-gensym (rest-var)
    (match-prefix ((qualifier keywordp) (object &rest args) &body body) rest
      (match-prefix ((documentation stringp) &body body) body
        (let ((rest (or (cadr (member '&rest args)) rest-var))
              (args (cdr (member '&args args))))
          `(defmethod call ,@qualifier
             (,object
              ,(if (keywordp method) `(,(gensym) (eql ,method)) method)
              &rest ,rest)
             ,@documentation
             ,@(if args
                   `((destructuring-bind ,args ,rest ,@body))
                   `((declare (ignore ,rest-var)) ,@body))))))))

(defcall method (object &args)
  "Silently ignore unknown messages")

(defvar *thread-counter* (cons (make-lock) 0))

(defun incf-thread-counter ()
  "Thread safe increment of *thread-counter*"
  (with-lock-held ((car *thread-counter*))
    (incf (cdr *thread-counter*))))

(defun function-name (func)
  "Return function name as a lowercased string"
  (let ((name (nth-value 2 (function-lambda-expression func))))
    (if (symbolp name) (string-downcase (symbol-name name)) "lambda")))

(defun gen-thread-name (func)
  "Generate a new unique thread name"
  (string-join
   (list *basename* (function-name func) (write-to-string (incf-thread-counter))) "-"))

(defun make-thread-pool ()
  "Create a new thread pool"
  (cons (make-lock) (dll-new)))

(defmacro dopool ((var list) &body body)
  "Iterate over a thread pool"
  `(dodll (,var (cdr ,list))
     ,@body))

(defun spawn (pool worker &rest args)
  "Start a new thread, add it to the pool, return its communication channel"
  (let ((channel (make-mailbox)))
    (if pool
        ;; Pool available
        (let ((channel* (with-lock-held ((car pool))
                          (dll-append (cdr pool) channel))))
          (flet ((finilize ()
                   (with-lock-held ((car pool))
                     (dll-remove channel*))))
            (make-thread
             (lambda ()
               (apply worker channel #'finilize args))
             :name (gen-thread-name worker))))

        ;; Pool unavailable
        (make-thread
         (lambda () (apply worker channel (lambda ()) args))
         :name (gen-thread-name worker)))
    channel))

;; "send" and "recieve" are there for naming consistency
;; TODO: remove send-msg, recv-msg

(defmacro send (mailbox &rest msg)
  "Send a message to a mailbox associated with a thread"
  `(send-msg ,mailbox (list ,@msg)))

(defmacro receive (mailbox)
  "Receice a message"
  `(recv-msg ,mailbox))

(defmacro defworker (name args &body body)
  "Define a worker that can recieve messages and that can be started with spawn"
  (match-prefix ((documentation stringp) &body body) body
    (with-gensym (channel finilize)
      `(defun ,name ,(list* channel finilize args)
         ,@documentation
         (macrolet ((receive () (macroexpand '(receive ,channel)))
                    (channel () ',channel))
           (unwind-protect
                (progn ,@body)
             (funcall ,finilize)))))))
