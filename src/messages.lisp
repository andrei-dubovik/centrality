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
