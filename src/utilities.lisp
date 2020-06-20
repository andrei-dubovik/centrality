;; Copyright (c) 2020 Andrey Dubovik <andrei@dubovik.eu>

;; Common miscellaneous functions

(in-package :centrality)

(defmacro with-gensym ((&rest names) &body body)
  "Use unique names in macros to avoid name conflicts"
  `(let ,(iter (for name in names) (collect `(,name (gensym))))
     ,@body))

(defun getvalue (key alist)
  "Get a value from an association list using :equal"
  (cdr (assoc key alist :test #'equal)))

(defun string-join (list &optional (delim ""))
  "Concatenate strings using a delimiter"
  (apply #'concatenate 'string
   (iter (for s on list)
         (collect (car s))
         (if (cdr s) (collect delim)))))

(defun composite-symbol (&rest parts)
  "Create and intern a composite symbol, e.g. composite-symbol"
  (intern
   (string-join
    (mapcar
     (lambda (part)
       (if (symbolp part) (symbol-name part) (string-upcase part)))
     parts) "-")))

(defmacro with-structure (type init &body body)
  "Initialize a structure sequentially with an ability to reference earlier slots"
  (multiple-value-bind (init args)
      (iter (for (slot value) in init)
            (if (keywordp slot)
                (let ((slot-symbol (intern (symbol-name slot))))
                  (collect (list slot-symbol value) into init2)
                  (collect slot into args)
                  (collect slot-symbol into args))
                (collect (list slot value) into init2))
            (finally (return (values init2 args))))
    `(let* (,@init)
       ,@body
       (,(composite-symbol "make" type) ,@args))))

(defmacro setstruct ((struct name) &body body)
  "Set multiple structure slots using a shorter syntax"
  `(progn
     ,@(mapcar
        (lambda (init)
          (destructuring-bind (slot value) init
            `(setf (,(composite-symbol name slot) ,struct) ,value)))
        body)))

(defmacro with-vector ((name size &optional (element-type t)) &rest body)
  "Create an empty vector, execute body, return the vector"
  `(let ((,name (make-array ,size :element-type ,element-type)))
     ,@body
     ,name))

(define-condition split-sequence-error (error) ())

(defun split-sequence (sequence length)
  "Split a sequence into subsequences of given length"
  (multiple-value-bind (size rem) (ceiling (length sequence) length)
    (if (not (zerop rem)) (error 'split-sequence-error))
    (with-vector (vector size)
      (dotimes (i size)
        (setf (aref vector i) (subseq sequence (* length i) (* length (1+ i))))))))

(defmacro if-let ((name test) then &optional else)
  "Rudimentary if-let"
  `(let ((,name ,test))
     (if ,name ,then ,else)))

(defun first-or-self (object)
  "Get first element if object is list, otherwise return object itself"
  (if (listp object) (car object) object))

(defun hash-list (hash-table)
  "Coerce hash table to list"
  (let (list)
    (dohash ((key value) hash-table)
      (push (cons key value) list))
    list))

;; Basic doubly linked lists
;; Structure: (cons (cons prev value) next)

(defun dll-new ()
  "Create a doubly linked list with one null element"
  (cons (cons nil nil) nil))

(defun dll-append (head obj)
  "Insert obj after head in a doubly linked list"
  (let* ((next (cdr head))
         (node (cons (cons head obj) next)))
    (if head (setf (cdr head) node))
    (if next (setf (caar next) node))
    node))

(defun dll-remove (head)
  "Remove head from a doubly linked list"
  (let ((prev (caar head))
        (next (cdr head)))
    (if prev (setf (cdr prev) next))
    (if next (setf (caar next) prev))
    next))

(defmacro dodll ((var list) &body body)
  "Iterate over a doubly linked list, skipping the header"
  (with-gensym (var*)
    `(dolist (,var* (cdr ,list))
       (let ((,var (cdr ,var*)))
         ,@body))))

;; Playing around with iteration macros :)

(defmacro while (cond &body body)
  "A rudimentary while macro"
  (with-gensym (begin)
    (if (eql (second cond) '=)
        (let ((var (first cond))
              (val (third cond)))
          `(prog (,var)
              ,begin
              (if (not (setq ,var ,val)) (return))
              ,@body
              (go ,begin)))
        `(prog ()
            ,begin
            (if (not ,cond) (return))
            ,@body
            (go ,begin)))))

(defmacro dohash (((key value) hash-table) &body body)
  "Iterate over a hash table"
  (with-gensym (iterator begin status)
    `(with-hash-table-iterator (,iterator ,hash-table)
       (prog ()
          ,begin
          (multiple-value-bind (,status ,key ,value) (,iterator)
            (when ,status
              ,@body
              (go ,begin)))))))

;; Bit-vector operations (TODO: switch to big integers?)

(defun bit-zerop (array)
  "Test if a bit vector is all zeroes"
  (not (find 1 array)))

(defun bit-onep (array)
  "Test if a bit vector is all ones"
  (not (find 0 array)))

(defun bit-clear (array)
  "Reset bit vector to 0s"
  (bit-xor array array array))

(defun bit-set (array)
  "Reset bit vector to 1s"
  (bit-eqv array array array))

(defmacro with-bit-vector ((name size) &rest body)
  "Create an empty bit vector, execute body, return the vector"
  `(with-vector (,name ,size 'bit) ,@body))
