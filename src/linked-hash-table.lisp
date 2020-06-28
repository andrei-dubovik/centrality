;; Copyright (c) 2020 Andrey Dubovik <andrei@dubovik.eu>

;; Linked hash tables (ordered dictionaries)

;; Structure: a linked hash table is a cons of a doubly-linked list
;; and a hash table. The elements of the list are (key . value). The
;; elements of the hash table are references to the respective list
;; nodes.

(in-package :centrality)

(defmacro dict-hash (dict)
  `(car ,dict))

(defmacro dict-chain (dict)
  `(cdr ,dict))

(defun make-dict (&rest rest)
  "Make an empty ordered dictionary"
  (cons (apply #'make-hash-table rest) (make-chain)))

(defun getkey (key dict &optional default)
  "Retrieve an element from an ordered dictionary"
  (multiple-value-bind (node present-p) (gethash key (dict-hash dict))
    (if present-p
        (values (cdr (node-value node)) t)
        (values default nil))))

(defun (setf getkey) (value key dict)
  "Set an element in an ordered dictionary"
  (let ((hash (dict-hash dict)))
    (multiple-value-bind (node present-p) (gethash key hash)
      (if present-p
          (setf (cdr (node-value node)) value)
          (setf (gethash key hash) (chain-pushback (cons key value) (dict-chain dict))))))
  value)

(defun remkey (key dict)
  "Remove an element from an ordered dictionary"
  (let ((hash (dict-hash dict)))
    (multiple-value-bind (node present-p) (gethash key hash)
      (when present-p
        (remhash key hash)
        (chain-remove node)
        t))))

(defun dict-count (dict)
  "Return the size of an ordered dictionary"
  (hash-table-count (dict-hash dict)))

(defun dict-front (dict)
  "Get (key value) from the front of an ordered dictionary"
  (if (> (dict-count dict) 0)
      (destructuring-bind (key . value) (chain-front (dict-chain dict))
        (values key value))))

(defun dict-back (dict)
  "Get (key value) from the back of an ordered dictionary"
  (if (> (dict-count dict) 0)
      (destructuring-bind (key . value) (chain-back (dict-chain dict))
        (values key value))))

(defun dict-popfront (dict)
  "Pop (key value) from the front of an ordered dictionary"
  (if (> (dict-count dict) 0)
      (destructuring-bind (key . value) (chain-popfront (dict-chain dict))
        (remhash key (dict-hash dict))
        (values key value))))

(defun dict-popback (dict)
  "Pop (key value) from the back of an ordered dictionary"
  (if (> (dict-count dict) 0)
      (destructuring-bind (key . value) (chain-popback (dict-chain dict))
        (remhash key (dict-hash dict))
        (values key value))))

(defmacro dodict (((key value) dict) &body body)
  "Iterate over an ordered dictionary"
  (with-gensym (var)
    `(dochain (,var (dict-chain ,dict))
       (destructuring-bind (,key . ,value) ,var
         ,@body))))
