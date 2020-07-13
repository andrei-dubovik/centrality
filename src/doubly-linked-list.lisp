;; Copyright (c) 2020 Andrey Dubovik <andrei@dubovik.eu>

;; Doubly-linked list

(in-package :centrality)

;; "doubly-linked-list" is a lot of typing, and I don't like any of
;; the acronyms, so I'll just call it a chain.

(defmacro make-node (prev value next)
  `(cons (cons ,prev ,value) ,next))

(defmacro next-node (node)
  `(cdr ,node))

(defmacro prev-node (node)
  `(caar ,node))

(defmacro node-value (node)
  `(cdar ,node))

(defmacro tail-node-p (node)
  `(eql (node-value ,node) 'tail))

(defmacro chain-head (chain)
  `(car ,chain))

(defmacro chain-tail (chain)
  `(cdr ,chain))

(defmacro chain-first (chain)
  `(next-node (chain-head ,chain)))

(defmacro chain-last (chain)
  `(prev-node (chain-tail ,chain)))

(defun chain-front (chain)
  "Return the first non-dummy element"
  (let ((value (node-value (chain-first chain))))
    (if (eql value 'tail) nil value)))

(defun chain-back (chain)
  "Return the last non-dummy element"
  (let ((value (node-value (chain-last chain))))
    (if (eql value 'head) nil value)))

(defun make-chain ()
  "Create an empty doubly-linked list"
  (let ((head (make-node nil 'head nil))
        (tail (make-node nil 'tail nil)))
    (setf (next-node head) tail)
    (setf (prev-node tail) head)
    (cons head tail)))

(defun insert-after (node obj)
  "Insert obj after node in a doubly linked list"
  (let* ((next (next-node node))
         (new (make-node node obj next)))
    (setf (next-node node) new)
    (setf (prev-node next) new)
    new))

(defun insert-before (node obj)
  "Insert obj before node in a doubly linked list"
  (let* ((prev (prev-node node))
         (new (make-node prev obj node)))
    (setf (next-node prev) new)
    (setf (prev-node node) new)
    new))

(defun chain-remove (node)
  "Remove a given node from a doubly linked list"
  (let ((value (node-value node)))
    (if (or (eql value 'head) (eql value 'tail))
        nil
        (let ((prev (prev-node node))
              (next (next-node node)))
          (setf (next-node prev) next)
          (setf (prev-node next) prev)
          value))))

(defmacro chain-pushfront (obj chain)
  `(insert-after (chain-head ,chain) ,obj))

(defmacro chain-pushback (obj chain)
  `(insert-before (chain-tail ,chain) ,obj))

(defmacro chain-popfront (chain)
  `(chain-remove (chain-first ,chain)))

(defmacro chain-popback (chain)
  `(chain-remove (chain-last ,chain)))

(defmacro dochain ((var list) &body body)
  "Iterate over a doubly linked list"
  (with-gensym (node)
    `(do ((,node (chain-first ,list) (next-node ,node)))
         ((tail-node-p ,node))
       (let ((,var (node-value ,node)))
         ,@body))))

(defun chain-count (chain)
  "Return the number of elements in a double linked list"
  (- (length (chain-head chain)) 2))

(defun chain-emptyp (chain)
  "Check if the chain is empty"
  (tail-node-p (chain-first chain)))
