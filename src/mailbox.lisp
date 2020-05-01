;; Copyright (c) 2020 Andrey Dubovik <andrei@dubovik.eu>

;; A thin wrapper around SBCL mailbox
;; (I could not find any widely used library)
;; Also a thin wrapper around SBCL queue

;; TODO: either program explicitly for SBCL or or think about fallbacks

(in-package :centrality)

#+sbcl
(defmacro make-mailbox ()
  `(sb-concurrency:make-mailbox))

#+sbcl
(defmacro recv-msg (mailbox)
  `(sb-concurrency:receive-message ,mailbox))

#+sbcl
(defmacro send-msg (mailbox message)
  `(sb-concurrency:send-message ,mailbox ,message))

#+sbcl
(defmacro make-queue ()
  `(sb-concurrency:make-queue))

#+sbcl
(defmacro dequeue (queue)
  `(sb-concurrency:dequeue ,queue))

#+sbcl
(defmacro enqueue (value queue)
  `(sb-concurrency:enqueue ,value ,queue))
