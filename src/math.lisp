;; Copyright (c) 2020 Andrey Dubovik <andrei@dubovik.eu>

;; Math functions

(in-package :ansible)

(defun expt-mod (base power modulus)
  "Raise base to power modulo modulus"
  (iter (with result = 1)
        (for i from 0 below (integer-length power))
        (for sqr first base then (mod (* sqr sqr) modulus))
        (if (logbitp i power)
            (setq result (mod (* result sqr) modulus)))
        (finally (return result))))

(defun random-permutation (size)
  "Generate a random permutation"
  (with-vector (perm size)
    (dotimes (i size)
      (setf (svref perm i) i))
    (dotimes (i (1- size))
      (rotatef
       (svref perm i)
       (svref perm (+ i (random (- size i))))))))
