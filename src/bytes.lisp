;; Copyright (c) 2020 Andrey Dubovik <andrei@dubovik.eu>

;; Miscellaneous byte operations

(in-package :centrality)

(defmacro with-bytearray ((name size) &rest body)
  "Create an empty bytes array, execute body, return the array"
  `(with-vector (,name ,size '(unsigned-byte 8)) ,@body))

;; Define global stream for dynamic overloading
;; (otherwise "stream" is the most common word in the whole program)

(defvar *stream*)

;; byte-read and byte-write are substitutes for read-byte and write-byte
;; where stream is optional

(defun byte-read (&optional (stream *stream*))
  (read-byte stream))

(defun byte-write (byte &optional (stream *stream*))
  (write-byte byte stream))

(defun read-bytes (len &optional (stream *stream*))
  "Read a given number of bytes from stream"
  (with-bytearray (bytes len)
    (read-sequence bytes stream :end len)))

;; adding write-bytes for consistency of notation

(defun write-bytes (bytes &optional (stream *stream*))
  (write-sequence bytes stream))

(defun write-cb (char &optional (stream *stream*))
  "Write character to a binary stream"
  (write-byte (char-code char) stream))

(defun write-sb (string &optional (stream *stream*))
  "Write string to a binary stream"
  (write-bytes (string-to-octets string) stream))

(defun unpack (bytes)
  "Unpack bytes into an integer (big-endian)"
  (let ((integer 0)
        (len (length bytes)))
    (dotimes (i len)
      (setq integer
       (boole boole-ior
        integer (ash (aref bytes (1- (- len i))) (* 8 i)))))
    integer))

(defun pack (integer len)
  "Pack integer into bytes (big-endian)"
  (with-bytearray (bytes len)
    (dotimes (i len)
      (setf (aref bytes (1- (- len i)))
            (boole boole-and 255 (ash integer (- (* 8 i))))))))

(defun read-int (len &optional (stream *stream*))
  "Read an integer from stream (big-endian)"
  (unpack (read-bytes len stream)))

(defun write-int (integer len &optional (stream *stream*))
  "Write an integer to stream (big-endian)"
  (write-bytes (pack integer len) stream))

(defun zero-bytes (number)
  "Make a zero bytearray"
  (make-array number :element-type '(unsigned-byte 8)))

(defun random-bytes (number)
  "Generate random bytes"
  (with-bytearray (bytes number)
    (dotimes (i number)
      (setf (aref bytes i) (random 256)))))

(defun conc-bytes (&rest sequences)
  "Concatenate bytes, integers, or string into a bytearray"
  (apply
   #'concatenate '(vector (unsigned-byte 8))
   (mapcar
    (lambda (elt)
      (cond
        ((integerp elt) (pack elt 1)) ; uses 1 byte per default
        ((stringp elt) (string-to-octets elt))
        (t elt)))
    sequences)))

(defun write-list (list &optional (stream *stream*))
  "Write bytes, integers, or strings to a binary stream"
  (dolist (elt list)
    (cond
      ((integerp elt) (write-int elt 1 stream)) ; uses 1 byte per default
      ((stringp elt) (write-sb elt stream))
      (t (write-bytes elt stream)))))

(define-condition noncongruent-bytes (error) ())

(defun bytes-xor (bytes1 bytes2)
  "XOR for bytearrays (equal length only)"
  (let ((len1 (length bytes1))
        (len2 (length bytes2)))
    (if (not (eql len1 len2)) (error 'noncongruent-bytes))
    (with-bytearray (out len1)
      (dotimes (i len1)
        (setf (aref out i) (boole boole-xor (aref bytes1 i) (aref bytes2 i)))))))

(defun bytes-bit-vector (bytes bit-vector)
  "Copy bytes into a bit-vector (big endian)"
  (dotimes (i (length bit-vector))
    (multiple-value-bind (j k) (floor i 8)
      (setf (sbit bit-vector i)
            (if (logbitp (- 7 k) (aref bytes j)) 1 0)))))
