;; Copyright (c) 2020 Andrey Dubovik <andrei@dubovik.eu>

;; Functions to decode/encode bencoded data

(in-package :ansible)

;; Strings

(defun decode-string (bytes)
  "Decode utf8 encoded string"
  (octets-to-string bytes :external-format :utf8))

(defun encode-string (string)
  "Encode utf8 encoded string"
  (string-to-octets string :external-format :utf8))

;; Decoding

(defun decode (stream)
  "Decode bencoded stream"
  (let ((head (code-char (peek-byte stream))))
    (cond
      ((eql head #\i) (decode-integer stream))
      ((eql head #\l) (decode-list stream))
      ((eql head #\d) (decode-dict stream))
      (t (decode-bytes stream)))))

(defun decode-integer (stream &optional (skip t) (delim #\e))
  "Decode integer"
  (if skip (read-byte stream))
  (parse-integer
   (with-output-to-string (out)
     (iter (for i next (code-char (read-byte stream)))
           (until (eql i delim))
           (write-char i out)))))

(defun decode-bytes (stream)
  "Decode bytes sequence"
  (read-bytes (decode-integer stream nil #\:) stream))

(defun decode-list (stream)
  "Decode list"
  (read-byte stream)
  (iter (for head next (code-char (peek-byte stream)))
        (until (eql head #\e))
        (collect (decode stream))
        (finally (read-byte stream))))

(defparameter *key-hook* (lambda (key start end)))

(defun decode-dict (stream)
  "Decode dictionary"
  (read-byte stream)
  (iter (for head next (code-char (peek-byte stream)))
        (until (eql head #\e))
        (let ((key (decode-string (decode stream)))
              (start (flexi-stream-position stream)))
          (collect (cons key (decode stream)))
          (funcall *key-hook* key start (flexi-stream-position stream)))
        (finally (read-byte stream))))

;; Encoding (not used)
;
;(defun encode (data stream)
;  "Bencode data to stream"
;  (cond
;    ((integerp data) (encode-integer data stream))
;    ((arrayp data) (encode-bytes data stream))
;    ((listp data)
;     (if (and (consp (car data)) (stringp (caar data)))
;         (encode-dict data stream)
;         (encode-list data stream)))))
;
;(defun encode-integer (integer stream &optional (start #\i) (end #\e))
;  "Encode integer"
;  (if start (write-cb start stream))
;  (iter (for i in-sequence (write-to-string integer))
;        (write-cb i stream))
;  (write-cb end stream))
;
;(defun encode-bytes (data stream)
;  "Encode byte sequence"
;  (encode-integer (length data) stream nil #\:)
;  (write-sequence data stream))
;
;(defun encode-list (data stream)
;  "Encode list"
;  (write-cb #\l stream)
;  (dolist (item data)
;    (encode item stream))
;  (write-cb #\e stream))
;
;(defun encode-dict (data stream)
;  "Encode dictionary"
;  (write-cb #\d stream)
;  (iter (for (k . v) in data)
;        (encode (encode-string k) stream)
;        (encode v stream))
;  (write-cb #\e stream))

;; Sequence operations

(defun decode-sequence (sequence)
  "Read sequence and return decoded content"
  (with-input-from-sequence (in sequence)
    (decode in)))

(defmethod flexi-stream-position ((stream flexi-streams::vector-input-stream))
  nil)
