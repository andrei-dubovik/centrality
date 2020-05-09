;; Copyright (c) 2020 Andrey Dubovik <andrei@dubovik.eu>

;;; OpenSSL wrappers

(in-package :centrality)

;; SHA1

(define-foreign-library libcrypto
  (t (:default "libcrypto")))

(use-foreign-library libcrypto)

(defcfun (c-sha1 "SHA1") :unsigned-char
  (d :pointer)
  (len :unsigned-int)
  (md :pointer))

(defun sha1 (data)
  "Compute SHA1 checksum (openssl wrapper)"
  (with-bytearray (md 20)
    (with-pointer-to-vector-data (p-data data)
      (with-pointer-to-vector-data (p-md md)
        (c-sha1 p-data (length data) p-md)))))

;; RC4

(defcstruct rc4-key-st
  (x :unsigned-int)
  (y :unsigned-int)
  (data :unsigned-int :count 256))

(defcfun (rc4-set-key "RC4_set_key") :void
  (key :pointer)
  (len :int)
  (data :pointer))

(defcfun (rc4 "RC4") :void
  (key :pointer)
  (len :unsigned-int)
  (indata :pointer)
  (outdata :pointer))

(defun rc4-init (key &optional (burnin 0))
  "Initialize RC4 cipher"
  (let* ((state* (foreign-alloc '(:struct rc4-key-st)))
         (state (cons state* nil))) ; a wrapper around state* to attach finalization to
    (with-pointer-to-vector-data (key* key)
      (rc4-set-key state* (length key) key*)
      (rc4-sequence state (zero-bytes burnin)))
    (finalize state (lambda () (foreign-free state*)))
    state))

(defun rc4-byte (state byte)
  "Encrypt/decrypt a single byte using RC4"
  (with-foreign-objects ((in* :unsigned-char)
                         (out* :unsigned-char))
    (setf (mem-ref in* :unsigned-char) byte)
    (rc4 (car state) 1 in* out*)
    (mem-ref out* :unsigned-char)))

(defun rc4-sequence-into (state in out &key (in-offset 0) (out-offset 0) (len (length in)))
  "Encrypt/decrypt a byte sequence using RC4"
  (with-pointer-to-vector-data (in* in)
    (with-pointer-to-vector-data (out* out)
      (rc4 (car state) len (inc-pointer in* in-offset) (inc-pointer out* out-offset)))))

(defun rc4-sequence (state seq &key (offset 0) (len (length seq)))
  "Encrypt/decrypt a byte sequence using RC4"
  (with-bytearray (out len)
    (rc4-sequence-into state seq out :in-offset offset :len len)))
