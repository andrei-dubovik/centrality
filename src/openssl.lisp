;; Copyright (c) 2020 Andrey Dubovik <andrei@dubovik.eu>

;;; OpenSSL wrappers

(in-package :centrality)

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
