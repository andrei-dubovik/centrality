;; Copyright (c) 2020 Andrey Dubovik <andrei@dubovik.eu>

;; SOCKS5 interface

(in-package :ansible)

;; Define SOCKS errors
(define-condition socks-general-error (error) ())
(define-condition socks-auth-error (error) ())
(define-condition socks-not-allowed (error) ())
(define-condition socks-network-unreachable (error) ())
(define-condition socks-host-unreachable (error) ())
(define-condition socks-connection-refused (error) ())
(define-condition socks-ttl-expired (error) ())
(define-condition socks-protocol-error (error) ())
(define-condition socks-bad-address (error) ())

;; some functions from bytes.lisp have come in handy, here and in
;; encryption.lisp, but those utility functions could be be improved

;; TODO: add support for IPv6 addresses (here and elsewhere)

(defvar *stream*)

(defmacro read-check (values error &optional (stream '*stream*))
  "Read bytes from *stream*, compare to values, raise error if not equal"
  `(if (not (and ,@(mapcar (lambda (val) `(eql ,val (byte-read ,stream))) values))) (error ,error)))

(defun write-address (address &optional (stream *stream*))
  "Write address to stream as per SOCKS specification"
  (destructuring-bind (host . port) address
    (cond
      ((stringp host)
       (write-list (list 3 (length host) host) stream))
      (t
       (write-list (list 1 host) stream)))
    (write-bytes (pack port 2) stream)))

(defun open-socks (address proxy auth)
  "Open a SOCKS5 tunnel using provided credentials"
  (destructuring-bind (host . port) proxy
    (let* ((socket (socket-connect host port :protocol :stream :element-type '(unsigned-byte 8)))
           (*stream* (socket-stream socket)))

      ;; Negotiate authentication method
      ;; (clear and username/password authentication are supported)
      (let ((auid (if auth 2 0)))
        (write-list (list 5 1 auid))
        (finish-output *stream*)
        (read-check (5) 'socks-general-error)
        (read-check (auid) 'socks-auth-error))

      ;; Authenticate
      (if auth
          (destructuring-bind (user . password) auth
            (write-list (list 1 (length user) user (length password) password))
            (finish-output *stream*)
            (read-check (1 0) 'socks-auth-error)))

      ;; Request connection
      (write-list (list 5 1 0))
      (write-address address)
      (finish-output *stream*)
      (read-check (5) 'socks-general-error)
      (case (byte-read)
        (0 nil)
        (2 (error 'socks-not-allowed))
        (3 (error 'socks-network-unreachable))
        (4 (error 'socks-host-unreachable))
        (5 (error 'socks-connection-refused))
        (6 (error 'socks-ttl-expired))
        (7 (error 'socks-protocol-error))
        (8 (error 'socks-bad-address))
        (t (error 'socks-general-error)))

      (read-check (0 1) 'socks-general-error) ; when no binding is requested SOCKS returns an empty IPv4
      (read-bytes 6)                          ; skip IPv4 host and port
      socket)))

(defun open-tcp (address &key proxy auth &allow-other-keys)
  "Open either a direct or proxied TCP tunnel"
  (let ((socket
         (if proxy
             (open-socks address proxy auth)
             (destructuring-bind (host . port) address
               (socket-connect host port :protocol :stream :element-type '(unsigned-byte 8))))))
    (log-msg 2 :event :socket :peer address :proxy proxy)
    socket))
