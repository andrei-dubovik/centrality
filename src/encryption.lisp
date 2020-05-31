;; Copyright (c) 2020 Andrey Dubovik <andrei@dubovik.eu>

;; BitTorrent obfuscation protocol

;; Utility functions

(in-package :centrality)

(define-condition rc4-negotiation-falure (error) ())

(defun forward (stream bytes limit)
  "Read from stream until given bytes sequence is found"
  (do ((i 0)
       (j 0 (1+ j)))
      ((eql i (length bytes)) j)
    (if (eql j limit) (error 'rc4-negotiation-failure))
    (if (eql (read-byte stream) (aref bytes i))
        (setq i (1+ i))
        (setq i 0))))

;; Streams

(defclass rc4-stream (fundamental-input-stream fundamental-output-stream)
  ((stream :initarg :stream :reader .stream)
   (in :initarg :in :reader .in)
   (out :initarg :out :reader .out)))

(defmethod stream-read-byte ((stream rc4-stream))
  (let ((byte (read-byte (.stream stream) nil :eof)))
    (if (eql byte :eof) :eof
        (rc4-byte (.in stream) byte))))

(defmethod stream-read-sequence ((stream rc4-stream) seq start end &key)
  (let ((len (- end start)))
    (rc4-sequence-into
     (.in stream) (read-bytes len (.stream stream)) seq :out-offset start))
  end)

(defmethod stream-write-byte ((stream rc4-stream) byte)
  (write-byte (rc4-byte (.out stream) byte) (.stream stream))
  byte)

(defmethod stream-write-sequence ((stream rc4-stream) seq start end &key)
  (write-bytes
   (rc4-sequence (.out stream) seq :offset start :len (- end start))
   (.stream stream))
  seq)

(defmethod stream-finish-output ((stream rc4-stream))
  (finish-output (.stream stream)))

(defmethod stream-listen ((stream rc4-stream))
  (listen (.stream stream)))

(defmethod close ((stream rc4-stream) &key abort)
  (close (.stream stream) :abort abort))

(defun make-rc4-stream (stream &key in out)
  "Make an RC4 encryption/decryption stream"
  (make-instance 'rc4-stream :stream stream :in in :out out))

;; BitTorrent Extension

(define-condition rc4-refused (error) ())
(define-condition padding-overflow (error) ())

(defun open-rc4-tunnel (stream hash)
  "Negotiate RC4 connection, return transparent stream"
  (let* ((*stream* stream)
         (xa (random (expt 2 160)))         ; A's private key
         (ya (expt-mod 2 xa *p*))           ; A's public key
         (pada (random-bytes (random 513))) ; random padding
         (padc (zero-bytes (random 513)))   ; zero padding
         yb s rc4a rc4b rc4s)

    ;; 1. A sends its public key to B
    (write-int ya 96)
    (write-bytes pada)
    (finish-output stream)

    ;; 2. A receives B's public key
    (setq yb (read-int 96)                                       ; B's public key
          s (pack (expt-mod yb xa *p*) 96)                       ; DH secret
          rc4a (rc4-init (sha1 (conc-bytes "keyA" s hash)) 1024) ; RC4 key for sending
          rc4b (rc4-init (sha1 (conc-bytes "keyB" s hash)) 1024) ; RC4 key for receiving
          rc4s (make-rc4-stream stream :in rc4b :out rc4a))      ; transparent RC4 stream

    ;; 3. A proofs it has private key and knows shared secret (torrent hash)
    (write-bytes (sha1 (conc-bytes "req1" s)))
    (write-bytes
     (bytes-xor (sha1 (conc-bytes "req2" hash))
                (sha1 (conc-bytes "req3" s))))
    (write-list
     (list *vc* *crypto-provide* (pack (length padc) 2) padc (literal-bytes 0 0)) rc4s)
    (finish-output rc4s)

    ;; 4. A verifies B has private key and knows shared secret
    (forward stream (rc4-sequence rc4b *vc*) 520)

    ;; Double check RC4 acceptance
    (if (not (equalp (read-bytes 4 rc4s) *crypto-provide*))
        (error 'rc4-refused))

    ;; Skip padding and return transparent stream
    (let ((padd (read-int 2 rc4s)))
      (if (> padd 512) (error 'padding-overflow))
      (read-bytes padd rc4s))
    rc4s))
