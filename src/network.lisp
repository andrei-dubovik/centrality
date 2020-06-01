;; Copyright (c) 2020 Andrey Dubovik <andrei@dubovik.eu>

;; Sending (encoding) and receiving (decoding) BitTorrent messages

(in-package :centrality)

;; Work in progress

(defclass network ()
  ((stream :initarg :stream :reader .stream)))

(defmacro eid (message)
  "Lookup extended id for a given message"
  (getvalue message *ext-msg-ids*))

(defun send-handshake (exts hash peerid &optional (stream *stream*))
  "Send BitTorrent handshake message"
  (write-list
   (list *protocol* (pack exts 8) hash peerid) stream)
  (finish-output stream))

(define-condition bad-protocol (error) ())

(defun recv-handshake (&optional (stream *stream*))
  "Receive BitTorrent handshake message"
  (let ((*stream* stream))
    (if (not (equalp (read-bytes 20) *protocol*))
        (error 'bad-protocol))
    (list (read-int 8) (read-bytes 20) (read-bytes 20))))

(define-condition unknown-message-id (error) ())
(define-condition unknown-extended-message-id (error) ())
(define-condition message-size-exceeded (error) ())

(defun receive-extended (id len stream)
  "Receive extended BitTorrent message"
  (cond
    ((eql id 0) `(:ext-handshake ,(decode-bounded stream len)))
    ((eql id (eid "ut_pex")) `(:ext-pex ,(decode-bounded stream len)))
    (t (error 'unknown-extended-message-id))))

(defun receive-message (stream)
  "Receive BitTorrent message"
  (let* ((*stream* stream)
         (len (read-int 4)))
    (if (not (zerop len))
        (progn
          (if (> len (+ 9 (expt 2 15))) (error 'message-size-exceeded))
          (case (byte-read)
            (0 '(:choke))
            (1 '(:unchoke))
            (2 '(:interested))
            (3 '(:notinterested))
            (4 `(:have ,(read-int 4)))
            (5 `(:bitfield ,(read-bytes (- len 1))))
            (6 `(:request ,(read-int 4) ,(read-int 4) ,(read-int 4)))
            (7 `(:piece ,(read-int 4) ,(read-int 4) ,(read-bytes (- len 9))))
            (8 `(:cancel ,(read-int 4) ,(read-int 4) ,(read-int 4)))
            (9 `(:port ,(read-int 2)))
            (20 (receive-extended (byte-read) (- len 2) stream))
            (t (error 'unknown-message-id))))
        '(:keepalive))))

(defun send-extended (id args stream)
  "Send BitTorrent message"
  (let ((*stream* stream))
    (case id
      (:ext-handshake
       (destructuring-bind (handshake) args
         (multiple-value-bind (bytes len) (encode-to-bytes handshake)
           (write-int (+ len 2) 4)
           (write-int 20 1) (write-int 0 1)
           (write-bytes bytes)))))))

(defcall id ((network network) &rest args)
  "Send BitTorrent message"
  (let ((*stream* (.stream network)))
    (case id
      (:keepalive     (write-int 0 4))
      (:choke         (write-int 1 4) (byte-write 0))
      (:unchoke       (write-int 1 4) (byte-write 1))
      (:interested    (write-int 1 4) (byte-write 2))
      (:notinterested (write-int 1 4) (byte-write 3))
      (:have
       (destructuring-bind (piece-index) args
         (write-int 5 4) (byte-write 4)
         (write-int piece-index 4)))
      (:bitfield
       (destructuring-bind (bitfield) args
         (write-int (1+ (length bitfield)) 4)
         (byte-write 5)
         (write-bytes bitfield)))
      (:request
       (destructuring-bind (index begin length) args
         (write-int 13 4) (byte-write 6)
         (write-int index 4) (write-int begin 4) (write-int length 4)))
      (:piece
       (destructuring-bind (index begin block) args
         (write-int (+ 9 (length block)) 4)
         (byte-write 7)
         (write-int index 4) (write-int begin 4) (write-bytes block)))
      (:cancel
       (destructuring-bind (index begin length) args
         (write-int 13 4) (byte-write 8)
         (write-int index 4) (write-int begin 4) (write-int length 4)))
      (:port
       (destructuring-bind (listen-port) args
         (write-int 3 4) (byte-write 9) (write-int listen-port 2)))
      (t (send-extended id args *stream*)))
    (finish-output *stream*)))
