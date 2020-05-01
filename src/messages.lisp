;; Copyright (c) 2020 Andrey Dubovik <andrei@dubovik.eu>

;; Sending (encoding) and receiving (decoding) BitTorrent messages

(in-package :centrality)

(defvar *torrent*) ; a shared structure for torrent bookkeeping
(defvar *peer*)    ; a private structure for peer bookkeeping

(defparameter *protocol*
  (conc-bytes 19 (string-to-octets "BitTorrent protocol")))

(defun send-handshake (exts hash peerid &optional (stream *stream*))
  "Send BitTorrent handshake message"
  (write-list
   (list *protocol* exts hash peerid) stream)
  (finish-output stream))

(define-condition bad-protocol (error) ())

(defun recv-handshake (&optional (stream *stream*))
  "Receive BitTorrent handshake message"
  (let ((*stream* stream))
    (if (not (equalp (read-bytes 20) *protocol*))
        (error 'bad-protocol))
    (list (read-bytes 8) (read-bytes 20) (read-bytes 20))))

(define-condition unknown-message-id (error) ())
(define-condition message-size-exceeded (error) ())

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
            (t (error 'unknown-message-id))))
        '(:keepalive))))

(defun send-message (msg stream)
  "Send BitTorrent message"
  (let ((*stream* stream))
    (destructuring-bind (id &rest msg) msg
      (case id
        (:keepalive     (write-int 0 4))
        (:choke         (write-int 1 4) (byte-write 0))
        (:unchoke       (write-int 1 4) (byte-write 1))
        (:interested    (write-int 1 4) (byte-write 2))
        (:notinterested (write-int 1 4) (byte-write 3))
        (:have
         (destructuring-bind (piece-index) msg
           (write-int 5 4) (byte-write 4)
           (write-int piece-index 4)))
        (:bitfield
         (destructuring-bind (bitfield) msg
           (write-int (1+ (length bitfield)) 4)
           (byte-write 5)
           (write-bytes bitfield)))
        (:request
         (destructuring-bind (index begin length) msg
           (write-int 13 4) (byte-write 6)
           (write-int index 4) (write-int begin 4) (write-int length 4)))
        (:piece
         (destructuring-bind (index begin block) msg
           (write-int (+ 9 (length block)) 4)
           (byte-write 7)
           (write-int index 4) (write-int begin 4) (write-bytes block)))
        (:cancel
         (destructuring-bind (index begin length) msg
           (write-int 13 4) (byte-write 8)
           (write-int index 4) (write-int begin 4) (write-int length 4)))
        (:port
         (destructuring-bind (listen-port) msg
           (write-int 3 4) (byte-write 9) (write-int listen-port 2))))))
  (finish-output stream))

(defun message-digest (msg)
  "Substitute byte fields with their sizes (for the logs)"
  (mapcar
   (lambda (field)
     (if (arrayp field) (length field) field))
   msg))

;; TODO: uses *peer* (should either add "peer" explicitly, or replace "stream" with "*stream*")
(defun log-torrent-msg (msg event)
  "Log BitTorrent message"
  (let* ((id (car msg))
         (level (if (or (eql id :request) (eql id :piece)) 4 3)))
    (log-msg level :event event :torrent (format-hash *torrent*) :peer (peer-address *peer*) :msg (message-digest msg))))

(defun receive-and-log (stream)
  "Receive and log BitTorrent message"
  (let* ((msg (receive-message stream)))
    (log-torrent-msg msg :msg-recv)
    msg))

(defun send-and-log (msg stream)
  "Send and log BitTorrent message"
  (send-message msg stream)
  (log-torrent-msg msg :msg-sent))

(defmacro recv (&optional (stream '*stream*))
  "A wrapper around receive-and-log with a shorter name"
  `(receive-and-log ,stream))

(defmacro send (msg &optional (stream '*stream*))
  "A wrapper around send-and-log to avoid typing **list**, and with a shorter name"
  `(send-and-log ,(cons 'list msg) ,stream))
