;; Copyright (c) 2020 Andrey Dubovik <andrei@dubovik.eu>

;; Peer related operations

(in-package :ansible)

(defparameter *clock* (* 0.1 internal-time-units-per-second))
(defparameter *minimum-window* 256) ; primitive congestion control
(defparameter *call-sign* "-AS0000-")

;; Call sign

(defun random-peerid ()
  "Generate peer id with a random suffix"
  (conc-bytes *call-sign* (random-bytes 12)))

;; Four dynamics contexts are used, all four are established by (open-channel ...)

(defvar *socket*)  ; connection socket
(defvar *stream*)  ; encoded stream used to communicate with the peer
(defvar *torrent*) ; a shared structure for torrent bookkeeping
(defvar *peer*)    ; a shared structure for peer bookkeeping

;; Scheduler

(defun choose-piece ()
  "Choose a random piece to download"
  (let ((mask (bit-ior (tr-piece-mask *torrent*)
                       (peer-req-mask *peer*)
                       (bit-not (peer-avl-mask *peer*)))))
    (find-if (lambda (i) (zerop (sbit mask i)))
             (random-permutation (tr-no-pieces *torrent*)))))

;; Sending messages

(defun request-piece (pid)
  "Queue one piece for download in random order"
  (iter (for bid in-vector (random-permutation (piece-size pid *torrent*)))
        (send (:request pid (* bid *block-length*) (block-length pid bid *torrent*))))
  (setf (sbit (peer-req-mask *peer*) pid) 1)
  (incf (peer-window *peer*) (piece-size pid *torrent*)))

(defun send-messages ()
  "Gets called at *clock* intervals to send messages"
  (if (and
       (< (peer-window *peer*) *minimum-window*)
       (not (peer-choked *peer*)))
      (if-let (piece (choose-piece))
        (request-piece piece))))

;; Receiving messages

(defgeneric process (type body)
  (:documentation "Process incoming message"))

(defmethod process (type body))

(defmethod process ((type (eql :unchoke)) body)
  (setf (peer-choked *peer*) nil))

(defmethod process ((type (eql :choke)) body)
  (setf (peer-choked *peer*) t))

(defmethod process ((type (eql :bitfield)) body)
  "Set initial bitfield"
  (destructuring-bind (bitfield) body
    (bytes-bit-vector bitfield (peer-avl-mask *peer*))))

(defmethod process ((type (eql :have)) body)
  "Register a new available piece"
  (destructuring-bind (pid) body
    (setf (sbit (peer-avl-mask *peer*) pid) 1)))

(define-condition block-bad-offset (error) ())
(define-condition block-bad-size (error) ())

(defmethod process ((type (eql :piece)) body)
  "Check block for errors, send to storage thread"
  (destructuring-bind (pid offset block) body
    (multiple-value-bind (bid rem) (floor offset *block-length*)
      (if (not (zerop rem)) (error 'block-bad-offset))
      (if (not (eql (length block) (block-length pid bid *torrent*)))
          (error 'block-bad-size)))
    (decf (peer-window *peer*)))
  (incf (peer-no-blocks *peer*))
  (send-msg (tr-queue *torrent*) body))

;; Handshake

(defun reset-peer (peer peerid exts torrent)
  "Reset specific peer slots"
  (setstruct (peer peer)
    (:peerid peerid)
    (:exts exts)
    (:choked t)
    (:avl-mask (make-array (tr-no-pieces torrent) :element-type 'bit))
    (:req-mask (make-array (tr-no-pieces torrent) :element-type 'bit))
    (:window 0)))

(define-condition bad-hash (error) ())

(defun peer-connect ()
  "Establish an encrypted connection to peer"
  (let ((stream (open-rc4-tunnel (socket-stream *socket*) (tr-hash *torrent*))))
    (send-handshake (zero-bytes 8) (tr-hash *torrent*) (random-peerid) stream)
    (destructuring-bind (exts hash peerid) (recv-handshake stream)
      (if (not (equalp hash (tr-hash *torrent*)))
          (error 'bad-hash))
      (reset-peer *peer* peerid exts *torrent*)
      (log-msg 2 :event :connect :torrent (format-hash *torrent*) :peer (peer-address *peer*)
                 :extensions exts :id (quri:url-encode peerid)))
    stream))

;; Event loop

;; Inbound messages are received as soon as they arrive. Outbound
;; messages are sent at regular intervals (required for bandwidth
;; management). If the loop is behind or ahead of the clock when
;; sending messages, the frequency is adjusted to catch up with the
;; clock.

(define-condition closed-connection (error) ())

(defun channel-loop (clock timeout)
  "Receive messages on arrival, send messages at regular intervals"
  (declare (optimize (debug 0))) ; tail-call optimization required
  (multiple-value-bind (socket remain) (wait-for-input *socket* :timeout timeout)
    (let ((empty (not (listen *stream*))))

      ;; a hackish way to detect a closed connection
      ;; (see https://stackoverflow.com/questions/61306791)
      (if (and (eql timeout remain) empty)
          (error 'closed-connection))

      ;; receive messages if any
      (while (listen *stream*)
        (destructuring-bind (type &rest rest) (recv)
          (process type rest)))

      ;; send messages if it is time or if overdue
      (when (or
             empty
             (> (floor (- (get-internal-real-time) clock) *clock*) 0))
        (send-messages)
        (setq clock (if clock (+ clock *clock*) (get-internal-real-time))))) ; accounts for first run

    ;; shedule next operation
    (channel-loop
     clock
     (/ (max 0 (- (+ clock *clock*) (get-internal-real-time)))
        (coerce internal-time-units-per-second 'float)))))

(defun channel-init (torrent peer queue alarm &rest rest)
  "Establish dynamic contexts, connect to peer and start trasnferring"
  (let ((socket (apply #'open-tcp (peer-address peer) rest)))
    (unwind-protect
         (let* ((*torrent* torrent)
                (*peer* peer)
                (*socket* socket)
                (*stream* (peer-connect)))
           (send (:interested)) ; TODO: temporary here, belongs elsewhere
           (channel-loop nil 0.0))
      (socket-close socket))))

(defun channel-catch (torrent peer queue alarm &rest rest)
  "A wrapper around channel-init that handles conditions"
  (handler-case (apply #'channel-init torrent peer queue alarm rest)
    (error (e)
      (setf (peer-active peer) nil)
      (signal-semaphore alarm)
      (log-msg 2 :event :abort :hash (format-hash torrent) :peer (peer-address peer) :condition (type-of e)))))

(defun open-channel (torrent peer queue alarm &rest rest)
  "Initiate peer connection in a separate thread"
  (make-thread (lambda () (apply #'channel-catch torrent peer queue alarm rest)) :name "ansible-channel"))
