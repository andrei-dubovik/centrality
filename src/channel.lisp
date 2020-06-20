;; Copyright (c) 2020 Andrey Dubovik <andrei@dubovik.eu>

;; Peer related operations

(in-package :centrality)

;; Call sign

(defun random-peerid ()
  "Generate peer id with a random suffix"
  (conc-bytes *call-sign* (random-bytes 12)))

(defun ext-handshake ()
  "Prepare extended handshake"
  `(("m" . ,*ext-msg-ids*) ("v" . ,(encode-string *call-name*))))

;; A number of dynamic contexts are used, all are established by (open-channel ...)
;; (These dynamic contexts will be phased out in future versions of the program)

(defvar *socket*)    ; connection socket
(defvar *network*)   ; a class containing encoded stream used to communicate with the peer
(defvar *torrent*)   ; a shared structure for torrent bookkeeping
(defvar *peer*)      ; a shared structure for peer bookkeeping
(defvar *control*)   ; control thread identifier (new peers are sent there)

;; Work in progress

(defclass peer2 () ())

;; Scheduler

(defun choose-piece ()
  "Choose a random piece to download"
  (let ((mask (bit-ior (tr-piece-mask *torrent*)
                       (peer-req-mask *peer*)
                       (bit-not (peer-avl-mask *peer*)))))
    (find-if (lambda (i) (zerop (sbit mask i)))
             (random-permutation (tr-no-pieces *torrent*)))))

;; Logging

(defun message-digest (msg)
  "Substitute byte fields with their sizes (for the logs)"
  (mapcar
   (lambda (field)
     (if (arrayp field) (length field) field))
   msg))

(defun log-torrent-msg (id args event)
  "Log BitTorrent message"
  (log-msg
   (case id
     ((:request :piece) 4)
     (t 3))
   :event event :torrent (format-hash *torrent*) :peer (peer-address *peer*) :msg (cons id (message-digest args))))

(defcall method :before ((peer peer2) &rest args)
  "Log incoming BitTorrent message"
  (log-torrent-msg method args :msg-recv))

(defcall method :before ((network network) &rest args)
  "Log outgoing BitTorrent message"
  (log-torrent-msg method args :msg-sent))

;; Sending messages

(defmacro send-network (msg)
  "A shorthand for (call network ...)"
  `(call *network* ,@msg))

(defun request-piece (pid)
  "Queue one piece for download in random order"
  (iter (for bid in-vector (random-permutation (piece-size pid *torrent*)))
        (send-network (:request pid (* bid *block-length*) (block-length pid bid *torrent*))))
  (setf (sbit (peer-req-mask *peer*) pid) 1)
  (incf (peer-window *peer*) (piece-size pid *torrent*)))

(defun send-messages ()
  "Gets called at *clock* intervals to send messages"
  (if (and
       (< (peer-window *peer*) *minimum-window*)
       (not (peer-choked *peer*)))
      (if-let (piece (choose-piece))
        (request-piece piece))))

(defun send-init ()
  "Send initial messages"
  (send-network (:interested))
  (if (logbitp 20 (peer-exts *peer*))
      (send-network (:ext-handshake (ext-handshake)))))

;; Receiving messages

(defcall :unchoke ((peer peer2) &args)
  (setf (peer-choked *peer*) nil))

(defcall :choke ((peer peer2) &args)
  (setf (peer-choked *peer*) t))

(defcall :bitfield ((peer peer2) &args bitfield)
  "Set initial bitfield"
  (bytes-bit-vector bitfield (peer-avl-mask *peer*)))

(defcall :have ((peer peer2) &args pid)
  "Register a new available piece"
  (setf (sbit (peer-avl-mask *peer*) pid) 1))

(define-condition block-bad-offset (error) ())
(define-condition block-bad-size (error) ())

(defcall :piece ((peer peer2) &args pid offset block)
  "Check block for errors, send to storage thread"
  (multiple-value-bind (bid rem) (floor offset *block-length*)
    (if (not (zerop rem)) (error 'block-bad-offset))
    (if (not (eql (length block) (block-length pid bid *torrent*)))
        (error 'block-bad-size)))
  (decf (peer-window *peer*))
  (incf (peer-no-blocks *peer*))
  (send (tr-queue *torrent*) :block pid offset block))

;; Receiving extended messages

(defcall :ext-handshake ((peer peer2) &args msg)
  "Process incoming extended handshake"
  (let ((yourip (getvalue "yourip" msg)))
    (if yourip (send *control* :yourip yourip))))

(defcall :ext-pex ((peer peer2) &args peers)
  "Register new peers with the control thread"
  (send *control* :peers (split-peers (getvalue "added" peers) 6)))

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
    (send-handshake *extensions* (tr-hash *torrent*) (random-peerid) stream)
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

(defun channel-loop (clock timeout peer2)
  "Receive messages on arrival, send messages at regular intervals"
  (declare (optimize (debug 0)))     ; tail-call optimization required
  (let ((stream (.stream *network*))) ; A temporary solution, *network* belongs in peer2
    (multiple-value-bind (socket remain)
        (wait-for-input *socket* :timeout timeout)
      (declare (ignore socket))
      (symbol-macrolet ((tick (ceiling (- (1+ (get-internal-real-time)) clock) *clock*))) ; missed clock cycles

        ;; a hackish way to detect a closed connection
        ;; (see https://stackoverflow.com/questions/61306791)
        (if (and (eql timeout remain) (not (listen stream)))
            (error 'closed-connection))

        ;; receive messages if any
        (while (and (listen stream) (<= tick 0))
          (apply #'call peer2 (receive-message stream)))

        ;; send messages if it is time or if overdue
        (when (> tick 0)
          (send-messages)
          (setq clock (+ clock *clock*))))

      ;; shedule next operation
      (channel-loop
       clock
       (/ (max (- clock (get-internal-real-time)) 0) *precision*)
       peer2))))

(defun channel-init (torrent peer control &rest rest)
  "Establish dynamic contexts, connect to peer and start trasnferring"
  (let ((socket (apply #'open-tcp (peer-address peer) rest)))
    (unwind-protect
         (let* ((*torrent* torrent)
                (*peer* peer)
                (*socket* socket)
                (*network* (make-instance 'network :stream (peer-connect)))
                (*control* control))
           (send-init)
           (channel-loop (get-internal-real-time) 0 (make-instance 'peer2)))
      (socket-close socket))))

(defworker channel-catch (torrent peer control &rest rest)
  "A wrapper around channel-init that handles conditions"
  (handler-case (apply #'channel-init torrent peer control rest)
    (error (e)
      (setf (peer-active peer) nil)
      (send control :respawn)
      (log-msg 2 :event :abort :hash (format-hash torrent) :peer (peer-address peer) :condition (type-of e)))))
