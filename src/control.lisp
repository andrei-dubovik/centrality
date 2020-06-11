;; Copyright (c) 2020 Andrey Dubovik <andrei@dubovik.eu>

;; Torrent download control (finding peers, initiating download, etc)

(in-package :centrality)

(defstruct peer
  ;; Managed by control.lisp
  address    ; ip address and port number

  ;; Shared between control.lisp and channel.lisp
  active     ; whether there is an open channel to the peer
  no-blocks  ; number of downloaded blocks (or nil for a new peer)

  ;; Managed by channel.lisp
  peerid     ; remote peer id
  exts       ; supported extensions
  choked     ; t if choked else nil
  avl-mask   ; bitmask of pieces that peer has
  req-mask   ; bitmask of requested pieces
  window     ; congestion windows (in blocks)
)

(defclass control ()
  ((channel :initarg :channel :reader .channel)
   (torrent :initarg :torrent :reader .torrent)
   (peers :initform (make-hash-table :test #'equalp) :reader .peers)
   (blacklist :initarg :blacklist :reader .blacklist)
   (args :initarg :args :reader .args)))

;; Algorithm for selecting new peers
;; - if we tried to connect to a peer before but fewer than *min-blocks* were received, skip
;; - choose a fresh peer with probability *exploration*, and an existing peer with one minus that
;; - among existing peers, prefer those from whom most blocks were downloaded
;; - if there are not enough fresh or existing peers, choose what is there

(defun choose-peers (peers total)
  "Choose new peers to connect to, so to as to have a given total of active peers"
  (let ((active 0)
        (buffer (make-array 0 :adjustable t)))
    (iter (for (addr peer) in-hashtable peers)
          (if (peer-active peer)
              (incf active)
              (let ((nb (peer-no-blocks peer)))
                (if (or (not nb) (>= nb *min-blocks*))
                    (vector-push-extend peer buffer)))))
    (sort buffer (lambda (x y) (or (not x) (and y (< x y)))) :key #'peer-no-blocks)
    (nreverse
     (do ((i active (1+ i))
          (l -1)
          (r (length buffer))
          (list nil))
         ((or (eql i total) (eql l (1- r))) list)
       (push (elt buffer (if (< (random 1.0) *exploration*) (incf l) (decf r))) list)))))

(defun empty-blacklist (p)
  "A default blacklist checker"
  (declare (ignore p))
  nil)

(defcall :peers ((state control) &args peers)
  "Add new peer to the list of known peers"
  (let (log-new log-black)
    (dolist (peer peers)
      (if (funcall (.blacklist state) peer)
          (push peer log-black)
          (when (not (gethash peer (.peers state)))
            (push peer log-new)
            (setf (gethash peer (.peers state)) (make-peer :address peer)))))
    (when (> (length log-new) 0)
      (send (.channel state) :respawn)
      (log-msg 2 :event :newpeers :torrent (format-hash (.torrent state))
               :count (length log-new) :peers log-new :blacklisted log-black))))

(defcall :respawn ((state control) &args)
  "Initiate new connections"
  (dolist (peer (choose-peers (.peers state) *no-connections*))
    (setstruct (peer peer)
      (:active t)
      (:no-blocks 0))
    (apply #'open-channel (.torrent state) peer (.channel state) (.args state))))

(defcall :tracker ((state control) &args tracker)
  "Start a new thread that regulary pulls from a tracker"
  (apply #'open-tracker tracker (.torrent state) (.channel state) (.args state)))

(defworker control-loop (torrent &rest rest &key (blacklist #'empty-blacklist) trackers &allow-other-keys)
  "Initiate new connections, keep track of past and present peers"
  (let ((state
         (make-instance
          'control
          :channel (channel)
          :torrent (new-tr torrent) ; TODO: move this initialization higher up the stack
          :blacklist blacklist
          :args rest)))
    (open-storage (.torrent state))
    (dolist (tracker (adjoin (tr-announce (.torrent state)) trackers :test #'equalp))
      (send (channel) :tracker tracker))
    (while (msg = (receive))
      (apply #'call state msg))))

(defun start (torrent &rest rest)
  "Start torrent download"
  (apply #'spawn nil #'control-loop torrent rest))
