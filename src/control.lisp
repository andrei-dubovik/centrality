;; Copyright (c) 2020 Andrey Dubovik <andrei@dubovik.eu>

;; Torrent download control (finding peers, initiating download, etc)

(in-package :centrality)

(defparameter *no-connections* 10) ; number of simultaneous connections per torrent
(defparameter *exploration* 0.5)   ; probability for selecting a fresh peer
(defparameter *min-blocks* 1)      ; minimum number of downloaded blocks to consider a reconnect

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

(defun control-loop (torrent &rest rest &key (blacklist #'empty-blacklist) trackers &allow-other-keys)
  "Initiate new connections, keep track of past and present peers"
  (let ((torrent (new-tr torrent)) ; TODO: move this initialization higher up the stack
        (peers (make-hash-table :test #'equalp))
        (new-peers (make-queue))
        (alarm (make-semaphore)))
    (open-storage torrent)
    (dolist (tracker (adjoin (tr-announce torrent) trackers :test #'equalp))
      (apply #'open-tracker tracker torrent new-peers alarm rest))
    (while (wait-on-semaphore alarm)

      ;; Check for new peers
      (let (log-new log-black)
        (while (peer = (dequeue new-peers))
          (if (funcall blacklist peer)
              (push peer log-black)
              (when (not (gethash peer peers))
                (push peer log-new)
                (setf (gethash peer peers) (make-peer :address peer)))))
        (if (> (length log-new) 0)
            (log-msg 2 :event :newpeers :torrent (format-hash torrent)
                     :count (length log-new) :peers log-new :blacklisted log-black)))

      ;; Open new connections
      (dolist (peer (choose-peers peers *no-connections*))
        (setstruct (peer peer)
          (:active t)
          (:no-blocks 0))
        (apply #'open-channel torrent peer new-peers alarm rest)))))


(defun start (torrent &rest rest)
  "Start torrent download"
  (make-thread (lambda () (apply #'control-loop torrent rest)) :name "centrality-control"))
