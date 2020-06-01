;; Copyright (c) 2020 Andrey Dubovik <andrei@dubovik.eu>

;; Configuration and technical parameters

;; Changing some of the parameters requires recompilation for the
;; changes to take effect.

(in-package :centrality)

;; Identification

(defparameter *call-sign* "-CY0000-")        ; base protocol identification
(defparameter *call-name* "Centrality 0.0")  ; extended protocol identification
(defparameter *user-agent* "Centrality/0.0") ; identification for http trackers

;; File operations

(defparameter *file-dir* "./")

;; Trackers

(defparameter *tracker-timeout* 120)
(defparameter *tracker-interval* 600)

;; Connections

(defparameter *listen-port* 6881)  ; not implemented, sent to trackers
(defparameter *no-connections* 10) ; number of simultaneous connections per torrent
(defparameter *exploration* 0.5)   ; probability for selecting a fresh peer
(defparameter *min-blocks* 1)      ; minimum number of downloaded blocks to consider a reconnect

;; Congestion control
;; (congestion control is primitive at the moment)

(defparameter *clock* (ceiling internal-time-units-per-second 10))
(defparameter *precision* (coerce internal-time-units-per-second 'float))
(defparameter *minimum-window* 256)

;; Logging

(defparameter *level-file* 3)
(defparameter *level-print* 2)

;; Protocol and extensions

(defparameter *protocol* (conc-bytes 19 (string-to-octets "BitTorrent protocol")))
(defparameter *block-length* (expt 2 14))
(defparameter *extensions* (ash 1 20)) ; extension protocol
(defparameter *ext-msg-ids* '(("ut_pex" . 1)))

;; Encryption constants

(defparameter *p* #xFFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD129024E088A67CC74020BBEA63B139B22514A08798E3404DDEF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245E485B576625E7EC6F44C42E9A63A36210000000000090563)
(defparameter *vc* (zero-bytes 8))
(defparameter *crypto-provide* (literal-bytes 0 0 0 2))
