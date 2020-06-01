;; Copyright (c) 2020 Andrey Dubovik <andrei@dubovik.eu>

;; TODO: some earlier files still reference some future files

(defsystem "centrality"
  :depends-on ("cffi" "dexador" "flexi-streams" "iterate" "quri" "usocket" "trivial-garbage" #+sbcl "sb-concurrency")
  :serial t
  :components ((:file "src/package")
               (:file "src/utilities")
               (:file "src/mailbox")
               (:file "src/math")
               (:file "src/bytes")
               (:file "src/params")
               (:file "src/messages")
               (:file "src/log")
               (:file "src/socks")
               (:file "src/openssl")
               (:file "src/encryption")
               (:file "src/bencode")
               (:file "src/torrent")
               (:file "src/tracker")
               (:file "src/network")
               (:file "src/storage")
               (:file "src/channel")
               (:file "src/control")))
