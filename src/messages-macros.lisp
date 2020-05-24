;; Copyright (c) 2020 Andrey Dubovik <andrei@dubovik.eu>

;; A few macros used by messages.lisp

(in-package :centrality)

(defparameter *ext-msg-ids* '(("ut_pex" . 1)))

(defmacro eid (message)
  "Lookup extended id for a given message"
  (getvalue message *ext-msg-ids*))
