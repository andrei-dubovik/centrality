;; Copyright (c) 2020 Andrey Dubovik <andrei@dubovik.eu>

;; Package definition

(defpackage :centrality
  (:use :common-lisp
        :trivial-garbage
        :trivial-gray-streams
        :cffi
        :flexi-streams
        :iterate
        :bordeaux-threads
        :usocket))
