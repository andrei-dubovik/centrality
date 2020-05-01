;; Copyright (c) 2020 Andrey Dubovik <andrei@dubovik.eu>

;; Package definition

(defpackage :ansible
  (:use :common-lisp
        :trivial-gray-streams
        :cffi
        :flexi-streams
        :iterate
        :bordeaux-threads
        :usocket))
