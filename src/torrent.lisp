;; Copyright (c) 2020 Andrey Dubovik <andrei@dubovik.eu>

;; Torrent files, torrent structures

(in-package :centrality)

;; File operations

(defun read-chunk (filename start end)
  "Read a binary chunk from a file"
  (with-open-file (in filename :element-type '(unsigned-byte 8))
    (file-position in start)
    (read-bytes (- end start) in)))

(defun decode-file (filename)
  "Read file and return decoded content"
  (with-open-file (in filename :element-type '(unsigned-byte 8))
    (decode (make-flexi-stream in))))

(defun read-torrent (filename)
  "Read and decode a torrent file, compute info_hash"
  (let (start end)
    (let ((*key-hook* (lambda (key s e) (when (equal key "info") (setq start s end e)))))
      (let ((torrent (decode-file filename)))
        (cons torrent (sha1 (read-chunk filename start end)))))))

;; Internal structures for torrent bookkeeping

(defparameter *block-length* (expt 2 14))

(defstruct (file (:constructor make-file (size path)))
  size ; length in bytes
  path ; relative pathname
  fd   ; file descriptor
)

(defstruct tr
  ;; .torrent content
  announce     ; tracker url
  name         ; unique file or top directory name
  hash         ; torrent hash
  length       ; torrent size
  files        ; list of files in torrent
  no-pieces    ; number of pieces
  piece-length ; piece length in bytes
  pieces       ; piece hashes

  ;; bookkeeping and communication
  piece-mask   ; bitmask of verified pieces
  block-mask   ; bitmask of downloaded blocks
  queue        ; queue for saving received blocks
  cache        ; cache of unverified pieces
)

(defun decode-names (info name)
  "Extract pathnames and their sizes from torrent/info"
  (let ((files (assoc "files" info :test #'equal)))
    (if files
        (mapcar
         (lambda (file)
           (make-file (getvalue "length" file)
                      (string-join
                       (cons name (mapcar #'decode-string (getvalue "path" file))) "/")))
         (cdr files))
        (list (make-file (getvalue "length" info) name)))))

(define-condition bad-no-pieces (error) ())

(defun new-tr (torrent)
  "Initialize torrent structure"
  (destructuring-bind (torrent . hash) torrent
    (with-structure tr
        ((info (getvalue "info" torrent))
         (pieces (getvalue "pieces" info))
         (:announce (decode-string (first-or-self (getvalue "announce" torrent))))
         (:name (decode-string (getvalue "name" info)))
         (:hash hash)
         (:piece-length (getvalue "piece length" info))
         (:pieces (split-sequence pieces 20))
         (:files (decode-names info name))
         (:length (reduce #'+ (mapcar #'file-size files)))
         (:no-pieces (ceiling length piece-length))
         (:piece-mask (make-array no-pieces :element-type 'bit))
         (piece-size (ceiling piece-length *block-length*))
         (piece-tail-size (ceiling (- length (* piece-length (1- no-pieces))) *block-length*))
         (:block-mask
          (with-vector (mask no-pieces)
            (dotimes (i (1- no-pieces))
              (setf (svref mask i) (make-array piece-size :element-type 'bit)))
            (setf (svref mask (1- no-pieces)) (make-array piece-tail-size :element-type 'bit))))
         (:cache (make-array no-pieces :initial-element nil)))
      (if (not (eql no-pieces (length pieces)))
          (error 'bad-no-pieces)))))

;; Various utility functions

(defun chunk-size (id len1 len2)
  (multiple-value-bind (quot rem) (floor len1 len2)
    (if (< id quot) len2 rem)))

(defun piece-length (pid torrent)
  "Return piece length (in bytes)"
  (chunk-size pid (tr-length torrent) (tr-piece-length torrent)))

(defun block-length (pid bid torrent)
  "Return block length (in bytes)"
  (chunk-size bid (piece-length pid torrent) *block-length*))

(defun piece-size (pid torrent)
  "Return piece size (in blocks)"
  (ceiling (piece-length pid torrent) *block-length*))

(defun format-hash (torrent)
  "Return torrent hash in hexademical format"
  (format nil "~{~x~^:~}" (coerce (tr-hash torrent) 'list)))
