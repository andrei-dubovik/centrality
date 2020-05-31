;; Copyright (c) 2020 Andrey Dubovik <andrei@dubovik.eu>

;; A storage thread that accepts downloaded blocks, verifies pieces, and saves them to disk

(in-package :centrality)

;; TODO maybe make global torrent?

(defun escape-pathname (pathname &optional (special "[]"))
  "Escape certain special characters in a pathname"
  (with-output-to-string (out)
    (loop for char across pathname do
         (if (find char special)
             (write-char #\\ out))
         (write-char char out))))

(defun file-dir (file)
  "Return absolute file path"
  (escape-pathname (concatenate 'string *file-dir* (file-path file))))

(defun ensure-open-file (file torrent)
  "Open a file if closed, allocate and open if does not exist"
  (symbol-macrolet ((fd (file-fd file)))
    (if (not fd)
        (let ((pathname (file-dir file)))
          (if (probe-file pathname)
              (progn
                (setf fd (open pathname :direction :output :if-exists :overwrite :element-type '(unsigned-byte 8)))
                (log-msg 2 :event :file-open :torrent (format-hash torrent) :path pathname))
              (progn
                (ensure-directories-exist pathname)
                (setf fd (open pathname :direction :output :element-type '(unsigned-byte 8)))
                (file-position fd (1- (file-size file)))
                (write-byte 0 fd)
                (finish-output fd)
                (log-msg 2 :event :file-create :torrent (format-hash torrent) :path pathname)))))))

(defun open-files (torrent &rest rest)
  "Open all existing files"
  (dolist (file (tr-files torrent))
    (let ((pathname (file-dir file)))
      (symbol-macrolet ((fd (file-fd file)))
        (when (probe-file pathname)
          (setf fd (apply #'open pathname :element-type '(unsigned-byte 8) rest))
          (log-msg 2 :event :file-open :torrent (format-hash torrent) :path pathname))))))

(defun close-files (torrent)
  "Close all open file descriptors"
  (dolist (file (tr-files torrent))
    (symbol-macrolet ((fd (file-fd file)))
      (when fd
        (close fd)
        (setf fd nil)
        (log-msg 2 :event :file-close :torrent (format-hash torrent) :path (file-path file))))))

(defun split-segment (callback start end files)
  "Assists in writing one piece into multiple files"
  (let ((i 0) j)
    (dolist (file files)
      (setq j (+ i (file-size file)))
      (let ((ii (max i start))
            (jj (min j end)))
        (if (> jj ii)
            (funcall callback (- ii start) (- jj start) file (- ii i))))
      (if (>= (setq i j) end) (return)))))

(defun save-piece (pid piece torrent)
  "Save verified piece to file (files)"
  (let* ((start (* (tr-piece-length torrent) pid))
         (end (+ start (piece-length pid torrent))))
    (split-segment
     (lambda (start end file offset)
       (ensure-open-file file torrent)
       (file-position (file-fd file) offset)
       (write-bytes (subseq piece start end) (file-fd file)))
     start end (tr-files torrent))))

(defun verify-piece (pid piece torrent)
  "Verify downloaded piece"
  (equalp (sha1 piece) (svref (tr-pieces torrent) pid)))

(defun process-block (block torrent)
  "Cache, verify checksum and save if piece is complete"
  (destructuring-bind (pid offset block) block
    (if (eql (sbit (tr-piece-mask torrent) pid) 1) (return-from process-block)) ; already verified
    (let ((bid (/ offset *block-length*))
          (mask (svref (tr-block-mask torrent) pid))
          (cache (tr-cache torrent)))
      (symbol-macrolet ((piece (svref cache pid)))
        (if (not piece) (setf piece (zero-bytes (piece-length pid torrent))))
        (setf (subseq piece offset) block)
        (setf (sbit mask bid) 1)
        (when (bit-onep mask)
          (if (verify-piece pid piece torrent)
              (progn
                (setf (sbit (tr-piece-mask torrent) pid) 1)
                (save-piece pid piece torrent)
                (setf piece nil) ; free memory (at lisp boundary, manual memory management is required)
                (log-msg 3 :event :check :torrent (format-hash torrent) :pid pid))
              (progn
                (bit-clear mask)
                (log-msg 3 :event :fail :torrent (format-hash torrent) :pid pid))))))))

(defun iterate-pieces (callback torrent)
  "Iterate over pieces in open files"
  (dotimes (pid (tr-no-pieces torrent))
    (let* ((start (* pid (tr-piece-length torrent)))
           (len (piece-length pid torrent))
           piece)
      (split-segment
       (lambda (start end file offset)
         (let ((fd (file-fd file)))
           (when fd
             (if (not piece) (setq piece (zero-bytes len)))
             (file-position fd offset)
             (read-sequence piece fd :start start :end end))))
       start (+ start len) (tr-files torrent))
      (if piece
          (funcall callback pid piece)))))

(defun load-files (torrent)
  "Read files and check what pieces have already been downloaded"
  (unwind-protect
       (let ((mask (tr-piece-mask torrent)))
         (open-files torrent :direction :input)
         (iterate-pieces
          (lambda (pid piece)
            (when (verify-piece pid piece torrent)
              (setf (sbit mask pid) 1)
              (bit-set (svref (tr-block-mask torrent) pid))))
          torrent)
         (log-msg 1 :event :load-files :torrent (format-hash torrent) :downloaded (cons (count 1 mask) (length mask))))
    (close-files torrent)))

;; Event loop

(defun storage-loop (torrent)
  "Wait on new blocks and process them"
  (log-msg 1 :event :start :torrent (format-hash torrent) :name (tr-name torrent)) ; belongs in a control thread?
  (load-files torrent)
  (unwind-protect
       (while (block = (recv-msg (tr-queue torrent)))
         (process-block block torrent)
         (when (bit-onep (tr-piece-mask torrent))
           (log-msg 1 :event :finish :torrent (format-hash torrent) :name (tr-name torrent)) ; belongs in a control thread?
           (format t "Download complete~%")
           (return-from storage-loop)))
    (close-files torrent)))

(defun open-storage (torrent)
  "Open a new storage that can accept incoming blocks"
  (setf (tr-queue torrent) (make-mailbox))
  (make-thread (lambda () (storage-loop torrent)) :name "centrality-storage"))
