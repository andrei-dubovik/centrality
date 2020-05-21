;; Copyright (c) 2020 Andrey Dubovik <andrei@dubovik.eu>

;; A storage thread that accepts downloaded blocks, verifies pieces, and saves them to disk

(in-package :centrality)

(defparameter *file-dir* "./")

;; TODO maybe make global torrent?

(defun escape-pathname (pathname &optional (special "[]"))
  "Escape certain special characters in a pathname"
  (with-output-to-string (out)
    (loop for char across pathname do
         (if (find char special)
             (write-char #\\ out))
         (write-char char out))))

;; TODO: reload files if exist
(defun ensure-open-file (file torrent)
  "Open a file if closed, allocate and open if does not exist"
  (symbol-macrolet ((fd (file-fd file)))
    (if (not fd)
        (let ((pathname (escape-pathname (concatenate 'string *file-dir* (file-path file)))))
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

(defun close-files (torrent)
  "Close all open file descriptors"
  (dolist (file (tr-files torrent))
    (when (streamp (file-fd file))
      (close (file-fd file))
      (log-msg 2 :event :file-close :torrent (format-hash torrent) :path (file-path file)))))

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

;; Event loop

(defun storage-loop (torrent)
  "Wait on new blocks and process them"
  (log-msg 1 :event :start :torrent (format-hash torrent) :name (tr-name torrent)) ; belongs in a control thread?
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
