;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/23/LISPexamples/RCS/streams:buffered-stream.lisp,v 1.2.8.1 2011/08/24 13:26:18 davef Exp $" -*-

;;----------------------------------------------------------------------------
;;
;; examples/streams/buffered-stream.lisp 

;; This example demonstrates the use of STREAM:BUFFERED-STREAM. A pipe stream
;; is connected to another pipe stream via a storage-buffer.
;;
;; To try it, compile and load this file and then execute:
;;
;;      (CL-USER::TEST-LISP-PIPE-PAIR )
;;
;;----------------------------------------------------------------------------
;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
;;----------------------------------------------------------------------------

(in-package "CL-USER")

(defstruct storage-buffer
  (data "")
  (data-end 0)
  (eofp nil)
  (lock (mp:make-lock :name "Strorage Buffer Stream Lock")))

(defun add-to-storage-buffer (storage string start end)
  (let* ((len (- end start))
	 (storage-data (storage-buffer-data storage))
	 (storage-data-length (length storage-data))
         (new-data-end (+ (storage-buffer-data-end storage) len)))
    (when (> new-data-end storage-data-length)
      (mp:process-wait "Waiting for storage buffer to empty."
                       #'(lambda ()
	                   (<= (setq new-data-end
                                     (+ (storage-buffer-data-end storage) len))
                               storage-data-length))))
    (mp:with-lock
        ((storage-buffer-lock storage))
     (replace storage-data string
              :start1 (storage-buffer-data-end storage)
              :end1 new-data-end)
     (setf (storage-buffer-data-end storage) new-data-end))))

(defun remove-from-storage-buffer (storage string start end)
  (flet ((readyp
                 ()
           (or (/= (storage-buffer-data-end storage) 0)
               (storage-buffer-eofp storage))))
    
    (loop 
     (mp:with-lock
         ((storage-buffer-lock storage))
       (when (readyp)
         (return 
          (let ((data-end (storage-buffer-data-end storage))
                (data (storage-buffer-data storage)))
            (if (> data-end 0)
                (let ((used-len (min data-end (- end start))))
                  (replace string data
                           :start1 start
                           :end1 (+ start used-len))
                  (replace data data :start1 used-len)
                  (decf (storage-buffer-data-end storage) used-len)
                  used-len)
              0)))))
     (mp:process-wait "Waiting for storage buffer to fill." #'readyp))))

(defun storage-buffer-listen (storage)
  (/= (storage-buffer-data-end storage) 0))

(defun storage-buffer-element-type (storage)
  (array-element-type (storage-buffer-data storage)))


(defclass lisp-pipe-stream (stream:buffered-stream)
  ((input-storage :initarg :input-storage :initform nil)
   (output-storage :initarg :output-storage :initform nil)))

(defmethod stream:stream-read-buffer ((stream lisp-pipe-stream) buffer start end)
  (with-slots (input-storage) stream
    (remove-from-storage-buffer input-storage buffer start end)))

(defmethod stream:stream-write-buffer ((stream lisp-pipe-stream) buffer start end)
  (with-slots (output-storage) stream
    (add-to-storage-buffer output-storage buffer start end)))

(defmethod close ((stream lisp-pipe-stream) &key abort)
  (declare (ignore abort))
  (with-slots (output-storage) stream
    (when output-storage
      (setf (storage-buffer-eofp output-storage) t)))
  t)

(defmethod stream:stream-listen ((stream lisp-pipe-stream))
  (with-slots (input-storage) stream
    (storage-buffer-listen input-storage)))

(defmethod stream:stream-check-eof-no-hang ((stream lisp-pipe-stream))
  (with-slots (input-storage) stream
    (and (storage-buffer-eofp input-storage)
         :eof)))

(defmethod stream-element-type ((stream lisp-pipe-stream))
  (with-slots (input-storage output-storage) stream
    (storage-buffer-element-type (or input-storage output-storage))))

(defun make-lisp-pipe-pair (&key (element-type 'base-char) (size 8192) (direction :io))
  "Return two values, a pair of streams connected together.  The DIRECTION argument controls the direction of the first stream, the second stream having the opposite direction.  By default, both streams are bidirectional."
  (check-type direction (member :input :output :io))
  (let ((storage-1-to-2 (unless (eq direction :input)
                          (make-storage-buffer
	                   :data (make-string size :element-type element-type))))
        (storage-2-to-1 (unless (eq direction :output)
                          (make-storage-buffer
	                   :data (make-string size :element-type element-type)))))
    (values (make-instance 'lisp-pipe-stream
                           :direction direction
                           :input-storage storage-2-to-1
                           :output-storage storage-1-to-2
	                   :element-type element-type)
	    (make-instance 'lisp-pipe-stream
                           :direction (case direction
                                        (:input :output)
                                        (:output :input)
                                        (otherwise direction))
                           :input-storage storage-1-to-2
                           :output-storage storage-2-to-1
	                   :element-type element-type))))

(defun test-lisp-pipe-pair ()
  (multiple-value-bind (here there)
      (make-lisp-pipe-pair)
    (unwind-protect
        (flet ((upcase
                ()
                (loop for ch = (read-char there nil nil)
                      while ch
                      do
                      (write-char (char-upcase ch) there)
                      (force-output there))))
          (mp:process-run-function "there" '() #'upcase)
          (format here "This was a mixed-case string~%")
          (force-output here)
          (read-line here))
      (close here)
      (close there))))

