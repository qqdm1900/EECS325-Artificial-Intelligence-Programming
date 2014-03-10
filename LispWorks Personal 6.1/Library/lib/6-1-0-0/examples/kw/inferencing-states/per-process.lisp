;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/58/KWdemos/RCS/inferencing-states:per-process.lisp,v 1.1.9.1 2011/08/24 13:25:20 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

;;; Example of forward chaining simultaneously in multiple threads.

;;; To run a single counter: (kw-user::test-1-counter 'foo).
;;; To run 10 counters simultaneously: (kw-user::test-counters 10).

(in-package kw-user)

(defvar *result*)

(def-kb-class counter ()
  ((value :initarg :value)
   (step :initarg :step)
   (data :initform nil)))

(defrule count-down :forward
  (counter ?counter value ?value step ?step data ?data)
  (test (> ?value 0))
  -->
  ((- ?value ?step) ?next-value)
  (erase ?counter)
  (assert (counter ? value ?next-value step ?step data (?value . ?data))))

(defrule print-results :forward
  (counter ?counter value ?value data ?data)
  (test (= ?value 0))
  -->
  ((setq *result* ?data)))

(defun test-1-counter (name &optional result)
  (let* ((*result* nil)
         (*inferencing-state* nil)
         (step (1+ (random 10)))
         (limit (* step (+ 2000 (random 100)))))
    (unwind-protect
        (progn
          (setq *inferencing-state* (make-inferencing-state name))
          (make-instance 'counter :value limit :step step)
          (infer))

      (destroy-inferencing-state *inferencing-state*))
    (let ((expected-result (loop for value from step to limit by step
                                 collect value)))
      (assert (equal *result* expected-result))
      (when result
        (setf (car result) limit)))))

(defun test-counters (count)
  (let ((results (make-list count)))
    (dotimes (index count)
      (mp:process-run-function (format nil "Test ~D" index)
                               '()
                               'test-1-counter
                               (gensym)
                               (nthcdr index results)))
    (loop (let ((old-results (copy-list results)))
            (mp:process-wait "Waiting for results"
                             #'(lambda ()
                                 (not (equal old-results results))))
            (unless (member nil results)
              (return))))
    (dotimes (index count)
      (format t "~%Process ~D counted to ~D."
              index
              (nth index results)))))

