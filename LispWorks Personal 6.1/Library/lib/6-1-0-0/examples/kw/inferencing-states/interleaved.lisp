;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/58/KWdemos/RCS/inferencing-states:interleaved.lisp,v 1.1.9.1 2011/08/24 13:25:19 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

;;; Two examples of forward chaining with multiple inferencing states in
;;; a single thread.

;;; To run with a single context: (KW-USER::TEST-STEPPING-SINGLE-CONTEXT).
;;; To run with two contexts: (KW-USER::TEST-STEPPING-DUAL-CONTEXT).

(in-package kw-user)

(def-named-kb-class step-controller ()
  ((value :initform 0)))

(defcontext step-one-context)

(defrule step-one :forward
  :context step-one-context
  (step-controller ?sc value ?value)
  -->
  ((+ ?value 1) ?new-value)
  (assert (step-controller ?sc value ?new-value))
  ((format t "~&~S stepped to ~D.~%" (kb-name ?sc) ?new-value))
  (return))

(defcontext step-two-context)

(defrule step-two :forward
  :context step-two-context
  (step-controller ?sc value ?value)
  -->
  ((+ ?value 2) ?new-value)
  (assert (step-controller ?sc value ?new-value))
  ((format t "~&~S stepped to ~D.~%" (kb-name ?sc) ?new-value))
  (return))

(defun test-stepping-single-context ()
  (let ((state1 (make-inferencing-state 'state1))
        (state2 (make-inferencing-state 'state2)))
    (unwind-protect
        (progn
          (let ((*inferencing-state* state1))
            (make-instance 'step-controller :kb-name 'stepper-one-a))
          (let ((*inferencing-state* state2))
            (make-instance 'step-controller :kb-name 'stepper-one-b))
          (loop repeat 10
                do
                (let ((*inferencing-state* state1))
                  (infer :contexts '(step-one-context)))
                (let ((*inferencing-state* state2))
                  (infer :contexts '(step-one-context)))))
      (destroy-inferencing-state state1)
      (destroy-inferencing-state state2))))

(defun test-stepping-dual-context ()
  (let ((state1 (make-inferencing-state 'state1))
        (state2 (make-inferencing-state 'state2)))
    (unwind-protect
        (progn
          (let ((*inferencing-state* state1))
            (make-instance 'step-controller :kb-name 'stepper-one))
          (let ((*inferencing-state* state2))
            (make-instance 'step-controller :kb-name 'stepper-two))
          (loop repeat 10
                do
                (let ((*inferencing-state* state1))
                  (infer :contexts '(step-one-context)))
                (let ((*inferencing-state* state2))
                  (infer :contexts '(step-two-context)))))
      (destroy-inferencing-state state1)
      (destroy-inferencing-state state2))))

