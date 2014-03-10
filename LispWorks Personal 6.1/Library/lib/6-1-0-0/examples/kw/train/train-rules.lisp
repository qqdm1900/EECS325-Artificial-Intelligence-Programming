;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/58/KWdemos/RCS/train:train-rules.lisp,v 1.1.13.1 2011/08/24 13:25:20 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

;;; type (infer :contexts '(trains)) to run

(in-package kw-user)

;;; ---------------------------- OPTIMISED OBJECTS --------------------------

(def-kb-struct train position)
(def-kb-struct signal position colour)

;;; ------------------------------- CONTEXTS ----------------------------

(defcontext trains :strategy (priority recency order)
            :auto-return t)

;;; ----------------------------- FORWARD CHAINING ---------------------

;;; if the train is at the end we've finished

(defrule train-end :forward
  :context trains
  :priority 1
  (train ?t position 10)
  -->
  ((format t "Train finished~%")))

;;; if there's a red signal without a train there, change the signal to green

(defrule change-signal :forward
  :context trains
  :priority 7
  (signal ?s position ?sig-pos colour red)
  (not (train ?t position ?sig-pos))
  -->
  ((format t "Signal ~a changing to green~%" ?sig-pos))
  (assert (signal ?s colour green)))

;;; if there's a green signal in front of the train, move it forward

(defrule move-train :forward
  :context trains
  :priority 4
  (signal ?s position ?sig-pos colour green)
  (train ?t position ?train-pos)
  (test (= ?sig-pos (1+ ?train-pos)))
  -->
  ((format t "Train moving to ~a~%" (1+ ?train-pos)))
  (assert (signal ?s colour red))
  ((1+ ?train-pos) ?new-pos)
  (assert (train ?t position ?new-pos)))
