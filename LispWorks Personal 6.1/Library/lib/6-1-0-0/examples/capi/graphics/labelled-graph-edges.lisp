;; -*- rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/graphics:labelled-graph-edges.lisp,v 1.6.10.1 2011/08/24 13:26:20 davef Exp $" -*-

;;----------------------------------------------------------------------------
;;
;; labelled-graph-edges.lisp
;;
;; This example demonstrates the ability label the graph edges
;;
;;----------------------------------------------------------------------------
;; To run this example, compile and load this file and then execute:
;;
;;    (cl-user::test-labelled-graph-edges)
;;
;; Other examples are
;;     (cl-user::test-labelled-graph-edges :left-right)
;;     (cl-user::test-labelled-graph-edges :top-down)
;;
;;----------------------------------------------------------------------------
;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
;;----------------------------------------------------------------------------


(in-package "CL-USER")

(defun test-labelled-graph-edges (&optional (direction :left-right))
  (capi:contain 
   (make-instance 'capi:graph-pane
                  :roots '(1)
                  :layout-function direction
                  :children-function 
                  #'(lambda(x) 
                      (when (< x 10)
                        (list (* x 2) (1+ (* x 2)))))
                  :edge-pane-function 
                  #'(lambda(self from to)
                      (declare (ignore self))
                      (let ((use-arrow (zerop (mod from 2)))
                            (use-label (zerop (mod to 2))))
                        (apply
                         'make-instance 
                         (if use-arrow
                             (if use-label
                                 'capi:labelled-arrow-pinboard-object
                               'capi:arrow-pinboard-object)
                           (if use-label
                               'capi:labelled-line-pinboard-object
                             'capi:line-pinboard-object))
                         (when use-label
                           (list :data (format nil "From ~R to ~R" from to))))))
                  :interaction :extended-selection)
   :best-width 700
   :best-height 700))

