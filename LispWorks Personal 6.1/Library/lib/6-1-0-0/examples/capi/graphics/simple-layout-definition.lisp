;; -*- rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/graphics:simple-layout-definition.lisp,v 1.5.11.1 2011/08/24 13:26:22 davef Exp $" -*-

;;----------------------------------------------------------------------------
;;
;; examples/capi/graphics/simple-layout-definition.lisp
;;
;; This example demonstrates the ability label the graph edges
;;
;;----------------------------------------------------------------------------
;; To run this example, compile and load this file and then execute:
;;
;;    (cl-user::test-simple-layout)
;;
;;----------------------------------------------------------------------------
;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
;;----------------------------------------------------------------------------


(in-package "CL-USER")

(defun demo-layout (self &key force)
  (declare (ignore force))
  (let ((nodes (capi:graph-pane-nodes self)))
    (loop for node in nodes
          for max-node = 10
          for pos from 0
          for rad = (* (/ pos max-node) 2 PI)
          for width from 100 by 10
          for X = (floor (+ (* width (cos rad)) 250))
          for Y = (floor (+ (* width (sin (- rad))) 250))
          do (setf (capi:graph-node-x node) X
                   (capi:graph-node-y node) Y))))

(defun children (x) 
  (list* (mod (1+ x) 21)
         (when (<= x 10) (list (+ x 10)))))

(defun test-simple-layout ()
  (capi:contain
   (make-instance 'capi:graph-pane
                  :children-function 'children
                  :roots '(0)
                  :edge-pane-function 
                  #'(lambda(&rest args)
                      (declare (ignore args))
                      'capi:arrow-pinboard-object)
                  :layout-function 'demo-layout)
   :default-width 400
   :default-height 400))

