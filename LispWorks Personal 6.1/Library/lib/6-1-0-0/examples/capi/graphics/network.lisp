;; -*- rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/graphics:network.lisp,v 1.6.11.1 2011/08/24 13:26:21 davef Exp $" -*-

;;----------------------------------------------------------------------------
;;
;; labelled-network-graph-edges.lisp
;;
;; This example demonstrates the ability label the graph edges of a network
;;
;;----------------------------------------------------------------------------
;; To run this example, compile and load this file and then execute:
;;
;;    (test-labelled-network-graph-edges 'child1)
;;    (test-labelled-network-graph-edges 'child2)
;;
;;----------------------------------------------------------------------------
;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
;;----------------------------------------------------------------------------


(in-package "CL-USER")

(defun child1(x)
  (remove x '(1 2 3 4 5 6 7 8 9)))

(defun child2(x)
  (let ((children (if (> x 9)
                      (remove x '(10 11 12 13 14 15 16 17 18))
                    (remove x '(1 2 3 4 5 6 7 8 9)))))
    (if (= x 9)
        (append '(10) children)
      children)))

(defun test-labelled-network-graph-edges (children-function)
  (capi:contain 
   (make-instance 'capi:simple-network-pane
                  :roots '(1)
                  :children-function children-function
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

                         (unless (and  (not use-label))
                           (list :data (format nil "~a=>~a" from to))))))
                  :interaction :extended-selection)
   :best-width 700
   :best-height 700))

