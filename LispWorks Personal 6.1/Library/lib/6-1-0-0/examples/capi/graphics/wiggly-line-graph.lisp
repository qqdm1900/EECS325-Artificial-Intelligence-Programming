;; -*- rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/graphics:wiggly-line-graph.lisp,v 1.1.9.1 2011/08/24 13:26:20 davef Exp $" -*-

;;----------------------------------------------------------------------------
;;
;; wiggly-line-graph.lisp
;;
;; This example demonstrates the ability draw custom line types in a graph.
;;
;;----------------------------------------------------------------------------
;; To run this example, compile and load this file and then execute:
;;
;;    (test-wiggly-line-graph)
;;
;;----------------------------------------------------------------------------
;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
;;----------------------------------------------------------------------------


(in-package "CL-USER")

(defclass wiggly-line (capi:line-pinboard-object)
  ())

(defmethod capi:draw-pinboard-object (pinboard (object wiggly-line)
                                               &key x y width height
                                               &allow-other-keys)
  (declare (ignore x y width height)) ; always draw it all
  (destructuring-bind
      (start-x start-y end-x end-y)
      (capi::coords-or-geometry object)
    (let* ((dx (- end-x start-x))
           (dy (- end-y start-y))
           (length (sqrt (+ (* dx dx) (* dy dy))))
           (wiggly-x (* (/ dy length) 4))
           (wiggly-y (* (/ dx length) -4))
           (last-x start-x)
           (last-y start-y)
           (sign -1)
           ;; aim for 10 pixels apart, but have at least 4 wiggles
           (wiggles (max (/ length 10) 4)))
      (dotimes (wiggle (floor wiggles))
        (let* ((wiggle-distance (/ (1+ wiggle) wiggles))
               (next-x (+ start-x
                          (* dx wiggle-distance)
                          (* sign wiggly-x)))
               (next-y (+ start-y
                          (* dy wiggle-distance)
                          (* sign wiggly-y))))
          (gp:draw-line pinboard last-x last-y next-x next-y)
          (setq last-x next-x
                last-y next-y
                sign (- sign)))))))

(defun test-wiggly-line-graph ()
  (capi:contain 
   (make-instance 'capi:graph-pane
                  :roots '(1)
                  :layout-function :left-right
                  :children-function 
                  #'(lambda(x) 
                      (when (< x 10)
                        (list (* x 2) (1+ (* x 2)))))
                  :edge-pane-function 
                  #'(lambda(self from to)
                      (declare (ignore self from to))
                      (make-instance 
                       'wiggly-line))
                  :interaction :extended-selection)
   :best-width 700
   :best-height 700))


