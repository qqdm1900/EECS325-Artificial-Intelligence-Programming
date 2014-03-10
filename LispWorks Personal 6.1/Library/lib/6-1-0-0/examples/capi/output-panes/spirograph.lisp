;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/output-panes:spirograph.lisp,v 1.3.12.2 2011/11/04 20:49:43 davef Exp $" -*-

;;----------------------------------------------------------------------------
;;
;; examples/capi/output-panes/spirograph.lisp
;;
;; This example demonstrates simple input model of an output pane
;; To try it, compile and load this file and then execute:
;;
;;      (CL-USER::TEST-SPIROGRAPH)
;;
;; Press the left button and drag. It leaves behind small discs
;; marking points that it collects. When you release, it connects the
;; points.

;; It does not collect a point until *collecting-points-time-gap* of
;; internal-time-units-per-second (milliseconds) passed from the
;; previous point. The default is 200, which is quite sluggish.

;; When connecting, it connects each point to
;; *connecting-points-count* points backwards and forwards. The
;; default is 2, which gives mildly complex drawing.

;;----------------------------------------------------------------------------
;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
;;----------------------------------------------------------------------------

(in-package "CL-USER")


(defparameter *collecting-points-time-gap* 200) ; in internal-time-units-per-second

(defparameter *connecting-points-count* 3) 

;; Create a circular list of colors
(defvar *colors* (let ((a '(:red :green :blue :yellow :black :white)))
                   (setf (cdr (last a)) a)
                   a))
;; Avoid collecting too many points. 




;;; Input model functions.

;;; Press - go to next color, get rid of previous points, setup the time
;;; and collect one point. 

(defun start-collecting-points (pane x y)
  (with-slots (colors) 
      (capi:element-interface pane)
    (setf colors (rest colors)))
  (capi:remove-capi-object-property pane :points)
  (setf (capi:capi-object-property pane :next-collect-time) 0)
  (collect-point pane x y))
  
;;; Move - if the mouse actually moved and enough time passed, add the
;;; point, set the time for the next one, and draw a small filled circle. 
(defun collect-point (pane x y)
  (let ((time (get-internal-real-time))
        (point (cons x y)))
    (when (and (> time (capi:capi-object-property pane :next-collect-time))
               (not  (equal point (car (capi:capi-object-property pane :points)))))
        (push point (capi:capi-object-property pane :points))
        (setf (capi:capi-object-property pane :next-collect-time) (+ time *collecting-points-time-gap*))
        (gp:draw-circle pane x y
                           3 :filled t
                           :foreground  (with-slots (colors) 
                                            (capi:element-interface pane)
                                          (car colors))))))

;;; Release - collect last point, remember the number of points, 
;;; make the points list circular and redraw. 

(defun finish-with-points (pane x y)
  (collect-point pane x y)
  (let ((points (capi:capi-object-property pane :points)))
      (let ((length (length points))) ; count before closing the circle
        (setf (cdr (last points)) points)
        (setf (capi:capi-object-property pane :length) length)))
  (gp:invalidate-rectangle pane))

;;; The display-callback. Don't bother to limit to the area given for
;;; simplicity. Draw all the lines and a string indicating the next
;;; color.

(defun test-spirograph-draw-points (pane x y width height)
  (declare (ignore x y width height))
  (with-slots (colors) 
      (capi:element-interface pane)
    (when-let (length (capi:capi-object-property pane :length)) ; don't try until finish-with-points called
      (let ((points (capi:capi-object-property pane :points))
            (color (car colors)))
        (loop for (point . rest) on points
              for count below length
              for (from-x . from-y) = point
              do 
              (loop for (to-x . to-y) in rest
                    for count below *connecting-points-count*
                    do (gp:draw-line pane from-x from-y
                                     to-x to-y
                                     :foreground color)))))
    (let ((string (format nil "Next color: ~a" (string-capitalize (car (cdr colors))))))
      (gp:draw-string pane string  30 30 :block t))))

(capi:define-interface spirograph () 
  ((colors :initform *colors*))
  (:panes 
   (output capi:output-pane
           :display-callback 'test-spirograph-draw-points
           :input-model '(((:button-1 :press) start-collecting-points)
                          ((:button-1 :motion) collect-point)
                          ((:button-1 :release) finish-with-points)))))

(defun test-spirograph ()
  (capi:display
   (make-instance 'spirograph :best-width 500 :best-height 500)))


