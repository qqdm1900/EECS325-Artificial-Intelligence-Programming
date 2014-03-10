;; -*- rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/graphics:pinboard-movement.lisp,v 1.10.1.1 2011/08/24 13:26:21 davef Exp $" -*-

;;;----------------------------------------------------------------------------
;;;
;;; pinboard-movement.lisp
;;;
;;; This example demonstrates dynamically adding and moving pinboard objects
;;; around a pinboard layout. Press the button to add a new object in a
;;; random position, and then use the left mouse button to drag
;;; the objects around. 
;;; When you finish moving an object, it randomly resize it by calling
;;; randomly-adjust from finish-pinboard-object-move.
;;;
;;;
;;;----------------------------------------------------------------------------
;;; To run this example, compile and load this file and then execute:
;;;
;;;    (CL-USER::TEST-PINBOARD-MOVEMENT)
;;;
;;;----------------------------------------------------------------------------
;;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
;;;----------------------------------------------------------------------------


(in-package "CL-USER")


;;----------------------------------------------------------------------------
;; Pinboard object handling
;;----------------------------------------------------------------------------

(defun make-new-pinboard-object ()
  (make-instance
   (nth (random 3) '(capi:ellipse capi:rectangle capi:line-pinboard-object))
   :visible-min-width 10
   :visible-min-height 10

   :graphics-args (list :thickness 3 ; for lines
                        :foreground
                        (let* ((red (random 1.0))
                               (green  (random 1.0))
                               (blue (random 1.0)))
                          (color:make-rgb red green blue)))
   :filled t))

(defun add-pinboard-object (pinboard)
  (let ((new-object (make-new-pinboard-object)))
    (capi:manipulate-pinboard pinboard new-object :add-top)
    (capi:with-geometry pinboard
      (setf (capi:pinboard-pane-position new-object) 
            (values (random (max (- capi:%width% 100) 10))
                    (random (max (-  capi:%height% 100) 10)))))
    (setf (capi:pinboard-pane-size new-object)
          (values (+ 20 (random 80)) (+ 20 (random 80))))))

(defstruct drag-status
  object
  x-offset
  y-offset)

(defvar *pinboard-drag-status* nil)

(defun start-pinboard-object-move (layout x y)
  (let ((object (capi:pinboard-object-at-position layout x y)))
    (setq *pinboard-drag-status*
          (when object
            (capi:with-geometry object
              (make-drag-status
               :object object
               :x-offset (- x capi:%x%)
               :y-offset (- y capi:%y%)))))))

(defun drag-pinboard-object (layout x y)
  (declare (ignore layout))
  (when *pinboard-drag-status*
    (let ((object (drag-status-object *pinboard-drag-status*)))
      (let ((new-x (- x (drag-status-x-offset *pinboard-drag-status*)))
            (new-y (- y (drag-status-y-offset *pinboard-drag-status*))))
        (setf (capi:pinboard-pane-position object) (values new-x new-y))))))

(defun randomly-adjust (x)
  (if (zerop (random 2))
      (if (> x 10) (floor x 2) x)
    (if (< x 100) (* x 2) x)))

(defun finish-pinboard-object-move (layout x y)
  (declare (ignore layout x y))
  (when *pinboard-drag-status*
    (let ((object (drag-status-object *pinboard-drag-status*)))
      (multiple-value-bind (width height)
          (capi:pinboard-pane-size object)
        (setf (capi:pinboard-pane-size object) 
              (values (randomly-adjust width)
                      (randomly-adjust height))))))
  (setq *pinboard-drag-status* nil))
  
(defun clear-pinboard-objects (pinboard)
  (setf (capi:layout-description pinboard) nil))

;;----------------------------------------------------------------------------
;; test-pinboard-movement
;;----------------------------------------------------------------------------

(capi:define-interface pinboard-movement-test ()
  ()
  (:panes
   (add-button
    capi:push-button
    :text "Add New Object"
    :callback-type :none
    :callback #'(lambda ()
                  (add-pinboard-object pinboard)))
   (clear-button
    capi:push-button
    :text "Clear Pinboard"
    :callback-type :none
    :callback #'(lambda ()
                  (clear-pinboard-objects pinboard))))
  (:layouts
   (buttons
    capi:row-layout
    '(add-button clear-button))
   (pinboard
    capi:pinboard-layout
    nil
    :draw-with-buffer t
    :input-model '(((:button-1 :press)   start-pinboard-object-move)
                   ((:button-1 :motion)  drag-pinboard-object)
                   ((:button-1 :release) finish-pinboard-object-move)))
   (main-layout
    capi:column-layout
    '(pinboard buttons)
    :x-adjust :centre))
  (:default-initargs
   :title "Pinboard Movement Test"
   :layout 'main-layout
   :best-width 400
   :best-height 400))

(defun test-pinboard-movement ()
  (capi:display (make-instance 'pinboard-movement-test)))
