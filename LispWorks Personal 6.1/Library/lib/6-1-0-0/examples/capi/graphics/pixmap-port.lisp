;; -*- rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/graphics:pixmap-port.lisp,v 1.3.1.1 2011/08/24 13:26:22 davef Exp $" -*-

;;----------------------------------------------------------------------------
;;
;; examples/capi/graphics/pixmap-port.lisp
;;
;; This example demonstrates the uses of pixmap-port within a
;; pinboard-layouts in the CAPI.
;;
;; To try it, compile and load this file and then execute:
;;
;;      (CL-USER::TEST-REDRAWING-WITH-PIXMAP)
;;
;; It creates a pixmap which a circle of with random dots of radius
;; 50, and inside it a circle with radius 25 with semi-transparent red.
;; You can move the circle by dragging with the left button, which also
;; changes the background color as you move.

;;----------------------------------------------------------------------------
;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
;;----------------------------------------------------------------------------


(in-package "CL-USER")


;;----------------------------------------------------------------------------
;; Define an interface
;;----------------------------------------------------------------------------

(capi:define-interface redrawing-with-pixmap ()
  ((pixmap :initform nil))
  (:panes
   (pixmap-object capi:drawn-pinboard-object
                  :display-callback 'draw-interface-pixmap))
  (:layouts
   (pixmap-layout capi:pinboard-layout
                  '((pixmap-object :x 5 :y 5 :width 100 :height 100))
                  :draw-with-buffer t
                  :input-model 
                  '(((:motion :button-1) redrawing-with-pixmap-example-do-move)
                   )
                  :background (color:make-rgb 0.1 0.35 0.95)))
  (:default-initargs
   :initial-constraints '(:min-width 300 :min-height 300)
   :destroy-callback 'destroy-interface-pixmap))

;;; The display-callbak of the pixmap-object. called when 
;;; an area in the pane needs redisplay. 

(defun draw-interface-pixmap (pinboard object x y width height)
  ;; x,y,width & height are the coords & size in pinboard of the object to be drawn.
  (with-slots (pixmap) (capi:element-interface pinboard)
    (capi:with-geometry object
      (unless pixmap
        (setf pixmap (compute-interface-pixmap pinboard)))
      (gp:copy-pixels pinboard  pixmap
                 x y width height
                 (- x capi:%x%)
                 (- y capi:%y%)))))


;;----------------------------------------------------------------------------
;; Code to create and destroy the pixmap.
;; make the background transparent (alpha 0).
;; Put random dots in a circle of radius 50.
;; Draw a crcle of radius 25 with semi-transparent red. 
;;----------------------------------------------------------------------------

(defun compute-interface-pixmap (pinboard)
  (let ((pixmap (gp:create-pixmap-port pinboard 100 100 :clear t 
                                       :background (color:color-with-alpha :black 0))))
    (setf (gp:graphics-port-foreground pixmap) :black)
    (dotimes (y 100)
      (let ((x-distance (floor (sqrt (- 2500 (expt (- y 50) 2))))))
      (loop for x from (- 50 x-distance) upto (+ 50 x-distance)
            do 
            (when (zerop (random 3))
              (gp:draw-point pixmap x y)))))
    (gp:draw-circle pixmap 50 50 25 :foreground (color:color-with-alpha :red 0.3)
                    :filled t)
                    
    pixmap))

(defun destroy-interface-pixmap (interface)
  (with-slots (pixmap) interface
    (when pixmap
      (gp:destroy-pixmap-port pixmap))))

;;; The motion callback. 
(defun redrawing-with-pixmap-example-do-move (pane x y)
  (let* ((location-ratio (/ x (capi:with-geometry pane capi:%width%)))
         (yellow-factor (+ 0.3 (* 0.7 location-ratio)))
         (new-background (color:make-rgb (* yellow-factor yellow-factor) ; red
                                         yellow-factor ; green 
                                         (- 1 yellow-factor))) ;blue
        (pos-x (- x 50))
        (pos-y (- y 50))
        (pixmap-object (slot-value (capi:element-interface pane)
                                   'pixmap-object)))

    (capi:with-atomic-redisplay (pane)
      (setf (capi:simple-pane-background pane) new-background)
      (setf (capi:pinboard-pane-position pixmap-object)
            (values pos-x pos-y)))))
;;----------------------------------------------------------------------------
;; The test function
;;----------------------------------------------------------------------------

(defun test-redrawing-with-pixmap ()
  (capi:display (make-instance 'redrawing-with-pixmap
                               :best-width 100
                               :best-height 100)))

