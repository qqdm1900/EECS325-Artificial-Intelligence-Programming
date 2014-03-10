;; -*- rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/graphics:circled-graph-nodes.lisp,v 1.9.8.1 2011/08/24 13:26:19 davef Exp $" -*-

;;----------------------------------------------------------------------------
;;
;; circled-graph-nodes.lisp
;;
;; This example demonstrates the ability to change the display
;; characteristics of the nodes of a graph-pane.
;;
;;----------------------------------------------------------------------------
;; To run this example, compile and load this file and then execute:
;;
;;    (cl-user::test-circled-graph-nodes)
;;
;; You can also use ellipses instead of circles by specifying:
;;
;;    (cl-user::test-circled-graph-nodes :circlep nil)
;;----------------------------------------------------------------------------
;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
;;----------------------------------------------------------------------------


(in-package "CL-USER")


;;----------------------------------------------------------------------------
;; text-in-ellipse
;;
;; This object draws an ellipse with some text in the middle. It inherits
;; its behaviour from item-pinboard-object, but overrides the display
;; methods:
;;
;;      capi:draw-pinboard-object
;;      capi:draw-pinboard-object-highlighted
;;      capi:draw-pinboard-object-unhighlighted
;;
;; It also redefines the minimum size of the object by defining a method
;; on the function:
;;
;;      capi:calculate-constraints
;;----------------------------------------------------------------------------

(defclass text-in-ellipse (capi:item-pinboard-object)
  ((indent  :initform 5 :accessor text-in-ellipse-indent  :initarg :indent)
   (number :initarg :number :initform 0)
   (circlep :initform t :accessor text-in-ellipse-circlep :initarg :circlep))
  (:default-initargs 
   ;; Override the defaults from capi:item-pinboard-object
   ;; so capi:calculate-constraints can set the min size.
   :visible-min-width :no-hint
   :visible-min-height :no-hint))

(defparameter *colour-mapping*
  #(:red :blue  :green :purple1  :black :lightblue  :pink :orangered :orange :yellowgreen :magenta :cyan :navy :peachpuff))

(defmethod capi:calculate-constraints ((self text-in-ellipse))
  (let ((double-indent (* (text-in-ellipse-indent self) 2))
        (circlep (text-in-ellipse-circlep self)))
    (multiple-value-bind
        (left top right bottom)
        (gp:get-string-extent (capi:pinboard-object-pinboard self)
                              (capi:item-text self)
                              (capi:simple-pane-font 
                               (capi:pinboard-object-pinboard self)))
      (let* ((x-diameter (+ double-indent (- right left)))
             (y-diameter (+ double-indent (- bottom top)))
             (max-diameter (max x-diameter y-diameter)))
        (capi:with-geometry self
          (setf capi:%min-width%  (if circlep max-diameter x-diameter)
                capi:%min-height% (if circlep max-diameter y-diameter)))))))

(defun draw-text-in-ellipse (pinboard self &key foreground background)
  (capi:with-geometry self
    (let* ((half-width  (floor (1- capi:%width%)  2))
           (half-height (floor (1- capi:%height%) 2))
           (circle-x (+ capi:%x% half-width))
           (circle-y (+ capi:%y% half-height))
           (foreground (or foreground
                           (capi:simple-pane-foreground
                            (capi:pinboard-object-pinboard self))))
           (font (capi:simple-pane-font
                  (capi:pinboard-object-pinboard self)))
           (ascent (gp:get-font-ascent pinboard font))
           (text (capi:item-text self)))
      (multiple-value-bind
          (left top right bottom)
          (gp:get-string-extent (capi:pinboard-object-pinboard self)
                                text
                                font)
        (when background
          (gp:draw-ellipse pinboard
                           circle-x circle-y
                           half-width half-height
                           :filled t
                           :foreground background))
        (gp:draw-ellipse pinboard
                         circle-x circle-y
                         half-width half-height
                         :foreground foreground)
        (gp:draw-string pinboard
                        text
                        (+ capi:%x% (floor (- capi:%width% (- right left)) 2))
                        (+ capi:%y% (floor (- capi:%height% (- bottom top)) 2)
                           ascent)
                        :foreground foreground
                        :font font)))))

(defmethod capi:draw-pinboard-object (pinboard (self text-in-ellipse)
                                               &key &allow-other-keys)
  (draw-text-in-ellipse pinboard self))

(defmethod capi:draw-pinboard-object-highlighted ((pinboard capi:pinboard-layout)
                                                  (self text-in-ellipse)
                                                  &key &allow-other-keys)
  (draw-text-in-ellipse pinboard self
                        :background (svref *colour-mapping*
                                           (mod (slot-value self 'number) (length *colour-mapping* )))
                        :foreground (capi:simple-pane-background pinboard)))

(defmethod capi:draw-pinboard-object-unhighlighted ((pinboard capi:pinboard-layout)
                                                    (self text-in-ellipse)
                                                    &key &allow-other-keys)
  (draw-text-in-ellipse pinboard self
                        :background (capi:simple-pane-background pinboard)))


;;----------------------------------------------------------------------------
;; test-new-graph-nodes
;;
;; An example use of this graph-pane
;;----------------------------------------------------------------------------

(defun make-node (graph-pane node circlep)
  (make-instance 'text-in-ellipse
                 :circlep circlep
                 :number node
                 :text (capi:print-collection-item node graph-pane)))
  
(defun test-circled-graph-nodes (&key (circlep t))
  (capi:contain
   (make-instance 'capi:graph-pane
                  :roots '(1)
                  :children-function #'(lambda (x)
                                         (when (< x 8)
                                           (list (* x 2) (1+ (* x 2)))))
                  :print-function #'(lambda (x)
                                      (format nil "node:~D" x))
                  :highlight-style :default ;;; override the graph-pane value
                  :node-pane-function #'(lambda (graph-pane node)
                                          (make-node graph-pane node circlep)))
   :best-width 500
   :best-height 500))
