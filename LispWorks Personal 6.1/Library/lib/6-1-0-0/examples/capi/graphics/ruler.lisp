;; -*- rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/graphics:ruler.lisp,v 1.4.1.1 2011/08/24 13:26:20 davef Exp $" -*-

;;----------------------------------------------------------------------------
;;
;; examples/capi/graphics/ruler.lisp
;;
;; This is a simple ruler implemented using drawn-pinboard-objects
;;
;; To try it out, compile and load this file, and then call:
;;
;;      (CAPI:CONTAIN (MAKE-INSTANCE 'CL-USER:RULER))
;;
;;----------------------------------------------------------------------------
;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
;;----------------------------------------------------------------------------


(in-package "CL-USER")


(export '(ruler))


;;----------------------------------------------------------------------------
;; The class ruler
;;----------------------------------------------------------------------------

(defclass ruler (capi:drawn-pinboard-object)
  ((gradations :initform 10 :initarg :gradations :accessor ruler-gradations)
   (start :initform 0 :initarg :start :accessor ruler-start)
   (length  :initform 20 :initarg :length :accessor ruler-length))
  (:default-initargs
   :display-callback 'draw-ruler
   :max-height t))

(defmethod capi:calculate-constraints ((self ruler))
  (capi:with-geometry self
    (with-slots (gradations) self
      (let ((output-pane (capi:pinboard-object-pinboard self)))
        (setf capi:%min-width% (1+ (* 10 gradations))
              capi:%min-height% (+ (gp:get-font-ascent output-pane) 10))))))


;;----------------------------------------------------------------------------
;; Draw the exposed area of the ruler
;;----------------------------------------------------------------------------

(defun draw-ruler (output-pane self x y width height)
  (declare (ignore y width height))
  (capi:with-geometry self
    (with-slots (gradations start) self
      (let* ((length (floor (1- capi:%width%) gradations))
             (font-ascent (gp:get-font-ascent output-pane))
             (start-pos (floor (- x capi:%x%) gradations))
             (gap 2)
             (height (floor (- capi:%height% font-ascent gap) 2))
             (height-for-5  (floor (* height 3) 2))
             (height-for-10 (* height 2))
             (start-x (+ capi:%x% (* start-pos gradations)))
             )
        (gp:draw-line output-pane
                      start-x capi:%y%
                      (+ capi:%x% (* gradations length)) capi:%y%)
        (loop for count from start-pos to length
              for value = (+ count start)
              for x from start-x by gradations
              for multiple-of-5  = (= (mod value 5)  0)
              for multiple-of-10 = (= (mod value 10) 0)
              do (gp:draw-line output-pane
                               x capi:%y%
                               x (+ capi:%y% (cond (multiple-of-10 height-for-10)
						   (multiple-of-5  height-for-5)
						   (t              height))))
              when (= (mod value 10) 0)
              do (gp:draw-adjusted-string
		  output-pane
		  (format nil "~D" value)
		  x (+ capi:%y% gap height-for-10 font-ascent)
		  (cond
		   ((eq count 0)      :left)
                   ((eq count length) :right)
                   (t                 :centre))
                  ))))))



;;----------------------------------------------------------------------------
;; Handle setf of the various ruler slots
;;----------------------------------------------------------------------------

(defmethod (setf ruler-gradations) :after ((new-value t) (self ruler))
  (unless (capi:invalidate-pane-constraints self)
    (capi:redraw-pinboard-object self)))

(defmethod (setf ruler-start) :after ((start t) (self ruler))
  (capi:redraw-pinboard-object self))

(defmethod (setf ruler-length) :after ((length t) (self ruler))
  (unless (capi:invalidate-pane-constraints self)
    (capi:redraw-pinboard-object self)))
