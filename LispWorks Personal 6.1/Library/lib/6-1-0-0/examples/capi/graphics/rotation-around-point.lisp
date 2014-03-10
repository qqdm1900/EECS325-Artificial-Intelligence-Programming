;; -*- rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/graphics:rotation-around-point.lisp,v 1.1.1.1 2011/08/24 13:26:21 davef Exp $" -*-

;;----------------------------------------------------------------------------
;;
;; examples/capi/graphics/rotation-around-point.lisp
;;
;; An example of using gp:apply-rotation-around-point. Compiler
;; To see the example, compile and load this file and then execute:
;;
;; (CL-USER::run-rotation-around-point-example)
;;
;; The pane displays a simple pattern (cross with small randomly colored
;; squares att he corners). When you left-click in the pane, it draws
;; a pink circle when you click, and then rotates the pattern around this
;; point, waits for two second, erases what it drew and show the pattern again. 
;;
;;----------------------------------------------------------------------------
;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
;;----------------------------------------------------------------------------

(in-package "CL-USER")

;;; Draws a "random" pattern (cross with colored small squares)
(defun rotation-around-point-draw-pattern (op)
  (let ((color (color:make-rgb (random 0.9) (random 0.9) (random 0.9))))
    (gp:draw-line op
                  85 85 165 165
                  :thickness 2 )
    (gp:draw-line op
                  85 165 165 85
                  :thickness 2 )
    (dolist (x '(80 160))
      (dolist (y '(80 160))
        (gp:draw-rectangle op x y 10 10 :filled t :foreground color)))))

;;; The drawing function for the rotation. During the rotation
;;; it is called each 50 millis by rotation-around-point-timer-function
;;; (via capi:apply-in-pane-process). It draws the pattern rotated
;;; using the tranform, and then apply rotation to the transform (for the next
;;; call). 

(defun rotation-around-point-draw-rotated-and-apply-rotation (op x y transform)
  (gp:with-graphics-transform (op transform)
    (rotation-around-point-draw-pattern op))
  (gp:APPLY-ROTATION-AROUND-POINT transform 0.1 x y))

;;; The timer function. Called each 50 milli seconds
;;; because rotation-around-point-left-click-callback uses it
;;; in the timer. 
;;; It got the count which it decrements. Until it reahces 40 it perfoms the 
;;; rotation using rotation-around-point-draw-rotated-and-apply-rotation.
;;; When it reahces 40, it draws the center again and don't do any drawing
;;; afterwards. That gives 2 seconds of pause. 
;;; When it reaches 0 it invalidates the pane, which erases the
;;; drawing. When it goes below 0, it returns :stop (which causes the timer to
;;; stop repeating). 

(defun rotation-around-point-timer-function (op transform x y count-cons)
  (let ((new-count (decf (car count-cons))))
    (cond ((> new-count 40) 
            (capi:apply-in-pane-process
             op
             'rotation-around-point-draw-rotated-and-apply-rotation
             op  x y transform))
          ((= new-count 40)
           (capi:apply-in-pane-process 
            op
            'gp:draw-circle op x y 15 :foreground :pink :filled t)) ; redraw the center again
          ((= new-count 0)
           (capi:apply-in-pane-process op 'gp:invalidate-rectangle op)
           )
          ((< new-count 0) 
           :stop))))

;;; The left-click callback, called because it is in
;;; the :input-model below. 
;;; If there is already a count-cons from the previous call,
;;; we set the car to -1, which causes the timer to do nothing and stop
;;; because rotation-around-point-timer-function retruns :stop. 
;;; Then create a new timer, clear the pane and draw the center,
;;; and schedule the timer. 
(defun rotation-around-point-left-click-callback (op x y)
  (let ((count-cons (list 240)))
    (when-let (old (capi:capi-object-property op 'count-cons))
      (setf (car old) -1))  ; causes any one that runs to dies
    (setf (capi:capi-object-property op 'count-cons) count-cons)
    (let ((timer  (mp:make-timer 'rotation-around-point-timer-function
                                 op (gp:make-transform) x y count-cons)))

      (gp:clear-rectangle op 0 0 1000 1000)
      (gp:draw-circle op x y 15 :foreground :pink :filled t) ;; center
      (mp:schedule-timer-relative-milliseconds timer 50 50)
      )))

  
;;; Create the window.

(defun run-rotation-around-point-example ()
  (capi:contain 
   (make-instance 'capi:output-pane
                  :display-callback
                  #'(lambda (op &rest rect)
                      (declare (ignore rect))
                      (rotation-around-point-draw-pattern op))
                  :input-model '(((:button-1 :press) rotation-around-point-left-click-callback))
                  :initial-constraints '(:visible-min-width 600 :visible-min-height 600)
                  )
   :title "Rotation around point example"))
