;; -*- rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/graphics:metafile-rotation.lisp,v 1.7.1.1 2011/08/24 13:26:21 davef Exp $" -*-

;;----------------------------------------------------------------------------
;;
;; examples/capi/graphics/metafile-rotation.lisp
;;
;; This example shows using graphics ports operations to manipulate
;; the drawing of a metafile.
;; To see the example, compile and load this file and then execute:
;;
;; (CL-USER::METAFILE-ROTATION-EXAMPLE)

;; It "animates", by rotating the lines and shrinking them. As long as
;; it animates, you can move it around by moving the mouse. When it
;; stops, you can restart it by clicking on the pane.  Note: on GTK+
;; it may be slow, because it involves a lot of communication with the
;; server.

;; The animation is done by createing a metafile in
;; METAFILE-ROTATION-EXAMPLE-CREATE-METAFILE, and then repeatedly
;; drawing it with rotation and scaling in
;; METAFILE-ROTATION-EXAMPLE-DISPLAY.  Each display call also modifies
;; the rotation and scaling. The display is invoked repeatedly by a
;; timer.

;;----------------------------------------------------------------------------
;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
;;----------------------------------------------------------------------------

(in-package "CL-USER")

(defconstant *metafile-rotation-example-size* 400)

;; This is used to translate such that the center is in the middle 
;; of the metafile, so we can rotate around the middle. 
(defconstant *metafile-rotation-example-center-translation* 
  (- (truncate *metafile-rotation-example-size* 2)))

;;; Generate the metafile with the lines. 

(defun metafile-rotation-example-create-metafile (pane)
  (let* ((half-step 20)
         (size *metafile-rotation-example-size*))
    (capi:with-internal-metafile
        (mf :pane pane :bounds (list 0 0 size size))
      (loop for num from 0 by (+ half-step half-step) below size
            do 
            (gp:draw-line mf 0 num size (- size num) :foreground :green)
            (gp:draw-line mf 0 (+ num half-step) size (- size num half-step) :foreground :yellow)
            (gp:draw-line mf  num 0 (- size num) size :foreground :yellow)
            (gp:draw-line mf (+  num half-step) 0 (- size num half-step) size :foreground :green)))))

;;; Do the actual display, and also change the scaling and rotation. 

(defun metafile-rotation-example-display (pane &rest x)
  (declare (ignore x))
  (when-let (stuff (capi:capi-object-property pane 'metafile-rotation-example-stuff))
    (destructuring-bind (metafile angle scale x y) stuff
      ;; change these for the next time. 
      (setf (second stuff) (+ angle 0.1)
            (third stuff) (* scale 0.98))
      (let* ((trans (gp:make-transform)))
        ;; Move the metafile so the center is on the origin. 
        (gp:apply-translation trans 
                              *metafile-rotation-example-center-translation* 
                              *metafile-rotation-example-center-translation*)
        ;; Rotate and scale
        (gp:apply-rotation trans angle)
        (gp:apply-scale trans scale scale)
        ;; Move to where we want it to be. 
        (gp:apply-translation trans x y)

        (gp:with-graphics-transform (pane trans)
          (capi:draw-metafile pane metafile 
                              0 0 
                              *metafile-rotation-example-size* 
                              *metafile-rotation-example-size*)))
      )))


;;; This is called by the timer. 
;;; If the size is small, it returns :STOP which causes the 
;;; timer scheduler to stop scheduling it. Otherwise
;;; it causes the pane to redisplay, by calling gp:invalidate-rectangle.
;;; Since we don't know on which process this is called, it uses
;;; capi:apply-in-pane-process to make it run on the right process. 

(defun metafile-rotation-example-animate (pane)
  (if (> (third (capi:capi-object-property pane 'metafile-rotation-example-stuff))
         0.2)
      (capi:apply-in-pane-process
       pane 
       'gp:invalidate-rectangle pane)
    :stop))


;;; This is the :MOTION callback. 
;;; Moves the metafile by changing the X and Y in "stuff", 
;;; and if the animation is already quite small, reset it to full size. 

(defun metafile-rotation-example-change-center (pane x y)
  (when-let (stuff (capi:capi-object-property pane 'metafile-rotation-example-stuff))
    (setf (fourth stuff) x (fifth stuff) y)
    (when (> 0.25 (third stuff))
      (setf (second stuff) 0.0 
            (third stuff) 1.0))))


;;; This is the :BUTTON-1 :PRESS callback. 
;;; Setup "Stuff" (Used by metafile-rotation-example-display), and start
;;; a timer.  

(defun metafile-rotation-example-do-animate  (pane x y)
  (if-let (stuff (capi:capi-object-property pane 'metafile-rotation-example-stuff))
      (setf (second stuff) 0.0 
            (third stuff) 1.0
            (fourth stuff) x
            (fifth stuff) y)
    (setf (capi:capi-object-property pane 'metafile-rotation-example-stuff)
          (list (metafile-rotation-example-create-metafile pane) 0.0 1.0 x y)))
  (mp:schedule-timer-relative-milliseconds 
   (mp:make-timer 'metafile-rotation-example-animate pane) 
   50 30))


;; This is just in case we want to inspect it.
(defvar *metafile-rotation-example-pane* nil)

(defun metafile-rotation-example ()
  (setq *metafile-rotation-example-pane* 
        (capi:contain (make-instance 'capi:output-pane
                                     :background :navyblue
                                     :draw-with-buffer t
                                     :input-model
                                     '(((:button-1 :press) metafile-rotation-example-do-animate)
                                       (:motion metafile-rotation-example-change-center))
                                     :display-callback 'metafile-rotation-example-display
                                     :initial-constraints '(:min-width 400 :min-height 400))
                      :title "Metafile example"))
  (capi:apply-in-pane-process *metafile-rotation-example-pane*
                              'metafile-rotation-example-do-animate 
                              *metafile-rotation-example-pane* 200 200))


