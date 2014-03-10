;; -*- rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/applications:balloons.lisp,v 1.16.1.1 2011/08/24 13:26:19 davef Exp $" -*-

;;;----------------------------------------------------------------------------
;;;
;;; examples/capi/applications/balloons.lisp
;;;
;;; Example of using :DRAW-WITH-BUFFER. 

;;; To try it, compile and load this file and then execute any
;;; of the following.
;;;
;;;      (CL-USER::BALLOONS nil) 
;;;      (CL-USER::BALLOONS t)
;;;
;;; On Cocoa and GTK there shouldn't be any difference. On 
;;; Windows and Motif when passed T it should not flicker (note that
;;; the balloons intentionally wobble). 
;;;
;;; The example also shows usage of timers (search for "timer"). Note
;;; using capi:execute-with-interface-if-alive in the timer function,
;;; so if the interface dies (e.g. the user closes it) we don't get an
;;; error. 
;;;
;;;----------------------------------------------------------------------------
;;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
;;;----------------------------------------------------------------------------

(in-package "CL-USER")

;;; ----------------------------------------------------------------------
;;; Balloon class + display method
;;; ----------------------------------------------------------------------

(defclass balloon (capi:pinboard-object)
  ((color :initform :red :initarg :color :accessor balloon-color)
   (helium-factor :initform 1 :initarg :helium-factor :accessor helium-factor)))


(defmethod capi:draw-pinboard-object (pinboard (object balloon) &key)
  (capi:with-geometry object
    (let* ((b-radius (ash (1- capi:%width%) -1))
           (x-centre (+ capi:%x% b-radius))
           (b-bottom (+ capi:%y% (ash b-radius 1))))
      (gp:draw-circle pinboard x-centre (+ capi:%y% b-radius) b-radius
                      :filled t
                      :foreground (balloon-color object))
      (gp:draw-line pinboard x-centre b-bottom x-centre (+ capi:%y% capi:%height% -1)
                    :foreground :brown)
      (gp:draw-polygon pinboard (list x-centre b-bottom 
                                      (+ x-centre 3) (+ 5 b-bottom)
                                      (- x-centre 3) (+ 5 b-bottom))
                       :filled t
                       :closed t
                       :foreground (balloon-color object))
      )))


(defun make-balloons ()
  (loop for helium-factor from 1 below 4
        append (loop for c below (+ 2 (random 3))
                 collect (make-instance 'balloon
                                        :x (random 500)
                                        :y (random 200)
                                        :width (+ 30 (* 10 helium-factor))
                                        :height (+ 45 (* 15 helium-factor))
                                        :color (nth (random 4) '(:red :green :blue :yellow))
                                        :helium-factor helium-factor))))

                  
;;; ----------------------------------------------------------------------
;;; Main interface
;;; ----------------------------------------------------------------------

(capi:define-interface sky ()
  ((timer :initform nil)
   (balloons :initform (make-balloons))
   (buffered :initform nil :initarg :buffered))
  (:menu-bar activate-menu)
  (:menus
   (activate-menu
    "Balloons"
    (("Go" :callback 'animate-balloons 
           :callback-type :interface
           :enabled-function #'(lambda (self)
                                 (with-slots (timer) self
                                   (not timer))))
     ("Stop" :callback 'stop-animating-balloons 
             :callback-type :interface
             :enabled-function #'(lambda (self)
                                 (with-slots (timer) self
                                   timer))))))
  (:panes
   (sky
    capi:pinboard-layout
    :visible-min-width 500
    :visible-min-height 300
    :draw-with-buffer buffered
    :description balloons
    :background :black
    :input-model '(((:button-1 :press) select-balloon)
                   ((:button-1 :motion) drag-balloon)
                   ((:button-1 :release) release-balloon))
    ))
  (:default-initargs :confirm-destroy-callback 'balloon-interface-dead))

;;; ----------------------------------------------------------------------
;;; Input Model
;;; Allow the user to drag a balloon using the mouse.
;;; ----------------------------------------------------------------------

(defun select-balloon (layout x y)
  (when-let (balloon (capi:pinboard-object-at-position layout x y))
    (setf (capi:capi-object-property layout :grabbed) balloon)
    (setf (capi:capi-object-property layout :grabbed-pos)
          (multiple-value-bind (bx by) (capi:pinboard-pane-position balloon)
                         (cons (- x bx) (- y by)) ))
    (setf (capi:interface-override-cursor (capi:element-interface layout)) :fleur)
  ))

(defun drag-balloon (layout x y)
  (when-let (balloon (capi:capi-object-property layout :grabbed))
    (let ((xy-list (capi:capi-object-property layout :grabbed-pos)))
      (setf (capi:pinboard-pane-position balloon) (values (- x (car xy-list)) (- y (cdr xy-list)))))))

(defun release-balloon (layout x y)
  (declare (ignore x y))
  (when (capi:capi-object-property layout :grabbed)
    (capi:remove-capi-object-property layout :grabbed)
    (setf (capi:interface-override-cursor (capi:element-interface layout)) nil)))


;;; ----------------------------------------------------------------------
;;; Timer 
;;; ----------------------------------------------------------------------

(defun balloon-interface-dead (self)
  (with-slots (timer) self
    (when timer
      (mp:unschedule-timer timer)
      (setq timer nil)))
  t)

;;; Use capi:execute-with-interface-if-alive, because the timer 
;;; can happen after the interface destroyed.
(defun animate-balloons (sky)
  (with-slots (timer) sky
    (setf timer (mp:make-timer 
                 #'(lambda (sky)
                     (capi:execute-with-interface-if-alive
                      sky
                      'animate-balloons-once
                      sky
                      0.05))
                 sky))
    (mp:schedule-timer-relative timer 0.0)))

(defun animate-balloons-once (sky delay)
   (with-slots (balloons timer) sky
     (capi:with-atomic-redisplay (sky)
       (dolist (balloon balloons)
         (animate-balloon balloon)))
     (when timer
       (mp:schedule-timer-relative timer delay))
     ))

(defun stop-animating-balloons (sky)
  (balloon-interface-dead sky))

;;; ----------------------------------------------------------------------
;;; Update balloon function.
;;; ----------------------------------------------------------------------

(defun animate-balloon (bal)
  (multiple-value-bind (x y)
      (capi:pinboard-pane-position bal)
    (multiple-value-bind (width height)
        (capi:pinboard-pane-size bal)
      (decf y (helium-factor bal))
      (setf x (min (capi:simple-pane-visible-width (capi:pinboard-object-pinboard bal))
                   (max (- width) (+ x (random 3) -1))))
      (when (< (+ y height) 0)
        (setf y (capi:simple-pane-visible-height  (capi:pinboard-object-pinboard bal))))
      (setf (capi:pinboard-pane-position bal) (values x y)))))


;;; ----------------------------------------------------------------------
;;; Top level interface
;;; ----------------------------------------------------------------------

(defun balloons (buffered)
  (let ((interface (capi:display (make-instance 'sky :buffered buffered))))
    (capi:execute-with-interface interface 'animate-balloons interface)))


