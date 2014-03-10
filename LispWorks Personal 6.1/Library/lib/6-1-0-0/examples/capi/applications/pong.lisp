;; -*- rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/applications:pong.lisp,v 1.16.1.1 2011/08/24 13:26:20 davef Exp $" -*-

;;;----------------------------------------------------------------------------
;;;
;;; examples/capi/applications/pong.lisp
;;;
;;; This example creates a complete pong application.
;;;
;;; To try it, compile and load this file and then execute
;;;
;;;      (CL-USER::PLAY-PONG)
;;;
;;;
;;;----------------------------------------------------------------------------
;;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
;;;----------------------------------------------------------------------------

(in-package cl-user)

(capi:define-interface pong-game ()
    ((delay :initform 0.02)
     (x-movement :initform 7)
     (y-movement :initform 6)
     (ball-size :initform 20)
     (computer-move :initform 8)
     (player-move :initform 8)
     (game-height :initform 400)
     (game-width :initform 500)
     (slug-height :initform 30)
     (slug-width :initform 10)
     (timer :initform nil)
     (clicks :initform 0)
     (initial-slug-position :initform 40))
  (:panes
   (ball capi:ellipse
         :width ball-size
         :height ball-size
         :graphics-args `(:foreground ,(color:make-rgb 1.0 1.0 0.0))
         :filled t)
   (ball2 capi:ellipse
         :width ball-size
         :height ball-size
         :graphics-args `(:foreground ,(color:make-rgb 0.9 0.9 0.0))
         :filled t)
   (ball3 capi:ellipse
         :width ball-size
         :height ball-size
         :graphics-args `(:foreground ,(color:make-rgb 0.75 0.75 0.0))
         :filled t)
   (ball4 capi:ellipse
         :width ball-size
         :height ball-size
         :graphics-args `(:foreground ,(color:make-rgb 0.6 0.6 0.0))
         :filled t)
   (left-paddle capi:rectangle
                :x 0
                :y 0
                :visible-min-width slug-width
                :visible-min-height slug-height
                :graphics-args '(:foreground :red)
                :filled t)
   (right-paddle capi:rectangle
                 :x (- game-width slug-width)
                 :y 0
                 :visible-min-width slug-width
                 :visible-min-height slug-height
                 :graphics-args '(:foreground :red)
                :filled t)
   )
  (:layouts
   (game capi:row-layout
         '(board)
         :gap 0 
         :visible-min-height game-height
         :visible-min-width game-width
         :visible-max-width t)
   (board capi:pinboard-layout
          '(ball4 ball3 ball2 ball left-paddle right-paddle)
          :background :darkgreen
          :input-model '(((:button-1 :motion) move-paddle))
          :draw-with-buffer t))
  (:menu-bar game-menu)
  (:menus
   (game-menu
    "Game"
    (("New" :callback 'play-game 
            :callback-type :interface 
            :enabled-function #'(lambda (self) 
                                  (with-slots (timer) self
                                    (not timer)))))))
  (:default-initargs :confirm-destroy-callback 'interface-dead))

(defun interface-dead (self)
  (with-slots (timer) self
    (when timer
      (mp:unschedule-timer timer)
      (setq timer nil)))
  t)

(defun move-paddle (self x y)
  (declare (ignore x))
  (with-slots (right-paddle) (capi:element-interface self)
    (multiple-value-bind 
        (x ignore)
        (capi:pinboard-pane-position right-paddle)
      (declare (ignore ignore))
      (setf (capi:pinboard-pane-position right-paddle)
            (values x y)))))

      
(defun play-game (self)
  (with-slots (timer x-movement ball ball2 ball3 ball4) self
    (dolist (bl (list ball ball2 ball3 ball4))
      (setf (capi:pinboard-pane-position bl) (values 250 200)))
    (unless (minusp x-movement)
      (setf x-movement (- x-movement)))
    (setf timer (mp:make-timer 'make-a-pong-move self))
    (mp:schedule-timer-relative timer 0)))

(defvar *inside-pong-move* nil)

(defun internal-make-a-pong-move (self)
  (unless *inside-pong-move*
    (let ((*inside-pong-move* t))
      (catch 'finished
        (with-slots (board left-paddle right-paddle ball-size
                           slug-height slug-width 
                           timer delay x-movement y-movement
                           ball ball2 ball3 ball4 computer-move) self
          (multiple-value-bind (x y)
              (capi:pinboard-pane-position ball)
            (multiple-value-bind (board-width board-height)
                (capi:simple-pane-visible-size board)
              (when (and board-width board-height) ; still on screen
                (cond ((<= slug-width x (- board-width (+ slug-width ball-size)))
                       t)
                      ((> x (- board-width (+ slug-width ball-size)))
                       (if (<= (nth-value 1 (capi:pinboard-pane-position right-paddle))
                               (+ y (ash ball-size -1))
                               (+ (nth-value 1 (capi:pinboard-pane-position right-paddle))
                                  slug-height))
                           (progn
                             (setf y-movement (- (random 13) 6))
                             (setf x-movement (- x-movement)))
                         (progn 
                           (when timer
                             (mp:unschedule-timer timer))
                           (setf timer nil)
                           (throw 'finished nil))))
                      ((> slug-width x)
                       (if (<= (nth-value 1 (capi:pinboard-pane-position left-paddle))
                               (+ y (ash ball-size -1))
                               (+ (nth-value 1 (capi:pinboard-pane-position left-paddle))
                                  slug-height))
                           (progn
                             (setf y-movement (- (random 13) 6))
                             (setf x-movement (- x-movement)))
                         (progn 
                           (mp:unschedule-timer timer)
                           (setf timer nil)
                           (throw 'finished nil))))
                      (t (setf x-movement (- x-movement))))
                (unless (<= 0 y (- board-height ball-size))
                  (setf y-movement (- y-movement)))
                (loop for (to from) in (list (list ball4 ball3)
                                             (list ball3 ball2)
                                             (list ball2 ball))
                      do (multiple-value-bind (x y)
                             (capi:pinboard-pane-position from)
                           (setf (capi:pinboard-pane-position to) (values x y))))
                (setf (capi:pinboard-pane-position ball)
                      (values (incf x x-movement)
                              (incf y y-movement)))
                (if (> x (ash board-height -1))
                    (let* ((paddle-middle (+ (nth-value 1 (capi:pinboard-pane-position left-paddle))
                                             (floor slug-height 2)))
                           (court-middle (ash (capi:simple-pane-visible-height board) -1))
                           (diff (- court-middle paddle-middle))
                           (move (+ (truncate diff 5) (- (random 7) 3))))
                      (multiple-value-bind (x y)
                          (capi:pinboard-pane-position left-paddle)
                        (setf (capi:pinboard-pane-position left-paddle)
                              (values x (+ y move)))))
                  (if (> y (nth-value 1 (capi:pinboard-pane-position left-paddle)))
                      (unless (> (+ (+ slug-height (nth-value 1 (capi:pinboard-pane-position left-paddle)))
                                    computer-move)
                                 (capi:simple-pane-visible-height board))
                        (multiple-value-bind (x y)
                            (capi:pinboard-pane-position left-paddle)
                          (setf (capi:pinboard-pane-position left-paddle)
                                (values x (+ y computer-move)))))
                    (unless (< (- (nth-value 1 (capi:pinboard-pane-position left-paddle)) computer-move) 0) 
                      (multiple-value-bind (x y)
                          (capi:pinboard-pane-position left-paddle)
                        (setf (capi:pinboard-pane-position left-paddle)
                              (values x (- y computer-move))))))))))
          (when timer
            (mp:schedule-timer-relative timer delay)))))))

(defun make-a-pong-move (self)
  (unless (capi-internals:representation self)
    (with-slots (timer) self
      (when timer 
        (mp:unschedule-timer timer)))
    (return-from make-a-pong-move nil))
  (capi:execute-with-interface-if-alive 
   self 'internal-make-a-pong-move self))

(defun play-pong ()
  (capi:display (make-instance 'pong-game)))

