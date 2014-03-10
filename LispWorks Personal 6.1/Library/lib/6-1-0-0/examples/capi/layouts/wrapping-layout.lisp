;; -*- rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/layouts:wrapping-layout.lisp,v 1.8.11.1 2011/08/24 13:26:21 davef Exp $" -*-


;;----------------------------------------------------------------------------
;;
;; wrapping-layout.lisp
;;
;; This example demonstrates a layout that knows how to wrap objects to fit
;; into the available space. The default demonstration illustrates word
;; wrapping whilst the more advanced demo includes arbitrary CAPI objects
;; interspersed in the text.
;;----------------------------------------------------------------------------
;; To run this demo, compile and load this file and then execute:
;;
;;    (DISPLAY-PARAGRAPHS)
;;
;; To include arbitrary CAPI objects in the demo do:
;;
;;    (DISPLAY-PARAGRAPHS :RANDOM-PANES T)
;;----------------------------------------------------------------------------
;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
;;----------------------------------------------------------------------------


(in-package "CL-USER")


;;----------------------------------------------------------------------------
;; Wrapping layout
;;----------------------------------------------------------------------------

(defclass wrapping-layout (capi:x-y-adjustable-layout)
  ((x-gap :initform 3 :initarg :x-gap)
   (y-gap :initform 3 :initarg :y-gap)))

(defmethod capi:interpret-description ((layout wrapping-layout)
                                       description interface)
  (loop for item in description
	collect (capi:parse-layout-descriptor item interface layout)))

(defun position-to-split-line (self panes width)
  (with-slots (x-gap) self
    (loop for (pane . remaining-panes) = panes then remaining-panes
          for position from 0
          for pane-width  = (capi:with-geometry pane
                              (or capi:%width% capi:%min-width%))
          for pane-height = (capi:with-geometry pane
                              (or capi:%height% capi:%min-height%))
          for old-end-width = 0 then end-width
          for pane-descent = (pane-descent pane)
          for pane-ascent =  (- pane-height pane-descent)
          for end-width = (+ pane-width x-gap) then (+ end-width x-gap pane-width)
          for max-descent = pane-descent then (max max-descent pane-descent)
          for max-ascent  = pane-ascent  then (max max-ascent  pane-ascent)
          when (and (not (eq position 0))
		    (> end-width width))
          return (values position max-ascent max-descent old-end-width)
          when (null remaining-panes)
          return (values (1+ position) max-ascent max-descent end-width))))

(defun generate-next-line (self panes x y width)
  (with-slots (x-gap) self
    (multiple-value-bind
        (position ascent descent total-width)
        (position-to-split-line self panes width)
      (let* ((height (+ ascent descent))
             (x-adjust (capi:layout-x-adjust self))
             (x-offset
              (case x-adjust
		(:justified x)
		(t
		 (+ (capi:pane-adjusted-offset self
					       (capi:layout-x-adjust self)
					       width total-width)
		    x))))
             (justified-x-gap
              (+ x-gap
                 (or (when (and (eq x-adjust :justified)
                                (> position 1))
		       (let ((extra
			      (floor (- width total-width) (1- position))))
			 (when (< extra 10)
			   extra)))
                     0))))
        (loop for count below position
              for pane in panes
              for pane-width  = (capi:with-geometry pane
                                  (or capi:%width% capi:%min-width%))
              for pane-height = (capi:with-geometry pane
                                  (or capi:%height% capi:%min-height%))
              for new-x = x-offset then (+ x-offset end-width)
              for end-width = (+ pane-width justified-x-gap)
                         then (+ end-width  justified-x-gap pane-width)
              do
              (capi:with-geometry pane
	        (setf capi:%x% new-x
		      capi:%y% (case (capi:layout-y-adjust self)
                                 (:fonts
                                  (+ y height
                                     (- descent (pane-ascent pane))))
                                 (t
                                  (+ y (capi:pane-adjusted-offset
                                        self (capi:layout-y-adjust self)
                                        height pane-height))))
		      capi:%width%  pane-width
		      capi:%height% pane-height)))
        (values (nthcdr position panes) height)))))

(defmethod capi:calculate-constraints ((self wrapping-layout))
    (let ((constrained-min-width 5))
      (capi:map-pane-children 
       self
       #'(lambda (pane)
           (capi:with-geometry pane
             (when capi:%min-width%
               (when (> capi:%min-width% constrained-min-width)
                 (setq constrained-min-width capi:%min-width%))))))
      (capi:with-geometry self
        (setq capi:%min-width% constrained-min-width
              capi:%min-height% nil
              capi:%max-width% nil
              capi:%max-height% nil))))

(defmethod capi:calculate-layout ((self wrapping-layout) x y width height)
  (declare (ignore height))
  (with-slots (y-gap) self
    (let (panes 
          (new-y y))
      (capi:map-pane-children self #'(lambda (child) (push child panes)))
      (setq panes (nreverse panes))
      (loop do
            (multiple-value-bind
                (remaining-panes max-height)
                (generate-next-line self panes x new-y width)
              (setq panes remaining-panes)
              (incf new-y (+ max-height y-gap)))
            while panes))))


;;----------------------------------------------------------------------------
;; Testing code
;;----------------------------------------------------------------------------

(defvar *fonts*
    (list
     (gp:make-font-description :family "helvetica" :weight :medium :slant :roman :size 12)
     (gp:make-font-description :family "helvetica" :weight :bold :slant   :roman :size 12)
     (gp:make-font-description :family "helvetica" :weight :bold :slant   :roman :size 20)
     (gp:make-font-description :family "helvetica" :weight :medium :slant :italic :size 20)
     (gp:make-font-description :family "courier" :weight   :medium :slant :roman :size 20)))

(defun test-wrapping-layout (&key text-only (y-adjust :centre))
  (capi:contain
   (make-instance
    'wrapping-layout
    :y-adjust y-adjust
    :description
    (append
     (unless text-only
       (list (make-instance 'capi:image-pinboard-object)))
     (loop for text in '("Hello"
			 "world"
			 "how"
			 "are"
			 "you"
			 "doing.")
	   collect
	   (make-instance 'capi:item-pinboard-object :text text))
     (unless text-only
       (list (make-instance 'capi:image-pinboard-object)))
     (loop for text in '("Hello"
			 "world"
			 "how"
			 "are"
			 "you"
			 "doing.")
	   collect
	   (make-instance 'capi:item-pinboard-object :text text))
     (unless text-only
       (list
        (make-instance 'capi:push-button :text "Press Me")))))))


;;----------------------------------------------------------------------------
;; Font centering code
;;----------------------------------------------------------------------------

(defmethod pane-descent ((self t))
  0)

(defmethod pane-ascent ((self t))
  (capi:with-geometry self
    (- (or capi:%height% capi:%min-height%) (pane-descent self))))

;;----------------------------------------------------------------------------
;; display-paragraphs
;;----------------------------------------------------------------------------

(defparameter *paragraphs*
    '("This is a long paragraph containing absolutely no content whatsover for the purposes of this demo only." "This is another paragraph, and quite a long one at that, if anything slightly longer than the one before."))

(defun words-in-paragraph (paragraph)
  (loop for old-position = 0 then (1+ position)
        for position = (position #\space paragraph :start old-position)
        for word = (subseq paragraph old-position position)
        collect word
        while position))

(defun test-graph-children-function (x)
  (when (< x 4)
    (list (* x 2)
          (1+ (* x 2)))))

(defparameter *random-objects*
    '((capi:text-input-pane)
      (capi:push-button :text "Press Me")
      (capi:radio-button :text "Button" :background :yellow)
      (capi:check-button :text "Button" :background :yellow)
      (capi:ellipse :width 50 :height 50)
      (capi:ellipse :width 50 :height 50 :filled t)
      (capi:rectangle :width 50 :height 50)
      (capi:graph-pane :horizontal-scroll nil :vertical-scroll nil
                       :roots (1) :visible-border nil
                       :children-function test-graph-children-function)))

(defparameter *random-colors*
    '(:red :blue :purple))

(defun make-random-pinboard-object ()
  (let ((object-details (nth (random (length *random-objects*))
			     *random-objects*))
        (foreground (nth (random (length *random-colors*)) *random-colors*)))
    (apply 'make-instance (first object-details)
           (append
            (if (subtypep (first object-details) 'capi:simple-pane)
                (list :foreground foreground)
              (list :graphics-args (list :foreground foreground)))
            (rest object-details)))))

(defun display-paragraphs (&key (paragraphs *paragraphs*)
                                (x-adjust :justified)
                                (y-adjust :fonts)
                                random-panes)
  (capi:contain
   (loop with screen = (capi:convert-to-screen)
         for paragraph in paragraphs
         collect
         (make-instance
          'wrapping-layout
          :x-adjust x-adjust
          :y-adjust y-adjust
          :description
          (loop for text in (words-in-paragraph paragraph)
                collect
                (make-instance 'capi:item-pinboard-object
                               :text text
                               :graphics-args 
                               (list :font (gp:find-best-font screen 
                                                              (nth (random (length *fonts*))
                                                                   *fonts*))))
                when (and random-panes (= (random 3) 1))
                collect (make-random-pinboard-object))))
   :best-width 500
   :best-height 500))
