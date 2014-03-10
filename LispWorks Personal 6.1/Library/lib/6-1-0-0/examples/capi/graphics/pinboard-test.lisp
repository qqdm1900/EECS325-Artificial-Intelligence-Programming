;; -*- rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/graphics:pinboard-test.lisp,v 1.12.1.2 2011/11/04 20:28:22 davef Exp $" -*-

;;----------------------------------------------------------------------------
;;
;; examples/capi/graphics/pinboard-test.lisp
;;
;; This example demonstrates the uses of pinboard-objects and
;; pinboard-layouts in the CAPI.
;;
;; To try it, compile and load this file and then execute:
;;
;;      (CL-USER::TEST-PINBOARD)
;;
;; The example demonstrates:
;;
;; 1) Define your own pinboard-objects by defining sub-classes of
;;    CAPI:PINBOARD-OBJECT and defining CAPI:DRAW-PINBOARD-OBJECT
;;    for these classes. Note that the example define kinds of objects
;;    that CAPI already have, but for more complex cases you may need 
;;    your own.
;; 
;; 2) Using :INPUT-MODEL, in this case to allow the user to drag the
;;    geometry of a new object. 

;; 3) Drawing an outliner while the user drags. We do that
;;    by having a CAPI:DRAWN-PINBOARD-OBJECT that we add to the
;;    pane while dragging and remove when finishing. The dragging is
;;    done by simply changing the geometry of the outliner. 
;;
;;----------------------------------------------------------------------------
;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
;;----------------------------------------------------------------------------


(in-package "CL-USER")

;;----------------------------------------------------------------------------
;; Define an interface
;;----------------------------------------------------------------------------

(capi:define-interface pinboard-test ()
  ((start-x :accessor start-x)
   (start-y :accessor start-y)
   (outliner :accessor outliner 
             :initform (make-instance 'capi:drawn-pinboard-object
                                      :display-callback
                                      'outliner-display-callback
                                                         )))
  (:panes
   (shape-buttons
    capi:radio-button-panel
    :accessor shape-buttons
    :title "Shape:"
    :title-position :top
    :layout-class 'capi:column-layout
    :items '(rectangle ellipse text)
    :print-function 'string-capitalize)
   (color-buttons
    capi:radio-button-panel
    :accessor color-buttons
    :title "Color:"
    :title-position :top
    :layout-class 'capi:column-layout
    :items '(:red :green :blue :black :white)
    :print-function 'string-capitalize)
   (style-buttons
    capi:radio-button-panel
    :accessor style-buttons
    :title "Style:"
    :title-position :top
    :layout-class 'capi:column-layout
    :items '(:solid :outline)
    :print-function 'string-capitalize))
  (:layouts
   (pinboard
    capi:pinboard-layout
    '()
    :background :white
    :input-model '(((:button-1 :press)   press-button-1)
                   ((:motion :button-1)  drag-button-1)
                   ((:button-1 :release) release-button-1)
                   ((:button-2 :press)   press-button-2))
    :draw-with-buffer t
    :vertical-scroll t
    :horizontal-scroll t
    :visible-min-width 500
    :visible-min-height 500)
   (button-layout
    capi:column-layout
    '(shape-buttons color-buttons style-buttons)
    :y-gap 10)
   (sub-layout
    capi:column-layout
    '(pinboard)
    :title "Drag button 1 to add an object and button 2 to delete one")
   (main-layout
    capi:row-layout
    '(button-layout sub-layout)
    ))
  (:default-initargs
   :layout 'main-layout
   :title "Pinboard Test"
   :best-height 400
   :best-width 200))

;;; --------------------------------------------------
;;; some utility functions
;;; --------------------------------------------------

(defun pinboard-test-color (pinboard-test)
  (capi:choice-selected-item (color-buttons pinboard-test)))

(defun pinboard-test-filled (pinboard-test)
  (eq (capi:choice-selected-item (style-buttons pinboard-test)) :solid))

(defun pinboard-test-shape (pinboard-test)
  (capi:choice-selected-item (shape-buttons pinboard-test)))

;;; Compute the geometry based on the end point (End-X and End-Y) 
;;; and the start point (START-X and START-Y)

(defun pinboard-test-new-geometry (interface  end-x end-y)
  (let ((start-x (start-x interface))
        (start-y (start-y interface)))
    (multiple-value-bind (use-x width)
        (if (> end-x start-x)
            (values start-x (- end-x start-x))
          (values end-x (- start-x end-x)))
      (if (> end-y start-y)
          (values use-x start-y width (- end-y start-y))
        (values use-x end-y width (- start-y end-y))))))

;;----------------------------------------------------------------------------
;; Display callback of the outliner. 
;;----------------------------------------------------------------------------

;;; The complexity is that we want to display different thing
;;; depending on the kind of object that we are adding. Here it
;;; is just an ellipse or rectangle, in real application there
;;; may be other things to do. 

;;; Note that we need to ensure that we don't draw outside
;;; the given area. Antialiasing can cause larger drawing
;;; than expected, so we use :shape-mode :plain to prevent
;;; antialiasing. 


(defun outliner-display-callback (pane object x y width height)
  (declare (ignore object))
  (unless (or (zerop width) (zerop height))
    (let ((ellipse-p (eq (pinboard-test-shape 
                          (capi:element-interface pane))
                         'ellipse)))
      (if ellipse-p
          (let* ((radius-x (/ width 2))
                 (center-x (+ radius-x x))
                 (radius-y (/ height 2))
                 (center-y (+ radius-y y)))
            (gp:draw-ellipse pane center-x center-y radius-x radius-y
                             :foreground :blue :shape-mode :plain))
        (gp:draw-rectangle pane x y width height 
                           :foreground :blue
                           :shape-mode :plain ;; antialasing makes it bigger
                           :dashed t
                           )))))


;;----------------------------------------------------------------------------
;; Define pinboard-object classes
;;----------------------------------------------------------------------------

;;; A new type of pinboard-object is just a sub-class of CAPI:PINBOARD-OBJECT,
;;; and at least CAPI:DRAW-PINBOARD-OBJECT (otherwise it draws nothing). 
;;; Note that there are already various useful sub-classes of CAPI:PINBOARD-OBJECT
;;; in CAPI (see the documentation) with pre-define darwing methods. 

;;; RECTANGLE 

(defclass rectangle (capi:pinboard-object)
  ((foreground :accessor foreground :initform nil :initarg :foreground)
   (filled :accessor filled :initform nil :initarg :filled))
  (:default-initargs
   :visible-min-width 30
   :visible-min-height 30))

;;; Drawing a rectangle object. 
;;; Non-filled rectangles draw the x+width and Y+height pixels, which are
;;; outside the geometry of the object, so for non-filled rectangles we
;;; need to subtract 1 from the width and height. 
(defmethod capi:draw-pinboard-object (pinboard (rectangle rectangle) &key)
  (capi:with-geometry rectangle
    (let ((filled? (filled rectangle)))
      (gp:draw-rectangle pinboard
                         capi:%x% capi:%y%
                         (if filled? capi:%width% (1- capi:%width%))
                         (if filled? capi:%height% (1- capi:%height%))
                         :foreground (foreground rectangle)
                         :filled filled?))))


;;; ELLIPSE 

(defclass ellipse (capi:pinboard-object)
  ((foreground :accessor foreground :initform nil :initarg :foreground)
   (filled :accessor filled :initform nil :initarg :filled))
  (:default-initargs
   :visible-min-width 30
   :visible-min-height 30))

;;; Drawing an ellipse object. 
;;; gp:draw-ellipse takes center and radii, so need to compute them. 

(defmethod capi:draw-pinboard-object (pinboard (ellipse ellipse) &key)
  (capi:with-geometry ellipse
    (let ((x-radius (/ (1- capi:%width%) 2))
          (y-radius (/ (1- capi:%height%) 2)))
      (gp:draw-ellipse pinboard
		       (+ capi:%x% x-radius)
                       (+ capi:%y% y-radius)
                       x-radius
                       y-radius
		       :foreground (foreground ellipse)
                       :filled (filled ellipse)))))




;;; TEXT 

(defclass text (capi:pinboard-object)
  ((foreground :accessor foreground :initform nil :initarg :foreground)
   (filled :accessor filled :initform nil :initarg :filled))
  (:default-initargs
   :visible-min-width 30
   :visible-min-height 30))

(defmethod capi:draw-pinboard-object (pinboard (text text) &key)
  (capi:with-geometry text
    (let ((foreground (foreground text))
          (background (capi:simple-pane-background pinboard))
          (filled (filled text)))
      (gp:draw-x-y-adjusted-string pinboard
                                   "This is a test."
		                   capi:%x%
		                   capi:%y%
                                   :y-adjust :top
                                   :foreground (if filled background foreground)
                                   :background (if filled foreground background)
                                   :block (filled text)))))




;;----------------------------------------------------------------------------
;; The callbacks in the :INPUT-MODEL 
;;----------------------------------------------------------------------------

;;; The next four functions are called in response to user mouse gestures
;;; because they appear in the :INPUT-MODEL of the PINBOARD-LAYOUT in the
;;; DEFINE-INTERFACE form at the top. 

;;; Press-button-1 - just set up the starting position.

(defun press-button-1 (pinboard press-x press-y)
  (let* ((interface (capi:element-interface pinboard)))
    (setf (start-x interface) press-x)
    (setf (start-y interface) press-y)))

;;; Draw-button-1 - first ensure that the outliner is displayed, 
;;; and then set its geometry.  The display is done by the setting,
;;; but it is dealyed until the end of the capi:with-atomic-redisplay
;;; form, so happens only once. 

(defun drag-button-1 (pinboard drag-x drag-y)
  (capi:with-atomic-redisplay (pinboard)
    (let* ((interface (capi:element-interface pinboard))
           (outliner (outliner interface)))
      ;; ensure the outliner is displayed.
      (unless (capi:pinboard-object-pinboard outliner)
        (capi:manipulate-pinboard pinboard outliner :add-top))
      
      ;; compute the new location of the outliner. 
      (multiple-value-bind (x y width height)
          (pinboard-test-new-geometry interface drag-x drag-y)
        ;; set the new geometry
        (setf (capi:pinboard-pane-position outliner) (values x y)
              (capi:pinboard-pane-size outliner) (values width height))))))

;;; Release-button-1 - first undisplay the outliner. Then compute
;;; the gometry of the new object, create it and add it to the 
;;; pinboard-layout

(defun release-button-1 (pinboard release-x release-y)
  (let* ((interface (capi:element-interface pinboard))
         (outliner (outliner interface)))

    ;; Remove the outliner
    (when (capi:pinboard-object-pinboard outliner)
      (capi:manipulate-pinboard pinboard outliner :delete))

    ;; Compute new geometry and check it yis reasonable
    (multiple-value-bind (x y width height)
        (pinboard-test-new-geometry interface release-x release-y)
      (when (and (> width 1) (> height 1))
          
        ;; Create the appropriate pinboard-object.
        (let ((new-object (make-instance (pinboard-test-shape interface)
                                         :foreground (pinboard-test-color interface)
                                         :filled (pinboard-test-filled interface)
                                         :x x 
                                         :y y
                                         :visible-min-width  width
                                         :visible-min-height height)))
          ;; Add to the pinboard-layout on top of existing objects. 
          (capi:manipulate-pinboard pinboard new-object :add-top))))))

;;; Press-Button-2 - check if the gesture was inside the area of
;;; an object, and if it was just remove it. 

(defun press-button-2 (pinboard press-x press-y)
  (let ((object (capi:pinboard-object-at-position pinboard press-x press-y)))
    (when object
      (capi:manipulate-pinboard pinboard object :delete))))


;;----------------------------------------------------------------------------
;; The test function
;;----------------------------------------------------------------------------

(defun test-pinboard ()
  (capi:display (make-instance 'pinboard-test)))
