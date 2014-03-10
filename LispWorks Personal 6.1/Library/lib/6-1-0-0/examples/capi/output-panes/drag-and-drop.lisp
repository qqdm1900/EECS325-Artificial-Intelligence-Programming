;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/output-panes:drag-and-drop.lisp,v 1.4.1.1 2011/08/24 13:26:19 davef Exp $" -*-

;;----------------------------------------------------------------------------
;;
;; examples/capi/applications/drag-and-drop.lisp
;;
;; This example shows how you use drag and drop within the CAPI.
;; The drag-example class is used to supply objects for dragging.
;; The drop-example class contains pane where the objects can be dropped.
;;
;; To try the example, compile and load this file and then execute one of:
;;
;;      (CL-USER::TEST-DRAG-AND-DROP-EXAMPLE)
;;
;; Then drag strings, rectangles or circles from the Drag Example
;; window to the Drop Example window.  The strings can only be dragged
;; as strings.  The rectangles can only be dragged in :value format.
;; The circles can be dragged in either :shape format or as a string.
;; The lefthand pane of the Drop Example will only accept strings.
;; The righthand pane of the Drop Example will accept object in either
;; :value or :shape format.  You can also drag to and from other
;; applications in string format.
;;
;; The rectangles also have an image-function to specify the the image
;; that is used use during the draggin. 
;;
;; Note the example doesn't actually do anything with the values that
;; are dropped. It just prints that it did it in the bottom of the 
;; "Drop Example" window. 
;;
;;----------------------------------------------------------------------------
;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
;;----------------------------------------------------------------------------



(in-package "CL-USER")

(capi:define-interface drag-example ()
  ()
  (:panes
   (message-pane
    capi:title-pane
    :visible-max-width nil))
  (:layouts
   (drag-pane
    capi:pinboard-layout
    (list (make-instance 'capi:item-pinboard-object
                         :text "String 1"
                         :x 10
                         :y 25)
          (make-instance 'capi:item-pinboard-object
                         :text "String 2"
                         :x 30
                         :y 105)
          (make-instance 'capi:item-pinboard-object
                         :text "String 3"
                         :x 20
                         :y 185)
          (make-instance 'capi:rectangle
                         :filled t
                         :x 100
                         :y 0
                         :width 64
                         :height 64
                         :graphics-args '(:foreground :red)
                         :name :red-rectangle)
          (make-instance 'capi:rectangle
                         :filled t
                         :x 120
                         :y 80
                         :width 64
                         :height 64
                         :graphics-args '(:foreground :green)
                         :name :green-rectangle)
          (make-instance 'capi:rectangle
                         :filled t
                         :x 110
                         :y 160
                         :width 64
                         :height 64
                         :graphics-args '(:foreground :blue)
                         :name :blue-rectangle)
          (make-instance 'capi:ellipse
                         :filled t
                         :x 200
                         :y 0
                         :width 64
                         :height 64
                         :graphics-args '(:foreground :red)
                         :name :red-circle)
          (make-instance 'capi:ellipse
                         :filled t
                         :x 220
                         :y 80
                         :width 64
                         :height 64
                         :graphics-args '(:foreground :green)
                         :name :green-circle)
          (make-instance 'capi:ellipse
                         :filled t
                         :x 210
                         :y 160
                         :width 64
                         :height 64
                         :graphics-args '(:foreground :blue)
                         :name :blue-circle))
    :input-model `(((:button-1 :press)
                    ,#'(lambda (pane x y)
                         (drag-example-drag-from pane x y)))
                   ((:button-1 :press :control)
                    ,#'(lambda (pane x y)
                         (drag-example-drag-from pane x y)))
                   ((:button-1 :press :meta)
                    ,#'(lambda (pane x y)
                         (drag-example-drag-from pane x y)))
                   ((:button-1 :press :hyper)
                    ,#'(lambda (pane x y)
                         (drag-example-drag-from pane x y))))
    :title "Drag any of the objects below"
    :title-position :top)
   (main-layout
    capi:column-layout
    '(drag-pane message-pane)))
  (:default-initargs
   :layout 'main-layout
   :title "Drag Example"
   :width 400
   :height 400))


(defparameter *rectangle-drag-margin-around-string* 5)


;;; The image function for dragging the rectangle.
;;; Note that we compute from scratch the image each time, and
;;; we cannot cache them because they are freed automatically by
;;; the drag operation. Normally that is what you want because
;;; the dragging image needs to represent what is dragged, so is different
;;; each time. 
;;; The second and third return values are the HOT-X and HOT-Y coordinates.
;;; This returns the width and height, so the mouse is in the bottom right
;;; corner and does not obscure the image. 

(defun create-dragging-image-for-rectangle-object (pane object)
  (let* ((name (capi:capi-object-name object))
         (color-name (sys:cdr-assoc name'((:red-rectangle . :red)
                                          (:blue-rectangle  . :blue)
                                          (:green-rectangle  . :green))))
         (string (string-capitalize color-name)))
    (multiple-value-bind (left top right bottom)
        (gp:get-string-extent pane string)
      (let ((width (+ (- right left)  (* 2 *rectangle-drag-margin-around-string*)))
            (height (+ (- bottom top) (* 2 *rectangle-drag-margin-around-string*))))
        (let ((pp (gp:create-pixmap-port pane width height :background color-name :clear t)))
          (unwind-protect
              (progn 
                (gp:draw-string pp string 
                                *rectangle-drag-margin-around-string*   ; X
                                (- *rectangle-drag-margin-around-string* top) ; Y
                                :foreground :white
                                :font (capi:simple-pane-font pane)
                                )
                (values (gp:make-image-from-port pp)
                        width  ; HOT X
                        height ; HOT Y
                        ))
            (gp:destroy-pixmap-port pp)))))))
    
(defun drag-example-drag-from (pane x y)
  (let ((object (capi:pinboard-object-at-position pane x y)))
    (typecase object
      (capi:item-pinboard-object
       (let ((string (capi:item-text object)))
         (drag-example-drag-object pane string
                                   nil :operations '(:move :copy :link :generic)
                                   :string string)))
      (capi:rectangle
       (multiple-value-bind (pane-x pane-y)
           (capi:pinboard-pane-position object)
         (let* ((name (capi:capi-object-name object))
                (value (list name (- x pane-x) (- y pane-y))))
           (drag-example-drag-object 
            pane (format nil "~A as a value" name)
            value
            :image-function #'(lambda (pane)
                                (create-dragging-image-for-rectangle-object pane object))))))
      (capi:ellipse
       (let* ((name (capi:capi-object-name object))
              (value (prin1-to-string name)))
         (drag-example-drag-object pane (format nil "~A as a string and a shape" name)
                                   value
                                   :plist (list :shape object)))))))

(defun drag-example-drag-object (pane title &rest drag-args)
  (let ((message-pane (slot-value (capi:element-interface pane) 'message-pane)))
    (setf (capi:title-pane-text message-pane)
          (format nil "Dragging ~A" title))
    (let ((result (apply 'capi:drag-pane-object pane drag-args)))
      (setf (capi:title-pane-text message-pane)
            (if (eq result :none)
                (format nil "Abandoned dragging ~A" title)
              (format nil "Dragged ~(~A~) ~A" result title))))))



(capi:define-interface drop-example ()
  ()
  (:panes
   (string-pane
    capi:output-pane
    :drop-callback 'drop-example-drop-string-callback
    :title "Drop strings below"
    :title-position :top)
   (shape-pane
    capi:output-pane
    :drop-callback 'drop-example-drop-shape-callback
    :title "Drop shapes below"
    :title-position :top)
   (message-pane
    capi:title-pane
    :visible-max-width nil))
  (:layouts
   (main-layout
    capi:column-layout
    '(panes-layout message-pane))
   (panes-layout
    capi:row-layout
    '(string-pane shape-pane)))
  (:default-initargs
   :layout 'main-layout
   :title "Drop Example"
   :width 400
   :height 400))

(defun drop-example-drop-string-callback (pane drop-object stage)
  (flet ((set-effect-for-operation (drop-object)
           ;; In a real application, this would be clever about which
           ;; effects to allow.
           (dolist (effect '(:move :copy :link :generic))
             (when (capi:drop-object-allows-drop-effect-p drop-object effect)
               (setf (capi:drop-object-drop-effect drop-object) effect)
               (return t)))))
    (case stage
      (:formats
       (capi:set-drop-object-supported-formats drop-object '(:string)))
      (:enter
       (when (capi:drop-object-provides-format drop-object :string)
         (set-effect-for-operation drop-object)))
      (:drag
       (when (capi:drop-object-provides-format drop-object :string)
         (set-effect-for-operation drop-object)))
      (:drop
       (when (and (capi:drop-object-provides-format drop-object :string)
                  (set-effect-for-operation drop-object))
         (let ((value (capi:drop-object-get-object drop-object
                                                   pane
                                                   :string)))
           (drop-example-show-drop pane
                                   (format nil "Dropped string ~S for ~(~A~) operation"
                                           value
                                           (capi:drop-object-drop-effect drop-object)))))))))

(defun drop-example-drop-shape-callback (pane drop-object stage)
  (case stage
    (:formats
     (capi:set-drop-object-supported-formats drop-object '(:shape :value)))
    (:enter
     (when (and (or (capi:drop-object-provides-format drop-object :shape)
                    (capi:drop-object-provides-format drop-object :value))
                (capi:drop-object-allows-drop-effect-p drop-object :copy))
       (setf (capi:drop-object-drop-effect drop-object) :copy)))
    (:drag
     (when (and (or (capi:drop-object-provides-format drop-object :shape)
                    (capi:drop-object-provides-format drop-object :value))
                (capi:drop-object-allows-drop-effect-p drop-object :copy))
       (setf (capi:drop-object-drop-effect drop-object) :copy)))
    (:drop
     (cond ((and (capi:drop-object-provides-format drop-object :shape)
                 (capi:drop-object-allows-drop-effect-p drop-object :copy))
            (let ((value (capi:drop-object-get-object drop-object
                                                      pane
                                                      :shape)))
              (drop-example-show-drop pane
                                      (format nil "Dropped shape ~S"
                                              value)))
              (setf (capi:drop-object-drop-effect drop-object) :copy))
           ((and (capi:drop-object-provides-format drop-object :value)
                 (capi:drop-object-allows-drop-effect-p drop-object :copy))
            (let ((value (capi:drop-object-get-object drop-object
                                                      pane
                                                      :value)))
              (drop-example-show-drop pane
                                      (format nil "Dropped value ~S"
                                              value))
              (setf (capi:drop-object-drop-effect drop-object) :copy)))))))

(defun drop-example-show-drop (pane title)
  (let ((message-pane (slot-value (capi:element-interface pane) 'message-pane)))
    (setf (capi:title-pane-text message-pane) title)))



(defun test-drag-and-drop-example ()
  (capi:display (make-instance 'drag-example))
  (capi:display (make-instance 'drop-example)))
