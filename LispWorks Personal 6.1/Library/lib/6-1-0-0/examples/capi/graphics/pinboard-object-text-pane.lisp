;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/graphics:pinboard-object-text-pane.lisp,v 1.1.11.2 2011/11/04 20:21:03 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

;;
;; An example showing a way to create a multi-line text pane
;; implemented entirely in pinboard objects.
;;
;;----------------------------------------------------------------------------
;;
;; To run this example, compile and load this file and then execute:
;;
;;       (CL-USER::TEST-MULTI-LINE-TEXT-PANES 30)
;;       (CL-USER::TEST-MULTI-LINE-TEXT-PANES 50)

;; This displays three similar bits of texts, wrapped for a width of
;; specific number of characters (the argument above). Each bit of
;; text is aligned differently, because of a different X-ADJUST in its
;; the layout. Each bit of text has a different color, because the
;; pinboard objects that display it have different FOREGROUND in their
;; GRAPHICS-ARGS.
;;----------------------------------------------------------------------------

(in-package "CL-USER")

;;; Specify the text, adjustment and color of each bit of text

(defvar *text-bit-pane-specs*
  '(("This text will be left aligned in the pane. It will be aligned on the left." 
     :left :black)
    ("This text will be centered in the middle of this pane. It will be centered."
     :centre :blue)
    ("This text will be right aligned in the pane. It will be aligned on the right."
     :right :darkgreen)))

;;----------------------------------------------------------------------------
;; Multi-line-text-pane definition
;;----------------------------------------------------------------------------

;;; This is a subclass of CAPI:COLUMN-LAYOUT, so just displays its
;;; children (the items in its DESCRIPTION) one below the other. The
;;; specialization is used to define what there is in the description,
;;; by the method (setf multi-line-text-pane-text) which sets the
;;; description.

(defclass multi-line-text-pane (capi:column-layout)
  ((width-of-text :initarg :width-of-text :reader width-of-text)
   (text-color :initarg :text-color :reader text-color :initform :black))
  (:default-initargs
   :y-gap 0 
   :visible-max-width nil
   ))

;;; This sets the text in the multi-line-text-pane, by setting its
;;; DESCRIPTION to CAPI:ITEM-PINBOARD-OBJECTs that display the lines
;;; of the text.  Note that for this to work, multi-line-text-pane
;;; must be a child of a CAPI:PINBOARD-OBJECT or subclass, because
;;; otherwise the pinboard objects cannot work. In this example this
;;; is inside a CAPI:SIMPLE-PINBOARD-LAYOUT which is created by
;;; test-multi-line-text-panes.

(defmethod (setf multi-line-text-pane-text) (text (self multi-line-text-pane))
  (let ((strings (capi:wrap-text text (width-of-text self)))
        (graphics-args (list :foreground (text-color self))))
    (setf (capi:layout-description self)
          (loop for string in strings
                collect (make-instance 'capi:item-pinboard-object
                                       :text string
                                       :graphics-args graphics-args)))))


(defmethod initialize-instance :after ((self multi-line-text-pane)
                                       &key text (x-adjust :left))
  (declare (ignorable x-adjust))
  (setf (multi-line-text-pane-text self) text))


(defun create-panes-for-bits-of-text (text-width)
  (loop for spec in *text-bit-pane-specs*
        collect
        (destructuring-bind (text x-adjust text-color)
            spec
            (make-instance 
             'multi-line-text-pane
             :text-color text-color
             :x-adjust x-adjust
             :text text
             :width-of-text text-width))))

;;----------------------------------------------------------------------------
;; test function
;;----------------------------------------------------------------------------

;;; We have a CAPI:SIMPLE-PINBOARD-LAYOUT to make pinboard objects
;;; work inside. The CAPI:SIMPLE-PINBOARD-LAYOUT makes its single
;;; child, the CAPI:COLUMN-LAYOUT, the same size as its own size. The
;;; column layout puts its children (the DESCRIPTION) one below the
;;; other.

(defun test-multi-line-text-panes (text-width)
  (capi:contain
   (make-instance 
    'capi:simple-pinboard-layout 
    :description 
    (list 
     (make-instance
      'capi:column-layout
      :y-gap 5
      :description (create-panes-for-bits-of-text text-width))))
   :title "Pinboard object text pane"
   ))
