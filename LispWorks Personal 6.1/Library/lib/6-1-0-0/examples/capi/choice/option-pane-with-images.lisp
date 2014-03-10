;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/choice:option-pane-with-images.lisp,v 1.2.2.1 2011/08/24 13:26:21 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

(in-package "CL-USER")

;;; An example of a option-pane with images. Also demonstrate
;;; using :POPUP-CALLBACK, :POPDOWN-CALLBACK and the setting
;;; of enabled positions. 

;;; To try it, compile and load this file and then execute:
;;;
;;;      (cl-user::test-option-pane-with-images)
;;;
;;; This shows an option pane, where each item is associated with
;;; an image. The images are all displayed above th eoption-pane
;;; ina row, with the image of the selected item higlighted in red. 

;;; When the OPTION-PANE raises the menu, it disabled one of the 
;;; unselected items at random. 
;;; 
;;; The OPTION-PANE prints to the standard-output all the
;;; interactions that occur in it. 
;;; 
;;; The option-pane gets images because it has a :IMAGE-FUNCTION.
;;; In the example below, the function takes a subimage of a 
;;; larger image, using gp:MAKE-SUB-IMAGE. The larger
;;; image is also displayed in an OUTPUT-PANE above the OPTION-PANE. 



(defvar *the-items* '(First second third fourth fifth sixth ))

(capi:define-interface option-pane-with-images-example ()
  ((image :initform nil)
   (vec :initform (make-array 6)))
  
  (:panes
   (option-pane-with-images
    capi:option-pane 
    :image-function #'(lambda (data)
                        (option-pane-with-images-example-get-sub-image capi:interface data))
    :items *the-items*
    :print-function 'string-capitalize 
    :popup-callback 'option-pane-with-images-popup-callback
    :popdown-callback 'option-pane-with-images-popdown-callback
    :callback-type :interface-data
    :selection-callback 'option-pane-with-images-selection-callback)
   (image-displayer
    capi:output-pane
    :visible-min-width 96  ;;; the size of the image
    :visible-min-height 16
    :background :lightcyan
    :display-callback 'image-displayer-display-callback))
  (:layouts
   (main-layout
    capi:column-layout
    '(image-displayer option-pane-with-images))))

;;; The :POPUP-CALLBACK of the OPTION-PANE. Disable at random
;;; one of the items except the selected one. Prints what 
;;; it is doing. 
(defun option-pane-with-images-popup-callback (option-pane)
  (let* ((selection (capi:choice-selection option-pane))
         (disable (mod (+ (random 5) 1 selection) 6)))
    (format t "Option-pane popup with selected item ~a, disabling ~d~%" 
            (capi:get-collection-item option-pane selection)
            disable)
    (setf (capi:option-pane-enabled-positions option-pane)
          (remove disable '(0 1 2 3 4 5 )))))

;;; The :POPDOWN-CALLBACK of the OPTION-PANE. Report it and
;;; all options selectable. 
(defun option-pane-with-images-popdown-callback (option-pane)
  (format t "    Option-pane popped down without changing selection : ~a~%"
          (capi:choice-selected-item option-pane))
  (setf (capi:option-pane-enabled-positions option-pane) :all))

;;; The :SELECTION-CALLBACK of the OPTION-PANE. Report the
;;; new selection and update the image-displayer. 
;;; We use the enable-positions as a flag whether
;;; it was by a menu or keyboard. Also Make all 
;;; options selectable.

(defun option-pane-with-images-selection-callback (interface data)
  (let* ((option-pane (slot-value interface 'option-pane-with-images))
         (enabled-positions (capi:option-pane-enabled-positions option-pane)))
    (format t "    Option-pane got a new selection by ~a: ~a~%" 
            (if (eq enabled-positions  :all) "keyboard" "menu    ") data)
    (gp:invalidate-rectangle
     (slot-value interface
                 'image-displayer))
    (setf (capi:option-pane-enabled-positions option-pane)
          :all)))

;;; The diplplay-callback of the image displayer. 
;;; Display a different background for the sub image that
;;; is selected, and then display the whole image. 
;;; For simplicity this draws everything each time. 

(defun image-displayer-display-callback (pane x y width height)
  (declare (ignore x y width height))
  (let* ((interface (capi:element-interface pane))
         (option-pane (slot-value interface 'option-pane-with-images))
         (selection (capi:choice-selection option-pane))
         (selected-image-x (* selection 16)))

    ;; background for the selected sub image. 
    (gp:draw-rectangle pane 
                       selected-image-x 0  ;;; X and Y
                       16 16
                       :filled t
                       :foreground :red)
    (gp:draw-image pane 
                   (option-pane-with-images-example-get-image interface)
                   0 0 )))

;;; Return the main image. 
;;; The external image is part of the LW distribution.
;;; It is a 96x16 image with a color table, with color
;;; 7 being the background. We force the background to be really
;;; transparent, because it ends up on different windows with potentailly
;;; different backgrounds. We do this by passing a cons of the background 
;;; color index (7) and the  "background color" (:TRANSPARENT).

(defun option-pane-with-images-example-get-image (interface)
  (with-slots (image) interface
    (or image
        (setf image 
              (gp:load-image
               interface
               (gp:read-external-image 
                (lispworks-file
                 "examples/capi/choice/option-pane-images.bmp")
                :transparent-color-index 
                 '(7 . :transparent) 
                ))))))

;;; The :IMAGE-FUNCTION of the OPTION-PANE. 
;;; DATA is the item for which we want an image. 
;;; we create and cache a sub-image from the main image.
(defun option-pane-with-images-example-get-sub-image (interface data)
  (when-let (index (position data *the-items*))
    (with-slots (vec) interface
      (or (svref vec index)
          (setf (svref vec index)
                (let ((selected-image-x (* index 16))
                      (main-image (option-pane-with-images-example-get-image interface)))
                  (gp:make-sub-image interface
                                     main-image
                                     selected-image-x 0  ;;; X and Y 
                                     16 16)))))))

(defun test-option-pane-with-images ()
  (capi:display (make-instance 'option-pane-with-images-example)))
