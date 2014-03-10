;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/elements:menu-with-images.lisp,v 1.3.1.1 2011/08/24 13:26:20 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

(in-package "CL-USER")

;;; An example of a menu with images. 

;;; To try it, compile and load this file and then execute:
;;;
;;;      (CL-USER::TEST-MENU-WITH-IMAGES)
;;;
;;; The menu gets images because it has a :IMAGE-FUNCTION.
;;; In the example below, the function takes a subimage of a 
;;; larger image, using GP:MAKE-SUB-IMAGE. The larger
;;; image is also displayed in an output-pane below. 


(capi:define-interface menu-with-images-example ()
  ((image :initform nil)
   (vec :initform (make-array 6)))
  (:menus 
   (menu-with-images
    "Menu with images"
    (("First string" :data 0) 
     ( "Another text" :data 1)
     ( "Foo" :data 2)
     ( "Bar" :data 3)
     ( "Baz" :data 4)
     ( "Quux" :data 5))
    :image-function #'(lambda (data)
                        (menu-with-images-example-get-sub-image capi:interface data))))
  (:panes
   (image-displayer
    capi:output-pane
    :visible-min-width 96  ;;; the size of the image
    :visible-min-height 16
    :background :lightcyan
    :display-callback #'(lambda (pane x y width height)
                          (declare (ignore x y width height))
                          (gp:draw-image pane 
                                         (menu-with-images-example-get-image
                                          capi:interface)
                                         0 0 ))))
  (:menu-bar menu-with-images)
  (:layouts
   (main-layout
    capi:column-layout
    '(image-displayer))))

;;; Return the main image. 
;;; The external image is in the LispWorks distribution.  It is a
;;; 96x16 image with a color table, with color 7 being the background.

(defun menu-with-images-example-get-image (interface)
  (with-slots (image) interface
    (or image
        (setf image 
              (gp:load-image
               interface
               (gp:read-external-image 
                (lispworks-file
                 "examples/capi/elements/images/toolbar-radio-images.bmp")
                :transparent-color-index '(7 . :transparent) ))))))

;;; Return a subimage.
;;; Index here will be the data 
(defun menu-with-images-example-get-sub-image (interface index)
  (with-slots (vec) interface
    (or (svref vec index)
        (setf (svref vec index)
              (gp:make-sub-image interface
                                 (menu-with-images-example-get-image interface)
                                 (* index 16) 0  ; X and Y 
                                 16 16)))))

;;; In some distributions of GTK the default is off for some reason. 

(defun check-menus-images ()
  #+capi-gtk-lib
  (when (eq (capi:default-library) :gtk)
    (let ((screen (capi:convert-to-screen)))
      (unless (capi-gtk-library:screen-settings-value screen "gtk-menu-images")
        (when (capi:prompt-for-confirmation "Images in menus are switched off. Switch it on?")
          (capi-gtk-library:set-screen-settings-value screen "gtk-menu-images" t))))))


(defun test-menu-with-images ()
  (check-menus-images)
                                          
  (capi:display (make-instance 'menu-with-images-example)))
