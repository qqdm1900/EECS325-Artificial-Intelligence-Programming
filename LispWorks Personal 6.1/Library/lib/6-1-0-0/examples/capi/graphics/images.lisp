;; -*- rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/graphics:images.lisp,v 1.5.9.1 2011/08/24 13:26:21 davef Exp $" -*-

;;----------------------------------------------------------------------------
;;
;; examples/capi/graphics/images.lisp
;;
;; This example demonstrates the uses of images in Graphics Ports.
;;   GP:READ-EXTERNAL-IMAGE to read an external-image from a file.
;;   GP:LOAD-IMAGE to convert an external-image to an image for drawing.
;;   GP:DRAW-IMAGE for drawing an image to an output-pane.
;;   GP:FREE-IMAGE for freeing an image.
;; Note that there is no need to free the image when the interface is
;; destroyed because the CAPI does this automatically.
;;
;; To try it, compile and load this file and then execute:
;;
;;      (CL-USER::TEST-IMAGE-FUNCTIONS)
;;
;;----------------------------------------------------------------------------
;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
;;----------------------------------------------------------------------------


(in-package "CL-USER")


;;----------------------------------------------------------------------------
;; Define an interface
;;----------------------------------------------------------------------------

(capi:define-interface test-image-functions ()
  ((image :initform nil))
  (:panes
   (viewer capi:output-pane
           :display-callback 'display-test-image-functions
           :horizontal-scroll t
           :vertical-scroll t
           :visible-min-width 500
           :visible-min-height 375)
   (controller capi:push-button-panel
               :items '(:change... :close)
               :callbacks '(test-image-functions-change-image
                            test-image-functions-close-image)
               :callback-type :interface
               :print-function 'string-capitalize))
  (:layouts
   (main-layout capi:column-layout
                '(viewer controller)))
  (:default-initargs
   :layout 'main-layout
   :best-width 200
   :best-height 200))

(defmethod initialize-instance :after ((self test-image-functions) &key
                                       &allow-other-keys)
  (update-test-image-functions-enabled self))

(defun update-test-image-functions (interface)
  (with-slots (viewer image) interface
    (gp:invalidate-rectangle viewer)
    (capi:set-horizontal-scroll-parameters viewer :min-range 0 :max-range (if image (gp:image-width image) 0))
    (capi:set-vertical-scroll-parameters viewer :min-range 0 :max-range (if image (gp:image-height image) 0))
    (update-test-image-functions-enabled interface)))

(defun update-test-image-functions-enabled (interface)
  (with-slots (controller image) interface
    (if image
        (capi:set-button-panel-enabled-items
         controller
         :set t)
      (capi:set-button-panel-enabled-items
       controller
       :set t
       :disable '(:close)))))

(defun display-test-image-functions (pane x y width height)
  (with-slots (image) (capi:top-level-interface pane)
    (when image
      (when (gp:rectangle-overlap x y (+ x width) (+ y height)
                                  0 0 (gp:image-width image) (gp:image-height image))
        (gp:draw-image pane image 0 0)))))

(defvar *image-file-filters* '("Bitmap" "*.bmp;*.dib" 
                               "GIF"    "*.gif"  
                               "JPEG"   "*.jpg;*.jpeg" 
                               "PNG"    "*.png" 
                               "TIFF"   "*.tif;*.tiff"))

(defvar *image-file-types* '("bmp" "dib" "gif" "jpg" "jpeg" "png" "tif" "tiff"))

(defun test-image-functions-change-image (interface)
  (with-slots (viewer image) interface
    (let ((file (capi:prompt-for-file "Choose a bitmap"
                                      :pathname (pathname-location #.(current-pathname))
                                      :filter (second *image-file-filters*)
                                      :filters *image-file-filters*
                                      :ok-check #'(lambda (file)
                                                    (member (pathname-type file) *image-file-types* :test 'equalp)))))
      (when (and file (probe-file file))
        (let ((external-image (gp:read-external-image file)))
          (when image
            (gp:free-image viewer image))
          (setf image (gp:load-image viewer external-image))
          (update-test-image-functions interface))))))

(defun test-image-functions-close-image (interface)
  (with-slots (viewer image) interface
    (gp:free-image viewer (shiftf image nil))
    (update-test-image-functions interface)))


;;----------------------------------------------------------------------------
;; The test function
;;----------------------------------------------------------------------------

(defun test-image-functions ()
  (capi:display (make-instance 'test-image-functions)))

