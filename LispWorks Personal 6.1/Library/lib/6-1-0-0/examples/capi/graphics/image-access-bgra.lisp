;; -*- rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/graphics:image-access-bgra.lisp,v 1.1.9.1 2011/08/24 13:26:20 davef Exp $" -*-

;;----------------------------------------------------------------------------
;;
;; examples/capi/graphics/image-access-bgra.lisp
;;
;; This example demonstrates the uses of image-access objects
;; in Graphics Ports to obtain BGRA color data from an image.
;;
;; To try it, compile and load this file and then execute:
;;
;;      (CL-USER::TEST-IMAGE-ACCESS-BGRA)
;; Click Grab to take a copy of the top part of the image.
;; Click Show to see its details.
;; Click Mirror to see it being modified.
;;
;;----------------------------------------------------------------------------
;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
;;----------------------------------------------------------------------------


(in-package "CL-USER")


;;----------------------------------------------------------------------------
;; Define an interface
;;----------------------------------------------------------------------------

(capi:define-interface test-image-access-bgra ()
  ((image :initform nil))
  (:panes
   (viewer capi:output-pane
           :display-callback 'display-test-image-access-bgra
           :visible-min-width 200
           :visible-min-height 200)
   (controller capi:push-button-panel
               :items '(:grab :show :mirror)
               :callbacks '(test-image-access-bgra-grab
                            test-image-access-bgra-show
                            test-image-access-bgra-mirror)
               :callback-type :interface
               :print-function 'string-capitalize))
  (:layouts
   (main-layout capi:column-layout
                '(viewer controller)))
  (:default-initargs
   :destroy-callback 'destroy-test-image-access-bgra
   :layout 'main-layout))

(defmethod initialize-instance :after ((self test-image-access-bgra) &key
                                       &allow-other-keys)
  (update-test-image-access-bgra-enabled self))

(defun update-test-image-access-bgra-enabled (interface)
  (with-slots (controller image) interface
    (if image
        (capi:set-button-panel-enabled-items
         controller
         :set t)
      (capi:set-button-panel-enabled-items
       controller
       :set t
       :disable '(:show :mirror)))))

(defun destroy-test-image-access-bgra (interface)
  (with-slots (viewer image) interface
    (when image
      (gp:free-image viewer image)
      (setf image nil)
      (update-test-image-access-bgra-enabled interface))))

(defun display-test-image-access-bgra (pane x y width height)
  (with-slots (image) (capi:top-level-interface pane)
    (let ((x2 (+ x width))
          (y2 (+ y height)))
      (when (gp:rectangle-overlap 0 0 50 50 x y x2 y2)
        (gp:draw-circle pane 25 25 25)
        (gp:draw-line pane 0 0 25 25))
      (when (and image (gp:rectangle-overlap 0 100 50 130 x y x2 y2))
        (gp:draw-image pane image 0 100)))))

(defun test-image-access-bgra-grab (interface)
  (with-slots (viewer image) interface
    (destroy-test-image-access-bgra interface)
    (setf image (gp:make-image-from-port viewer 0 0 50 30))
    (gp:invalidate-rectangle viewer 0 100 50 130)
    (update-test-image-access-bgra-enabled interface)))

(defun test-image-access-bgra-show (interface)
  (with-slots (viewer image) interface
    (let ((string (convert-test-image-access-bgra-to-string viewer image)))
      (capi:popup-confirmer
       (make-instance 'capi:display-pane
                      :text string
                      :font
                      (gp:make-font-description
                       :stock :system-fixed-font))
       nil
       :cancel-button nil))))

(defun convert-test-image-access-bgra-to-string (viewer image)
  (let ((access (gp:make-image-access viewer image)))
    (unwind-protect
        (let ((pixel-chars (copy-tree '((nil . #\X)
                                        (nil . #\.)
                                        (nil . #\O)
                                        (nil . #\#)
                                        (nil . #\-)))))
          (gp:image-access-transfer-from-image access)
          (let* ((height (gp:image-access-height access))
                 (width (gp:image-access-width access))
                 (bgra-vector (make-array (* height width 4)
                                          :element-type '(unsigned-byte 8)))
                 (bgra (make-array (list height width 4)
                                   :element-type '(unsigned-byte 8)
                                   :displaced-to bgra-vector)))
            (gp:image-access-pixels-to-bgra access bgra-vector)
            (with-output-to-string (char-view)
              (dotimes (y height)
                (dotimes (x width)
                  (let* ((pixel (list (aref bgra y x 0)
                                      (aref bgra y x 1)
                                      (aref bgra y x 2)
                                      (aref bgra y x 3)))
                         (char (assoc pixel pixel-chars :test 'equal)))
                    (unless char
                      (when (setq char (assoc nil pixel-chars))
                        (setf (car char) pixel)))
                    (write-char (or (cdr char) #\?) char-view)))
                (terpri char-view)))))
      (gp:free-image-access access))))

(defun test-image-access-bgra-mirror (interface)
  (with-slots (viewer image) interface
    (let ((access (gp:make-image-access viewer image)))
      (unwind-protect
          (progn
            (gp:image-access-transfer-from-image access)
            (let* ((height (gp:image-access-height access))
                   (width (gp:image-access-width access))
                   (bgra-vector (make-array (* height width 4)
                                            :element-type '(unsigned-byte 8)))
                   (bgra (make-array (list height width 4)
                                     :element-type '(unsigned-byte 8)
                                     :displaced-to bgra-vector)))
              (gp:image-access-pixels-to-bgra access bgra-vector)
              (dotimes (y height)
                (dotimes (x (floor width 2))
                  (let ((mirror-x (- (1- width) x)))
                    (rotatef (aref bgra y x 0) (aref bgra y mirror-x 0))
                    (rotatef (aref bgra y x 1) (aref bgra y mirror-x 1))
                    (rotatef (aref bgra y x 2) (aref bgra y mirror-x 2))
                    (rotatef (aref bgra y x 3) (aref bgra y mirror-x 3)))))
              (gp:image-access-pixels-from-bgra access bgra-vector))
            (gp:image-access-transfer-to-image access))
        (gp:free-image-access access)))
    (gp:invalidate-rectangle viewer 0 100 50 130)))

;;----------------------------------------------------------------------------
;; The test function
;;----------------------------------------------------------------------------

(defun test-image-access-bgra ()
  (capi:display (make-instance 'test-image-access-bgra)))

