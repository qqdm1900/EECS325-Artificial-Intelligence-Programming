;; -*- rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/graphics:image-access-alpha.lisp,v 1.1.2.1 2011/08/24 13:26:20 davef Exp $" -*-

;;----------------------------------------------------------------------------
;;
;; examples/capi/graphics/image-access-alpha.lisp
;;
;; This example demonstrates the uses of image-access objects
;; in Graphics Ports with colors that use an alpha component.
;;
;; To try it, compile and load this file and then execute:
;;
;;      (CL-USER::TEST-IMAGE-ACCESS-ALPHA)
;;
;; It draws 4 rows of rectangles with an image on top of a solid
;; background of green, blue and yellow.  The top two rows of
;; rectangles show a red gradient on top of background.  The bottom
;; two rows of rectangles show the same gradient in their top halves
;; and solid red in their bottom halves.  The images for the second
;; and fourth rows are computed by reading the row above and mirroring
;; it.
;;
;;----------------------------------------------------------------------------
;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
;;----------------------------------------------------------------------------


(in-package "CL-USER")


;;----------------------------------------------------------------------------
;; Define an interface
;;----------------------------------------------------------------------------

(capi:define-interface test-image-access-alpha ()
  ((images :initform nil))
  (:panes
   (viewer capi:output-pane
           :display-callback 'display-test-image-access-alpha
           :visible-min-width 200
           :visible-min-height 200))
  (:layouts
   (main-layout capi:column-layout
                '(viewer)))
  (:default-initargs
   :create-callback 'create-test-image-access-alpha
   :destroy-callback 'destroy-test-image-access-alpha
   :layout 'main-layout))

(defun create-test-image-access-alpha (interface)
  (with-slots (images viewer) interface
    (let* ((image-bgra (create-test-image-access-alpha-bgra
                        viewer))
           (image-bgra-mirror (create-test-image-access-alpha-bgra-mirror
                               viewer image-bgra))
           (image-direct (create-test-image-access-alpha-direct
                          viewer))
           (image-direct-mirror (create-test-image-access-alpha-direct-mirror
                                 viewer image-direct)))
      (setf images
            (list image-bgra
                  image-bgra-mirror
                  image-direct
                  image-direct-mirror)))))

(defun create-test-image-access-alpha-bgra (viewer)
  (let* ((image-bgra (gp:make-image viewer 32 32))
         (access (gp:make-image-access viewer image-bgra)))
    (unwind-protect
        (let* ((bgra-vector (make-array (* 32 32 4)
                                        :element-type '(unsigned-byte 8)))
               (bgra (make-array (list 32 32 4)
                                 :element-type '(unsigned-byte 8)
                                 :displaced-to bgra-vector)))
          (dotimes (y 32)
            (dotimes (x 32)
              (let ((alpha (round (* x (/ 255 31)))))
                (setf (aref bgra y x 0) 0)
                (setf (aref bgra y x 1) 0)
                (setf (aref bgra y x 2) alpha)
                (setf (aref bgra y x 3) alpha))))
          (gp:image-access-pixels-from-bgra access bgra-vector)
          (gp:image-access-transfer-to-image access))
      (gp:free-image-access access))
    image-bgra))

(defun create-test-image-access-alpha-bgra-mirror (viewer image-bgra)
  (let* ((image-bgra-mirror (gp:make-image viewer 32 32))
         (access (gp:make-image-access viewer image-bgra))
         (access-mirror (gp:make-image-access viewer image-bgra-mirror)))
    (unwind-protect
        (progn
          (gp:image-access-transfer-from-image access)
          (let* ((bgra-vector (make-array (* 32 32 4)
                                          :element-type '(unsigned-byte 8)))
                 (bgra (make-array (list 32 32 4)
                                   :element-type '(unsigned-byte 8)
                                   :displaced-to bgra-vector)))
            (gp:image-access-pixels-to-bgra access bgra-vector)
            (dotimes (y 32)
              (dotimes (x 16)
                (let ((mirror-x (- 31 x)))
                  (rotatef (aref bgra y x 0) (aref bgra y mirror-x 0))
                  (rotatef (aref bgra y x 1) (aref bgra y mirror-x 1))
                  (rotatef (aref bgra y x 2) (aref bgra y mirror-x 2))
                  (rotatef (aref bgra y x 3) (aref bgra y mirror-x 3)))))
            (gp:image-access-pixels-from-bgra access-mirror bgra-vector)
            (gp:image-access-transfer-to-image access-mirror)))
      (gp:free-image-access access)
      (gp:free-image-access access-mirror))
    image-bgra-mirror))

(defun create-test-image-access-alpha-direct (viewer)
  (let* ((image-direct (gp:make-image viewer 32 32))
         (access (gp:make-image-access viewer image-direct)))
    (unwind-protect
        (progn
          (dotimes (y 32)
            (dotimes (x 32)
              (let* ((alpha (if (< y 16) (/ x 31) nil))
                     (pixel (color:convert-color
                             viewer
                             (color:make-rgb (or alpha 1) 0 0 alpha))))
                (setf (gp:image-access-pixel access x y)
                      pixel))))
          (gp:image-access-transfer-to-image access))
      (gp:free-image-access access))
    image-direct))

(defun create-test-image-access-alpha-direct-mirror (viewer image-direct)
  (let* ((image-direct-mirror (gp:make-image viewer 32 32))
         (access (gp:make-image-access viewer image-direct))
         (access-mirror (gp:make-image-access viewer image-direct-mirror)))
    (unwind-protect
        (progn
          (gp:image-access-transfer-from-image access)
          (dotimes (y 32)
            (dotimes (x 16)
              (let* ((mirror-x (- 31 x))
                     (pixel1 (gp:image-access-pixel access x y))
                     (pixel2 (gp:image-access-pixel access mirror-x y)))
                (setf (gp:image-access-pixel access-mirror x y)
                      pixel2)
                (setf (gp:image-access-pixel access-mirror mirror-x y)
                      pixel1))))
          (gp:image-access-transfer-to-image access))
      (gp:free-image-access access)
      (gp:free-image-access access-mirror))
    image-direct-mirror))

(defun destroy-test-image-access-alpha (interface)
  (with-slots (viewer images) interface
    (dolist (image images)
      (gp:free-image viewer image))
    (setf images nil)))

(defun display-test-image-access-alpha (pane x y width height)
  (declare (ignore x y width height))
  (with-slots (images)
      (capi:top-level-interface pane)
    (loop for image in images
          for draw-y from 8 by 40
          do
          (loop for color in '(:green :blue :yellow)
                for draw-x from 8 by 40
                do
                (gp:draw-rectangle pane
                                   draw-x draw-y 32 32
                                   :filled t
                                   :foreground color)
                (when image
                  (gp:draw-image pane image draw-x draw-y))))))

;;----------------------------------------------------------------------------
;; The test function
;;----------------------------------------------------------------------------

(defun test-image-access-alpha ()
  (capi:display (make-instance 'test-image-access-alpha)))

