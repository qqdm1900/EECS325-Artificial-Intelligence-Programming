;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/printing:fit-to-page.lisp,v 1.3.9.1 2011/08/24 13:26:20 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

;;; ----------------------------------------------------------------------
;;;
;;; This example demonstrates how to use the printer API to fit a
;;; given drawing to the size of the paper.
;;; To try it, compile and load this file and then execute:
;;;
;;;      (CL-USER::START-FIT-TO-PAGE-EXAMPLE)
;;;
;;; ----------------------------------------------------------------------

(in-package "CL-USER")

(defvar *fit-to-page-drawing-sizes*
  '((200 100)
    (100 200)
    (2000 1500)
    (1500 2000)))

(capi:define-interface fit-to-page-example ()
  ((drawing-width :accessor drawing-width)
   (drawing-height :accessor drawing-height))
  (:panes (drawing-area capi:output-pane
                        :display-callback
                        #'(lambda (pane x y width height)
                            (declare (ignore x y width height))
                            (draw-fit-to-page-example
                             (capi:element-interface pane)
                             pane))
                        :horizontal-scroll t
                        :vertical-scroll t))
  (:layouts (drawing-layout capi:simple-layout
                            '(drawing-area)))
  (:menus (pane-menu "Pane"
                     (("Print to fit..."
                       :callback 'print-fit-to-page-example
                       :callback-type :interface)
                      pane-size-component))
          (pane-size-component :component
                               ()
                               :interaction :single-selection
                               :selection-callback 'reset-drawing-size
                               :callback-type :interface
                               ))
  (:menu-bar pane-menu)
  (:default-initargs :layout 'drawing-layout
   :best-width 300
   :best-height 300
   :Title "Fit To Page Example"
   ))

(defmethod initialize-instance :after ((self fit-to-page-example)
                                       &key &allow-other-keys)
  (with-slots (pane-size-component) self
    (setf (capi:collection-items pane-size-component)
          (loop for size in *fit-to-page-drawing-sizes*
                collect
                (make-instance 'capi:menu-item
                               :data size
                               :print-function #'(lambda (size)
                                                   (format nil "~Dx~D"
                                                           (first size)
                                                           (second size))))))
    (reset-drawing-size self)))

(defun reset-drawing-size (interface)
  (with-slots (drawing-area pane-size-component) interface
    (let* ((size (capi:item-data (capi:choice-selected-item pane-size-component)))
           (width (first size))
           (height (second size)))
      (setf (drawing-width interface) width
            (drawing-height interface) height)
      (capi:set-horizontal-scroll-parameters drawing-area :max-range width)
      (capi:set-vertical-scroll-parameters drawing-area :max-range height)
      (gp:invalidate-rectangle drawing-area))))

(defun start-fit-to-page-example ()
  (capi:display (make-instance 'fit-to-page-example)))

(defun draw-fit-to-page-example (interface pane)
  (let* ((drawing-width (drawing-width interface))
         (drawing-height (drawing-height interface))
         (third-width (/ drawing-width 3))
         (third-height (/ drawing-height 3)))
    (gp:draw-rectangle pane 0 0 third-width third-height)
    (gp:draw-rectangle pane
                       (- drawing-width third-width)
                       0
                       third-width third-height)
    (gp:draw-rectangle pane
                       (- drawing-width third-width)
                       (- drawing-height third-height)
                       third-width third-height)
    (gp:draw-rectangle pane
                       0
                       (- drawing-height third-height)
                       third-width third-height)
    (gp:draw-rectangle pane
                       (/ drawing-width 4)
                       (/ drawing-height 4)
                       (/ drawing-width 2)
                       (/ drawing-height 2))))

(defun print-fit-to-page-example (interface)
  ;; Setup the printer for a 1 page document
  (let ((printer (capi:print-dialog :print-pages-p nil
                                    :print-copies-p t)))
    (when printer
      (capi:with-print-job (printer-port :printer printer)
        (multiple-value-bind
            (page-width page-height)
            (capi:get-page-area printer)
          ;; Compute a transform which will fit the best dimension
          ;; to the paper size dimension, whilst preserving the
          ;; aspect ratio.
          (let* ((drawing-width (1+ (drawing-width interface)))
                 (drawing-height (1+ (drawing-height interface)))
                 (widen-p (> (/ page-width page-height)
                             (/ drawing-width drawing-height)))
                 (page-transform-x 0)
                 (page-transform-y 0)
                 (page-transform-width (if widen-p
                                           (* page-width
                                              (/ drawing-height page-height))
                                         drawing-width))
                 (page-transform-height (if widen-p
                                            drawing-height
                                          (* page-height
                                             (/ drawing-width page-width)))))
            (capi:with-document-pages (page 1 1) ; all on one page
              (capi:with-page-transform (page-transform-x
                                         page-transform-y
                                         page-transform-width
                                         page-transform-height)
                (draw-fit-to-page-example interface printer-port)))))))))
