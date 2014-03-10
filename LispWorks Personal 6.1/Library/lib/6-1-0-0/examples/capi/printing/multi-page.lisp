;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/printing:multi-page.lisp,v 1.2.3.1 2011/08/24 13:26:21 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

;;; ----------------------------------------------------------------------
;;;
;;; This example demonstrates how to use the printer API to print
;;; multiple pages from a drawing.  This is similar to what
;;; CAPI:SIMPLE-PRINT-PORT does but can be customized to add headers
;;; and footers etc.
;;;
;;; To try it, compile and load this file and then execute:
;;;
;;;      (CL-USER::START-MULTI-PAGE-EXAMPLE)
;;;
;;; ----------------------------------------------------------------------

(in-package "CL-USER")

(capi:define-interface multi-page-example ()
  ((drawing-width :accessor drawing-width)
   (drawing-height :accessor drawing-height))
  (:panes (drawing-area capi:output-pane
                        :scroll-width 2000
                        :scroll-height 2000
                        :horizontal-scroll t
                        :vertical-scroll t
                        :display-callback
                        #'(lambda (pane x y width height)
                            (draw-multi-page-example pane x y width height))))
  (:layouts (drawing-layout capi:simple-layout
                            '(drawing-area)))
  (:menus (pane-menu "Pane"
                     (("Page Setup..."
                       :callback 'capi:page-setup-dialog
                       :callback-type :none)
                      ("Print..."
                       :callback 'print-multi-page-example
                       :callback-type :interface
                       :accelerator "accelerator-p"))))
  (:menu-bar pane-menu)
  (:default-initargs :layout 'drawing-layout
   :best-width 300
   :best-height 300
   :Title "Multi Page Example"
   ))

(defun start-multi-page-example ()
  (capi:display (make-instance 'multi-page-example)))

(defun draw-multi-page-example (pane x y width height &optional annotatep)
  (let ((x2 (+ x width))
        (y2 (+ y height))
        (annotation '()))
    (when (gp:rectangle-overlap x y x2 y2 0 0 1500 1500)
      (gp:draw-rectangle pane 0 0 1500 1500)
      (when annotatep
        (push "rectangle" annotation)))
    (when (gp:rectangle-overlap x y x2 y2 0 0 500 500)
      (gp:draw-line pane 0 0 500 500)
      (when annotatep
        (push "line" annotation)))
    (when (gp:rectangle-overlap x y x2 y2 1500 0 2000 1500)
      (gp:draw-ellipse pane 1750 750 249 750)
      (when annotatep
        (push "ellipse" annotation)))
    (when (and (gp:rectangle-overlap x y x2 y2 0 1500 2000 2000)
               ;; Skip if the printed rectangle is below the actual
               ;; edge of the diamond.
               (not (< (+ x2 5) (* (- y 1750) 1000/250)))
               (not (> (- x 5) (- 2000 (* (- y 1750) 1000/250))))
               )
      (gp:draw-polygon pane '(0 1750
                                1000 1500
                                1999 1750
                                1000 1999
                                0 1750))
      (when annotatep
        (push "diamond" annotation)))
    (when annotatep
      (gp:draw-string pane
                      (format nil "~{~A~^ + ~}"
                              (or annotation '("nothing")))
                      (+ x 10)
                      (+ y 10 (gp:get-font-ascent pane))))))

(defun print-multi-page-example (interface &optional (scale 1))
  (let ((printer (capi:print-dialog :print-pages-p nil
                                    :print-copies-p t)))
    (when printer
      (let ((pane (slot-value interface 'drawing-area)))
        (capi:with-print-job (printer-pane :pane pane :printer printer)
          (multiple-value-bind
              (page-width page-height)
              (capi:get-page-area printer :scale scale)
            (let ((canvas-width (capi:get-horizontal-scroll-parameters
                                 pane
                                 :max-range))
                  (canvas-height (capi:get-vertical-scroll-parameters
                                  pane
                                  :max-range)))
              (let* ((x-pages (ceiling canvas-width page-width))
                     (y-pages (ceiling canvas-height page-height))
                     (page-count (* x-pages y-pages)))
                (capi:with-document-pages (page 1 page-count)
                  (multiple-value-bind
                      (row column)
                      (floor (1- page) x-pages)
                    (let* ((page-x (* column page-width))
                           (page-y (* row page-height))
                           (draw-width (if (> (+ page-x page-width) canvas-width) 
                                           (- canvas-width page-x)
                                         page-width))
                           (draw-height (if (> (+ page-y page-height) canvas-height) 
                                            (- canvas-height page-y)
                                          page-height)))
                      (capi:with-page-transform (page-x page-y page-width page-height)
                        (draw-multi-page-example pane
                                                 page-x page-y
                                                 draw-width draw-height
                                                 t)))))))))))))

