;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/printing:page-on-demand.lisp,v 1.5.2.1 2011/08/24 13:26:20 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

;;; ----------------------------------------------------------------------
;;;
;;; This example demonstrates basic printer API usage with page on
;;; demand printing, with separate parts of a pane on separate pages.
;;; To try it, compile and load this file and then execute:
;;;
;;;      (CL-USER::START-WITH-PRINT-JOB-EXAMPLE)
;;;
;;; On the screen, a red square is displayed with a green rectangle on
;;; its right.  When the "Print on 3 pages..." menu is invoked, a
;;; document of three pages is printed, with the red square on page 1,
;;; the green rectangle on page 2 and the green rectangle again on page 3
;;; together with a blue rectangle that is not on the screen.
;;;
;;; ----------------------------------------------------------------------

(in-package "CL-USER")

(capi:define-interface with-print-job-example ()
  ()
  (:panes (drawing-area capi:output-pane
                        :display-callback
                        #'(lambda (pane x y width height)
                            (declare (ignore x y width height))
                            (draw-red-square pane)
                            (draw-green-rectangle pane))
                        :visible-min-width 300
                        :visible-min-height 100))
  (:layouts (drawing-layout capi:simple-layout
                            '(drawing-area)))
  (:menus (pane-menu "Pane"
                     (("Print on 3 pages..."
                       :callback 'print-with-print-job-example-on-3-pages
                       :callback-type :interface))))
  (:menu-bar pane-menu)
  (:default-initargs :layout 'drawing-layout
   :title "With Print Job Example"))

(defun start-with-print-job-example ()
  (capi:display (make-instance 'with-print-job-example)))

(defun draw-red-square (pane)
  (gp:draw-rectangle pane 0 0 100 100
                     :filled t
                     :foreground :red))

(defun draw-green-rectangle (pane)
  (gp:draw-rectangle pane 100 0 200 100
                     :filled t
                     :foreground :green))

(defun print-with-print-job-example-on-3-pages (self)
  (declare (ignore self))
  ;; Setup the printer for a 3 page document
  (let ((printer (capi:print-dialog :print-pages-p t
                                    :first-page 1
                                    :last-page 3
                                    :print-copies-p t)))
    (when printer
      (capi:with-print-job (printer-port :printer printer)
        (multiple-value-bind (page-dx page-dy)
            (capi:get-page-area printer)
          (capi:with-document-pages (page 1 3) ; print pages 1 thru 3
            (case page
              ;; When told to print page 1, draw the red square.
              (1 (capi:with-page-transform (0 0 page-dx page-dy)
                   (draw-red-square printer-port)))
              ;; When told to print page 2, draw the green rectangle.
              (2 (capi:with-page-transform (100 0 page-dx page-dy)
                   (draw-green-rectangle printer-port)))
              ;; When told to print page 3, draw the green rectangle and also
              ;; a blue rectangle touching its bottom-right corner.
              (3 (capi:with-page-transform (100 0 page-dx page-dy)
                   (draw-green-rectangle printer-port))
                 (capi:with-page-transform (0 400 page-dx page-dy)
                   (gp:draw-rectangle printer-port 200 500 200 100
                                      :filled t
                                      :foreground :blue))))))))))
