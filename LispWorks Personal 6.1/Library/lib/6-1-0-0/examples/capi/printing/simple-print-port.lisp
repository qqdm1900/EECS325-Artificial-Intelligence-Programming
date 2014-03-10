;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/printing:simple-print-port.lisp,v 1.2.9.1 2011/08/24 13:26:21 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

;;; ----------------------------------------------------------------------
;;;
;;; This example demonstrates CAPI:SIMPLE-PRINT-PORT.
;;; To try it, compile and load this file and then execute:
;;;
;;;      (CL-USER::START-SIMPLE-PRINT-PORT-EXAMPLE)
;;;
;;; The display callback fills drawing-area with the color red.  When
;;; the "Print..." menu items is invoked, the whole of the pane is
;;; printed and so the page is also filled with the color red.
;;;
;;; ----------------------------------------------------------------------

(in-package "CL-USER")

(capi:define-interface simple-print-port-example ()
  ()
  (:panes (drawing-area capi:output-pane
                        :display-callback
                        #'(lambda (pane x y width height)
                            (gp:draw-rectangle pane x y width height
                                               :filled t
                                               :foreground :red))))
  (:layouts (default-layout capi:simple-layout
                            '(drawing-area)))
  (:menus (pane-menu "Pane"
                     (("Page Setup..."
                       :callback 'capi:page-setup-dialog
                       :callback-type :none)
                      ("Print..."
                       :callback 'capi:simple-print-port
                       :callback-type :data
                       :callback-data drawing-area
                       :accelerator "accelerator-p"))))
  (:menu-bar pane-menu)
  (:default-initargs :layout 'default-layout
   :title "Simple Print Port Example"
   ))

(defun start-simple-print-port-example ()
  (capi:display (make-instance 'simple-print-port-example)))


