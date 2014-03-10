;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/layouts:titles-in-grid.lisp,v 1.1.10.1 2011/08/24 13:26:20 davef Exp $" -*-

;;----------------------------------------------------------------------------
;;
;; examples/capi/layouts/titles-in-grid.lisp
;;
;; This example demonstrates the use of the :HAS-TITLE-COLUMN-P initarg
;; to CAPI:GRID-LAYOUT to form a column of titles.
;; Note that mnemonics are not supported on all platforms.
;;
;; To try it, compile and load this file and then execute:
;;
;;      (CL-USER::TEST-TITLES-IN-GRID)
;;
;;----------------------------------------------------------------------------
;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
;;----------------------------------------------------------------------------


(in-package "CL-USER")

(capi:define-interface titles-in-grid-test ()
  ()
  (:panes
   (text-pane1
    capi:text-input-pane
    :text "Initial text 1")
   (text-pane2
    capi:text-input-pane
    :text "Initial text 2")
   (option
    capi:option-pane
    :items '(3 33 333))
   (info
    capi:title-pane
    :text "This is just a label."))
  (:layouts
   (main-layout
    capi:grid-layout
    '("" info
      "Enter text:" text-pane1
      (:title "Enter more text:") text-pane2
      (:mnemonic-title "O&ption 3:") option)
    :has-title-column-p t))
  (:default-initargs
   :layout 'main-layout
   :title "Titled In Grid Test"
   :best-width 400))

;;----------------------------------------------------------------------------
;; test-titles-in-grid
;;----------------------------------------------------------------------------

(defun test-titles-in-grid ()
  (capi:display (make-instance 'titles-in-grid-test)))

