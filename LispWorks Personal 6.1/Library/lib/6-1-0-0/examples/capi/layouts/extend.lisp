;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/layouts:extend.lisp,v 1.1.5.1 2011/08/24 13:26:20 davef Exp $" -*-

;;----------------------------------------------------------------------------
;;
;; examples/capi/layouts/extend.lisp
;;
;; This example demonstrates the uses of :bottom-extend and :right-extend in
;; a CAPI grid layout.
;;
;; The example grid has three columns (a, b and c) and 6 rows (1...6).
;;
;; To try it, compile and load this file and then execute:
;;
;;      (CL-USER::TEST-EXTEND-LAYOUTS)
;;
;;----------------------------------------------------------------------------
;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
;;----------------------------------------------------------------------------

(in-package "CL-USER")

(capi:define-interface extend-layouts-test ()
  ()
  (:panes
   ;; Row 1 contains 3 separate panes.
   (pane1a
    capi:display-pane
    :text "Pane 1a"
    :visible-max-width nil)
   (pane1b
    capi:display-pane
    :text "Pane 1b"
    :visible-max-width nil)
   (pane1c
    capi:display-pane
    :text "Pane 1c"
    :visible-max-width nil)
   ;; Row 2 contains 1 pane across all columns.
   (pane2abc
    capi:display-pane
    :text "Pane 2abc"
    :visible-max-width nil)
   ;; Row 3 contains 2 panes, with columns a and b joined.
   (pane3ab
    capi:display-pane
    :text "Pane 3ab"
    :visible-max-width nil)
   (pane3c
    capi:display-pane
    :text "Pane 3c"
    :visible-max-width nil)
   ;; Row 4 contains 2 panes, with columns b and c joined.
   (pane4a
    capi:display-pane
    :text "Pane 4a"
    :visible-max-width nil)
   (pane4bc
    capi:display-pane
    :text "Pane 4bc"
    :visible-max-width nil)
   ;; Rows 5 and 6 are joined in column a
   (pane56a capi:display-pane
            :text '("Pane 56a")
            :visible-max-width nil
            :visible-max-height nil)
   (pane5b capi:display-pane
           :text '("Pane 5b")
           :visible-max-width nil
           :visible-max-height nil)
   (pane5c capi:display-pane
           :text '("Pane 5c")
           :visible-max-width nil
           :visible-max-height nil)
   (pane6b capi:display-pane
           :text "Pane 6b"
           :visible-max-width nil
           :visible-max-height nil)
   (pane6c capi:display-pane
           :text "Pane 6c"
           :visible-max-width nil
           :visible-max-height nil))
  (:layouts
   (main-layout
    capi:grid-layout
    '(pane1a pane1b pane1c
      pane2abc :right-extend :right-extend
      pane3ab :right-extend pane3c
      pane4a pane4bc :right-extend
      pane56a pane5b pane5c
      :bottom-extend pane6b pane6c)
    :columns 3))
  (:default-initargs
   :layout 'main-layout
   :title "Extend Layout Test"))

;;----------------------------------------------------------------------------
;; test-extend-layouts
;;----------------------------------------------------------------------------

(defun test-extend-layouts ()
  (capi:display (make-instance 'extend-layouts-test)))



