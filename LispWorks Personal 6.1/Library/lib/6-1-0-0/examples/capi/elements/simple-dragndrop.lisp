;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/elements:simple-dragndrop.lisp,v 1.2.1.2 2011/11/04 20:11:32 davef Exp $" -*-

;;----------------------------------------------------------------------------
;;
;; examples/capi/elements/simple-dragndrop.lisp
;;
;; This example demonstrates minimal dragging/dropping of strings
;; from/into a pane (CAPI:TREE-VIEW here), using the simple (list)
;; drag-callback.
;;
;; To try it, compile and load this file and then execute:

;;      (CL-USER::TEST-SIMPLE-DRAGNDROP)

;; This creates two windows, String Dragging source and String
;; Dragging target. The source has a drag-callback, so you can drag
;; items from it. The target has drop-callback, so you drop into it.
;; You can drag-and-drop between the two windows, or try with other
;; applications.
;;
;; See the documentation of CAPI:SIMPLE-PANE for details. 
;;----------------------------------------------------------------------------
;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
;;----------------------------------------------------------------------------

(in-package "CL-USER")

;; The drag callback. Would work on any CAPI:COLLECTION. 

(defun collection-drag-callback (pane indices)
  (let ((string (funcall (capi:collection-print-function pane)
                         (capi:get-collection-item  pane (first indices)))))
    (list :string string)))



;;; The drop-stage-callback. Called when the user actually drops. 

(defun act-on-drop (tree-view  new-string  index placement)
  (declare (ignore placement))
  (let ((item (capi:get-collection-item tree-view index)))
    (setf (cdr item)
          (nconc (cdr item) (list (list new-string))))
    (capi:tree-view-update-item tree-view item nil)))


(defun TEST-SIMPLE-DRAGNDROP ()
  (capi:contain
   (make-instance 'capi:tree-view
                  :roots (copy-tree '(("First item" ("Sub-item")) ("Second item") ))
                  :children-function 'cdr
                  :print-function 'car
                  :drop-callback '(:copy :string act-on-drop))
   :title "String Dragging target")

  (capi:contain
   (make-instance 'capi:tree-view
                  :roots (copy-tree '(("Source first root" ("Source sub-item")) ("Source second root") ))
                  :children-function 'cdr
                  :print-function 'car
                  :drag-callback 'collection-drag-callback)
   :title "String Dragging source"))
