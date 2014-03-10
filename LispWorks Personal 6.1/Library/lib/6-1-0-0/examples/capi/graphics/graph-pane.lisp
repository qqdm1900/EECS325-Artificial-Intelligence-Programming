;; -*- rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/graphics:graph-pane.lisp,v 1.3.11.1 2011/08/24 13:26:21 davef Exp $" -*-

;;----------------------------------------------------------------------------
;;
;; examples/capi/elements/graph-pane.lisp
;;
;; This example demonstrates the uses of graph-panes in the CAPI.
;; To try it, compile and load this file and then execute:
;;
;;      (CL-USER::TEST-GRAPH-PANE)
;;
;;----------------------------------------------------------------------------
;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
;;----------------------------------------------------------------------------


(in-package "CL-USER")


(capi:define-interface graph-pane-test ()
  ()
  (:panes
   (no-selection-graph-pane
    capi:graph-pane
    :title "No Selection:"
    :interaction :no-selection
    :roots '(1)
    :children-function 'children-function
    :callback-type :data-interface
    :selection-callback 'graph-pane-selection-callback
    :action-callback    'graph-pane-action-callback
    :extend-callback    'graph-pane-extend-callback
    :retract-callback   'graph-pane-retract-callback)
   (single-selection-graph-pane
    capi:graph-pane
    :title "Single Selection:"
    :interaction :single-selection
    :roots '(1)
    :children-function 'children-function
    :callback-type :data-interface
    :selection-callback 'graph-pane-selection-callback
    :action-callback    'graph-pane-action-callback
    :extend-callback    'graph-pane-extend-callback
    :retract-callback   'graph-pane-retract-callback)
   (multiple-selection-graph-pane
    capi:graph-pane
    :title "Multiple Selection:"
    :interaction :multiple-selection
    :roots '(1)
    :children-function 'children-function
    :callback-type :data-interface
    :selection-callback 'graph-pane-selection-callback
    :action-callback    'graph-pane-action-callback
    :extend-callback    'graph-pane-extend-callback
    :retract-callback   'graph-pane-retract-callback)
   (extended-selection-graph-pane
    capi:graph-pane
    :title "Extended Selection:"
    :interaction :extended-selection
    :roots '(1)
    :children-function 'children-function
    :callback-type :data-interface
    :selection-callback 'graph-pane-selection-callback
    :action-callback    'graph-pane-action-callback
    :extend-callback    'graph-pane-extend-callback
    :retract-callback   'graph-pane-retract-callback)
   (advanced-graph-pane
    capi:graph-pane
    :interaction :no-selection
    :title "Advanced Graph:"
    :roots '(1)
    :children-function 'children-function
    :node-pane-function 'make-pane-for-node
    :callback-type :data-interface
    :selection-callback 'graph-pane-selection-callback
    :action-callback    'graph-pane-action-callback
    :extend-callback    'graph-pane-extend-callback
    :retract-callback   'graph-pane-retract-callback)
   (collector
    capi:collector-pane))
  (:layouts
   (row
    capi:row-layout
    '(no-selection-graph-pane single-selection-graph-pane
			     multiple-selection-graph-pane
			     extended-selection-graph-pane))
   (main-layout
    capi:column-layout
    '(row advanced-graph-pane collector)))
  (:default-initargs
   :layout 'main-layout
   :title "Graph Pane Test"
   :best-width 700
   :best-height 550))


;;----------------------------------------------------------------------------
;; A generic callback
;;----------------------------------------------------------------------------

(defun graph-pane-callback (type data interface)
  (with-slots (collector) interface
    (format (capi:collector-pane-stream collector)
            "~S: ~A ~S ~%"
            (capi:interface-title interface) type data)))

(defun graph-pane-selection-callback (&rest args)
  (apply 'graph-pane-callback "selected" args))

(defun graph-pane-action-callback (&rest args)
  (apply 'graph-pane-callback "action on" args))

(defun graph-pane-extend-callback (&rest args)
  (apply 'graph-pane-callback "extended" args))

(defun graph-pane-retract-callback (&rest args)
  (apply 'graph-pane-callback "retract" args))


;;----------------------------------------------------------------------------
;; Graph default routines
;;----------------------------------------------------------------------------

(defun make-pane-for-node (graph-pane node)
  (declare (ignore graph-pane))
  (cond
   ((< node 4)
    (make-instance 'capi:push-button :data node))
   ((< node 8)
    (make-instance 'capi:check-button :data node))
   ((eq (mod node 2) 1)
    (make-instance 'capi:text-input-pane :text (princ-to-string node)))
   (t
    (make-instance 'capi:item-pinboard-object :data node))))

(defun children-function (x)
  (when (< x 8)
    (list (* x 2) (1+ (* x 2)))))


;;----------------------------------------------------------------------------
;; The test function
;;----------------------------------------------------------------------------

(defun test-graph-pane ()
  (capi:display (make-instance 'graph-pane-test)))
