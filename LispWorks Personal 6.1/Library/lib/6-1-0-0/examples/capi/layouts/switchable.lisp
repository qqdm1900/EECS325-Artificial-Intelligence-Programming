;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/layouts:switchable.lisp,v 1.3.1.1 2011/08/24 13:26:20 davef Exp $" -*-

;;----------------------------------------------------------------------------
;;
;; examples/capi/layouts/switchable.lisp
;;
;; This example demonstrates the uses of switchable layouts in the CAPI.
;; Note that in many cases it is better to use a TAB-LAYOUT where the user might
;; want to switch between parts of an interface.
;;
;; To try it, compile and load this file and then execute one of these:
;;
;;      (CL-USER::TEST-SWITCHABLE-LAYOUTS :height-control nil)
;;      (CL-USER::TEST-SWITCHABLE-LAYOUTS :height-control T)
;;      (CL-USER::TEST-SWITCHABLE-LAYOUTS :height-control :COMBINE)
;;
;; Press on the buttons at the bottom to switch what is displayed inside
;; the switchable area. The panes in the switchable area itself do nothing. 

;; The argument to CL-USER::TEST-SWITCHABLE-LAYOUTS define the height-control:
;;   * If the argument is  NIL only the minimum size is controlled. 
;;     The switchable area grows as needed, but doesn't shrink on its own. 
;;   * If the argument is T, the height of the swithable area is controlled
;;     by its own contents. It goes up and down as needed. This is done
;;     by passing :VISIBLE-MAX-HEIGHT T to the switchable-layout. 
;;   * If the argument is :COMBINED, the height is fixed to the height of the
;;     highest child (the "All Panes" child). This is done by passing 
;;     :COMBINE-CHILD-CONSTRAINTS T to the switchable-layout.

;;----------------------------------------------------------------------------
;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
;;----------------------------------------------------------------------------


(in-package "CL-USER")

(capi:define-interface switchable-layouts-test ()
  ()
  (:panes
   (pane1
    capi:text-input-pane
    :text "Pane 1 initial text")
   (pane2
    capi:push-button
    :text "Pane 2")
   (pane3
    capi:option-pane
    :items '(3 33 333))
   (info
    capi:title-pane)
   (controls
    capi:push-button-panel
    :items '(pane1 pane2 pane3 all-panes nothing)
    ;; Mnemonics for Microsoft Windows -- other platforms do not support them.
    :mnemonics '(#\1 #\2 #\3 #\A #\N)
    :selection-callback 'set-switchable-layouts-test-pane
    :print-function 'string-capitalize
    :callback-type :interface-data))
  (:layouts
   (all-panes
    capi:column-layout
    '(pane1 pane2 pane3))
   (switchable
    capi:switchable-layout
    '(pane1 pane2 pane3 all-panes)
    :visible-child 'pane1
    :title "Switchable area"
    :title-position :frame
    :visible-max-width nil
    :visible-max-height (:initarg :switchable-max-height)
    :combine-child-constraints (:initarg :combine-child-constraints)
    )
   (main-layout
    capi:column-layout
    '(switchable controls info)))
  (:default-initargs
   :layout 'main-layout
   :title "Switchable Layout Test"
   :best-height 150))

(defmethod initialize-instance :after ((self switchable-layouts-test)
                                       &key)
  (update-switchable-layouts-test-info self))


;;----------------------------------------------------------------------------
;; callbacks
;;----------------------------------------------------------------------------

(defun set-switchable-layouts-test-pane (interface pane)
  (with-slots (switchable) interface
    (setf (capi:switchable-layout-visible-child switchable)
          (if (eq pane 'nothing)
              nil
            pane))
    (update-switchable-layouts-test-info interface)))

(defun update-switchable-layouts-test-info (interface)
  (with-slots (info switchable) interface
    (let ((child (capi:switchable-layout-visible-child switchable)))
      (setf (capi:title-pane-text info)
            (format nil "Currently displaying ~A."
                    (if (symbolp child)
                        child
                      (capi:capi-object-name child)))))))

;;----------------------------------------------------------------------------
;; test-switchable-layouts
;;----------------------------------------------------------------------------

(defun test-switchable-layouts (&key height-control)
  (let ((interface (make-instance 'switchable-layouts-test
                                  :combine-child-constraints (eq height-control :combine)
                                  :switchable-max-height (eq height-control t))))
    (capi:display interface)))

