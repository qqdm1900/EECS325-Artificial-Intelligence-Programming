;; -*- rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/graphics:pinboard-help.lisp,v 1.2.11.1 2011/08/24 13:26:21 davef Exp $" -*-

;;----------------------------------------------------------------------------
;;
;; examples/capi/graphics/pinboard-help.lisp
;;
;; This example demonstrates how to make tooltips for pinboard-objects
;; in the CAPI.
;;
;; To try it, compile and load this file and then execute:
;;
;;      (CL-USER::TEST-PINBOARD-HELP)
;;
;;----------------------------------------------------------------------------
;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
;;----------------------------------------------------------------------------


(in-package "CL-USER")


;;----------------------------------------------------------------------------
;; Define an interface
;;----------------------------------------------------------------------------

(capi:define-interface pinboard-help ()
  ()
  (:panes)
  (:layouts
   (main-layout
    capi:pinboard-layout
    (list (make-instance 'capi:item-pinboard-object
                         :text "Move the mouse over one of the items below."
                         :print-function 'print-pinboard-help-item)
          (make-instance 'capi:column-layout
                         :description
                         (loop for data below 10
                               collect
                               (make-instance 'capi:item-pinboard-object
                                              :data data
                                              :print-function 'print-pinboard-help-item))
                         :gap 10
                         :x 40
                         :y 50))
    :background :white
    :input-model '((:motion   pinboard-help-motion-callback))))
  (:default-initargs
   :layout 'main-layout
   :title "Pinboard Help"))


;;----------------------------------------------------------------------------
;; The callbacks
;;----------------------------------------------------------------------------

(defun print-pinboard-help-item (data)
  (format nil "Item ~D" data))

(defun pinboard-help-motion-callback (pinboard x y)
  (let* ((object (capi:pinboard-object-at-position pinboard x y))
         (data (and (capi:itemp object) (capi:item-data object))))
    (if data
        (let ((help-text (format nil "This is item ~R" data)))
          (capi:with-geometry object
            ;; Display a tooltip for this item, positioned where the item starts.
            (capi:display-tooltip pinboard
                                  :x capi:%x%
                                  :y capi:%y%
                                  :text help-text)))
      ;; Remove the tooltip since we are not over an item.
      (capi:display-tooltip pinboard))))


;;----------------------------------------------------------------------------
;; The test function
;;----------------------------------------------------------------------------

(defun test-pinboard-help ()
  (capi:display (make-instance 'pinboard-help)))
