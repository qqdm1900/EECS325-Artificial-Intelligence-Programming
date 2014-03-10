;; -*- rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/elements:scroll-bar.lisp,v 1.1.10.1 2011/08/24 13:26:20 davef Exp $" -*-

;;----------------------------------------------------------------------------
;;
;; examples/capi/elements/scroll-bar.lisp
;;
;; This example demonstrates the uses of standalone scroll-bars in the CAPI.
;;
;; NB: Normally the best way to obtain a scrolling in a pane is to use the
;; :HORIZONTAL-SCROLL or :VERTICAL-SCROLL initargs.
;;
;; To try it, compile and load this file and then execute:
;;
;;      (CL-USER::TEST-SCROLL-BAR)
;;
;;----------------------------------------------------------------------------
;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
;;----------------------------------------------------------------------------


(in-package "CL-USER")


(capi:define-interface scroll-bar-test ()
  ()
  (:panes
   (controllable-scroll-bar
    capi:scroll-bar
    :start 10
    :end 232
    :slug-start 10
    :slug-end 20
    :callback 'controllable-scroll-bar-callback)
   (controllable-start
    capi:text-input-pane
    :callback 'controllable-scroll-bar-set-callback
    :callback-type :interface-item)
   (controllable-end
    capi:text-input-pane
    :callback 'controllable-scroll-bar-set-callback
    :callback-type :interface-item)
   (controllable-slug-start
    capi:text-input-pane
    :callback 'controllable-scroll-bar-set-callback
    :callback-type :interface-item)
   (controllable-slug-end
    capi:text-input-pane
    :callback 'controllable-scroll-bar-set-callback
    :callback-type :interface-item)
   (controllable-label
    capi:title-pane)
   (useless-scroll-bar
    capi:scroll-bar
    :start 10
    :end 232
    :slug-start 10
    :slug-end 232
    :title "Scroll bar that cannot be moved"
    :title-position :frame)
   (disabled-scroll-bar
    capi:scroll-bar
    :start 10
    :end 232
    :slug-start 10
    :slug-end 22
    :enabled nil
    :title "Scroll bar that is disabled"
    :title-position :frame))
  (:layouts
   (controllable-scroll-bar-layout
    capi:column-layout
    '(controllable-scroll-bar
      (capi:grid-layout ("Range start:"
                         controllable-start
                         "Range end:"
                         controllable-end
                         "Slug start:"
                         controllable-slug-start
                         "Slug end:"
                         controllable-slug-end)
                        :columns 4
                        :y-adjust :center)
      controllable-label)
    :gap 5
    :title "Scroll bar and values"
    :title-position :frame)
   (main-layout
    capi:column-layout
    '(controllable-scroll-bar-layout
      useless-scroll-bar
      disabled-scroll-bar)
    :gap 10
    :internal-border 5))
  (:default-initargs
   :title "Scroll Bar Test"
   :best-width 300
   :layout 'main-layout))

(defmethod initialize-instance :after ((self scroll-bar-test) &key)
  (update-panes-from-scroll-bar-test self))

(defun update-panes-from-scroll-bar-test (self)
  (with-slots (controllable-scroll-bar
               controllable-start controllable-end
               controllable-slug-start controllable-slug-end)
      self
    (setf (capi:text-input-pane-text controllable-start)
          (format nil "~D" (capi:range-start controllable-scroll-bar)))
    (setf (capi:text-input-pane-text controllable-end)
          (format nil "~D" (capi:range-end controllable-scroll-bar)))
    (setf (capi:text-input-pane-text controllable-slug-start)
          (format nil "~D" (capi:range-slug-start controllable-scroll-bar)))
    (setf (capi:text-input-pane-text controllable-slug-end)
          (format nil "~D" (capi:range-slug-end controllable-scroll-bar)))))


;;----------------------------------------------------------------------------
;; Callbacks
;;----------------------------------------------------------------------------

(defun controllable-scroll-bar-callback (interface self how where)
  (declare (ignore self))
  (with-slots (controllable-label) interface
    (setf (capi:title-pane-text controllable-label)
          (format nil "Last scroll: ~S ~S" how where))
    (update-panes-from-scroll-bar-test interface)))

(defun controllable-scroll-bar-set-callback (interface text-pane)
  (with-slots (controllable-scroll-bar) interface
    (handler-case
        (let ((value (parse-integer (capi:text-input-pane-text text-pane))))
          (case (capi:capi-object-name text-pane)
            (controllable-start
             (setf (capi:range-start controllable-scroll-bar) value))
            (controllable-end
             (setf (capi:range-end controllable-scroll-bar) value))
            (controllable-slug-start
             (setf (capi:range-slug-start controllable-scroll-bar) value))
            (controllable-slug-end
             (setf (capi:range-slug-end controllable-scroll-bar) value))))
      (parse-error (condition)
                   (capi:display-message "~A" condition)))))

;;----------------------------------------------------------------------------
;; The test function test-scroll-bar
;;----------------------------------------------------------------------------

(defun test-scroll-bar ()
  (capi:display (make-instance 'scroll-bar-test)))

