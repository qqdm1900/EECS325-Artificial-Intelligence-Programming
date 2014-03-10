;;----------------------------------------------------------------------------
;;
;; scrolling-test.lisp
;;
;; This test demonstrates scrolling two windows in unison by using a
;; scroll-callback on output-panes.
;;
;;----------------------------------------------------------------------------
;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
;;----------------------------------------------------------------------------
;; To run this test, compile and load this file and then execute:
;;
;;     (CL-USER::SCROLLING-TEST)
;;----------------------------------------------------------------------------


(in-package "CL-USER")


;;----------------------------------------------------------------------------
;; scrolling-test
;;
;; This function will run the test
;;----------------------------------------------------------------------------

(capi:define-interface scrolling-test ()
  ()
  (:panes
   (message-pane
    capi:title-pane
    :accessor scrolling-test-message-pane
    :visible-min-width nil
    :visible-max-width nil))
  (:layouts
   (column-1
    capi:column-layout
    (make-pinboard-objects))
   (scroll-layout-1
    capi:simple-pinboard-layout
    '(column-1)
    :accessor scrolling-test-scroll-layout-1
    :horizontal-scroll t
    :vertical-scroll t
    :scroll-callback 'scroll-callback)
   (column-2
    capi:column-layout
    (make-pinboard-objects))
   (scroll-layout-2
    capi:simple-pinboard-layout
    '(column-2)
    :accessor scrolling-test-scroll-layout-2
    :horizontal-scroll t
    :vertical-scroll t
    :scroll-callback 'scroll-callback)
   (row
    capi:row-layout
    '(scroll-layout-1 scroll-layout-2))
   (main-layout
    capi:column-layout
    '(row message-pane)))
  (:default-initargs
   :title "Scrolling Test"
   :layout 'main-layout
   :best-width 400
   :best-height 500))

(defun make-pinboard-objects ()
  (loop for i below 50
        collect (make-instance 'capi:item-pinboard-object :data i)))

#|
;;; This is the obsolete form.
(defun scroll-callback (self x y)
  (let ((interface (capi:element-interface self)))
    (setf (capi:title-pane-text (scrolling-test-message-pane interface))
          (format nil "Scrolled to ~S,~S" x y))
    (capi:set-scroll-position (scrolling-test-scroll-layout-1 interface)
                              x y)
    (capi:set-scroll-position (scrolling-test-scroll-layout-2 interface)
                              x y)))
|#

(defun scroll-callback (self scroll-dimension scroll-operation scroll-value
                             &key
                             ;; T iff invoked as a result of scroll bar input
                             interactive
                             ;; Remaining are user defined.
                             &allow-other-keys)

  ;; Scroll the other pane only if this callback was invoked as a result of a scroll-bar operation.
  ;; Otherwise, its the programatic scroll and we don't want to cause an infinite loop
  ;;  of scroll callbacks!

  (when interactive

    (let* ((interface (capi:element-interface self))
           (other-pane (if (eq (scrolling-test-scroll-layout-1 interface) self)
                           (scrolling-test-scroll-layout-2 interface)
                         (scrolling-test-scroll-layout-1 interface)))
           (x (capi:get-horizontal-scroll-parameters self :slug-position))
           (y (capi:get-vertical-scroll-parameters self :slug-position)))

        (setf (capi:title-pane-text (scrolling-test-message-pane interface))
              (format nil "Scrolled to ~S,~S" x y))

        (capi:scroll other-pane scroll-dimension scroll-operation scroll-value))))

(defun scrolling-test ()
  (capi:display (make-instance 'scrolling-test)))
