;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/layouts:docking-layout.lisp,v 1.4.5.1 2011/08/24 13:26:21 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

;;----------------------------------------------------------------------------
;;
;; docking-layout.lisp
;;
;; This example demonstrates the use of class CAPI:DOCKING-LAYOUT.
;;
;;----------------------------------------------------------------------------
;; To run this demo, compile and load this file and then execute one of these;
;;
;;    (CL-USER::TEST-SIMPLE-DOCKING-LAYOUT)
;;    (CL-USER::TEST-DOCKING-LAYOUT-WITH-CONTROLLER)
;;    (CL-USER::TEST-DOCKING-LAYOUT-WITH-TEST-FUNCTION)
;;    (CL-USER::TEST-DOCKING-LAYOUT-WITH-STATE)
;;
;;----------------------------------------------------------------------------
;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
;;----------------------------------------------------------------------------


(in-package "CL-USER")


;; Example showing a simple docking layout with toolbar.  The title of
;; the first item defaults to its class, TOOLBAR.

(capi:define-interface simple-docking-layout ()
  ()
  (:panes
   (list capi:list-panel
         :items '(a b c d)
         :visible-min-height '(character 4))
   (docking-toolbar1 capi:toolbar
                     :items '(1 2 3 4 5 6))
   (docking-toolbar2 capi:toolbar
                     :items '(7 8 9 10)))
  (:layouts
   (main capi:column-layout
         '(docking list))
   (docking capi:docking-layout
            '()
            :items '(docking-toolbar1
                     (docking-toolbar2 :title "Dock2"))))
  (:default-initargs
   :layout 'main
   :title "Simple Docking Layout"))

(defun test-simple-docking-layout ()
  (capi:display (make-instance 'simple-docking-layout)))

;;----------------------------------------------------------------------------


;; Example showing a group of docking layouts.  The toolbars can be
;; dropped into either docking layout because they share a controller.

(capi:define-interface docking-layout-with-controller ()
  ((controller :initform (capi:make-docking-layout-controller)))
  (:panes
   (list capi:list-panel
         :items '(a b c d)
         :visible-min-height '(character 4))
   (docking-toolbar1 capi:toolbar
                     :items '(1 2 3 4 5 6))
   (docking-toolbar2 capi:toolbar
                     :items '(7 8 9 10))
   (docking-toolbar3 capi:toolbar
                     :items '(11 12 13))
   (docking-toolbar4 capi:toolbar
                     :items '(14 10)))
  (:layouts
   (main capi:column-layout
         '(top-docking list bottom-docking))
   (top-docking capi:docking-layout
                '()
                :controller controller
                :items '((docking-toolbar1 :title "Dock1")
                         (docking-toolbar2 :title "Dock2")
                         (docking-toolbar3 :title "Dock3")))
   (bottom-docking capi:docking-layout
                   '()
                   :controller controller
                   :items '((docking-toolbar4 :title "Dock4"))))
  (:default-initargs
   :layout 'main
   :title "Docking Layout With Controller"))

(defun test-docking-layout-with-controller ()
  (capi:display (make-instance 'docking-layout-with-controller)))

;;----------------------------------------------------------------------------


;; Example showing a constrained group of docking layouts.  The
;; toolbars can be dropped into either docking layout but the list can
;; only be dropped into the left hand layout.  Also, docking-toolbar2
;; starts undocked initially.

(capi:define-interface docking-layout-with-test-function ()
  ((controller :initform (capi:make-docking-layout-controller)))
  (:panes
   (display capi:display-pane
            :text "This is a display pane."
            :visible-min-height '(:character 20)
            :visible-max-width nil
            :visible-max-height nil)
   (docking-toolbar1 capi:toolbar
                     :items '(1 2 3 4 5 6))
   (docking-toolbar2 capi:toolbar
                     :items'(7 8 9 10))
   (docking-toolbar3 capi:toolbar
                     :items '(11 12 13))
   (docking-list capi:list-panel
                 :items '(1 2 3 4)
                 :visible-min-width 100
                 :vertical-scroll nil)
   (docking-toolbar4 capi:toolbar
                     :items '(14 10)))
  (:layouts
   (main capi:column-layout
         '(top-docking bottom))
   (bottom capi:row-layout
           '(left-docking display))
   (left-docking capi:docking-layout
                 '()
                 :items '((docking-toolbar1 :title "Dock1")
                          (docking-toolbar2 :title "Dock2" :docked-p nil
                                            :undocked-geometry (100 40 nil nil))
                          (docking-toolbar3 :title "Dock3")
                          (docking-list :start-new-line-p t))
                 :controller controller
                 :orientation :vertical)
   (top-docking capi:docking-layout
                '()
                :controller controller
                :items '((docking-toolbar4 :title "Dock4"))
                :docking-test-function
                #'(lambda (docking-layout pane)
                    (declare (ignore docking-layout))
                    (not (eq pane docking-list)))))
  (:default-initargs
   :layout 'main
   :title "Docking Layout With Test Function"))

(defun test-docking-layout-with-test-function ()
  (capi:display (make-instance 'docking-layout-with-test-function)))


;;----------------------------------------------------------------------------


;; Example showing a constrained group of docking layouts.  The
;; toolbars can be dropped into either docking layout but the list can
;; only be dropped into the left hand layout.  Also, docking-toolbar2
;; starts undocked initially.

(capi:define-interface docking-layout-with-state ()
  ((controller :initform (capi:make-docking-layout-controller))
   (count :initform 0))
  (:panes
   (docking-toolbar1 capi:toolbar
                     :items '(1 2 3 4 5 6))
   (docking-toolbar2 capi:toolbar
                     :items'(7 8 9 10))
   (docking-toolbar3 capi:toolbar
                     :items '(11 12 13))
   (docking-toolbar4 capi:toolbar
                     :items '(14 10))
   (state-list capi:list-panel
               :print-function 'car
               :action-callback 'docking-layout-with-state-select
               :callback-type :interface-data
               :visible-min-height '(character 8))
   (control-buttons capi:push-button-panel
                    :items '("Capture" "Discard" "Restore")
                    :callbacks '(docking-layout-with-state-capture
                                 docking-layout-with-state-discard
                                 docking-layout-with-state-restore)
                    :callback-type :interface))
  (:layouts
   (main capi:column-layout
         '(top-docking bottom))
   (state-layout capi:column-layout
                 '(state-list control-buttons)
                 :title "Captured Docking states"
                 :title-position :frame
                 :internal-border 10)
   (bottom capi:row-layout
           '(left-docking state-layout))
   (left-docking capi:docking-layout
                 '()
                 :items '((docking-toolbar1 :title "Dock1")
                          (docking-toolbar2 :title "Dock2" :docked-p nil)
                          (docking-toolbar3 :title "Dock3"))
                 :controller controller
                 :orientation :vertical)
   (top-docking capi:docking-layout
                '()
                :controller controller
                :items '((docking-toolbar4 :title "Dock4"))))
  (:default-initargs
   :layout 'main
   :title "Docking Layout With State"))

(defun docking-layout-with-state-capture (interface)
  (with-slots (state-list count left-docking top-docking) interface
    (let ((state (list (format nil "Captured state ~D" (incf count))
                       (capi:docking-layout-items left-docking)
                       (capi:docking-layout-items top-docking))))
      (setf (capi:collection-items state-list)
            (concatenate 'list
                         (capi:collection-items state-list)
                         (list state)))
      (setf (capi:choice-selected-item state-list) state))))

(defun docking-layout-with-state-discard (interface)
  (with-slots (state-list) interface
    (let ((selection (capi:choice-selection state-list))
          (remaining-states (remove (capi:choice-selected-item state-list)
                                    (capi:collection-items state-list)
                                    :test 'eq)))
      (setf (capi:collection-items state-list)
            remaining-states)
      (setf (capi:choice-selection state-list)
            (let ((last (1- (length remaining-states))))
              (and (>= last 0)
                   (min selection last)))))))

(defun docking-layout-with-state-restore (interface)
  (with-slots (state-list) interface
    (let ((state (capi:choice-selected-item state-list)))
      (when state
        (docking-layout-with-state-select interface state)))))

(defun docking-layout-with-state-select (interface state)
  (with-slots (left-docking top-docking) interface
    (destructuring-bind (name left-state top-state) state
      (declare (ignore name))
      (setf (capi:docking-layout-items left-docking) left-state
            (capi:docking-layout-items top-docking) top-state))))

(defun test-docking-layout-with-state ()
  (capi:display (make-instance 'docking-layout-with-state)))


