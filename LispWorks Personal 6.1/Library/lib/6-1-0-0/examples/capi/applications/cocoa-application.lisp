;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/applications:cocoa-application.lisp,v 1.3.1.1 2011/08/24 13:26:20 davef Exp $" -*-

;;----------------------------------------------------------------------------
;;
;; examples/capi/applications/cocoa-application.lisp
;;
;; This example shows how you use an application interface object to control
;; the application menu and multiple document windows in CAPI with Cocoa on
;; Mac OS X.
;;
;; When run outside the LispWorks IDE, the application interface's menu bar
;; (the Circle menu) is shown if all document windows have been closed.  When
;; run inside the LispWorks IDE, the application interface's menu bar is
;; available as a submenu of the LispWorks application menu.
;;
;; To try the example, compile and load this file and then execute:
;;
;;      (CL-USER::TEST-COCOA-APPLICATION-MULTIPLE)
;;
;; or see the example in delivery/macos/multiple-window-application.lisp for
;; details on how to build it into a standalone application.
;;
;;----------------------------------------------------------------------------
;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
;;----------------------------------------------------------------------------



(in-package "CL-USER")

;;----------------------------------------------------------------------------
;; The main interface
;;----------------------------------------------------------------------------

(capi:define-interface multiple-circle-drawer ()
  ((runp :initform nil))
  (:panes
   (label capi:title-pane)
   (drawing capi:output-pane
            :display-callback 'multiple-circle-drawer-display))
  (:layouts
   (main-layout capi:column-layout
                '(label drawing)))
  (:menus
   (action-menu "Action"
                (("Draw Circles"
                  :callback 'multiple-circle-drawer-start
                  :callback-type :interface)))
   (circle-menu
    "Circle"
    (("New"
      :callback 'multiple-circle-drawer-new
      :callback-type :none))))
  (:menu-bar circle-menu action-menu)
  (:default-initargs
   :layout 'main-layout
   :best-width 250
   :best-height 250))

(defun multiple-circle-drawer-new ()
  (capi:display (make-instance 'multiple-circle-drawer)))

(defvar *multiple-circle-drawer-count* 0)

(defmethod initialize-instance :after ((self multiple-circle-drawer) &key)
  (setf (capi:interface-title self)
        (format nil "My Window ~D" (incf *multiple-circle-drawer-count*))))

(defvar *multiple-circle-drawer-outer-color* :red)

(defun set-multiple-circle-drawer-outer-color (color)
  (setq *multiple-circle-drawer-outer-color* color))

(defun multiple-circle-drawer-display (pane x y width height)
  (declare (ignore x y width height))
  (when (slot-value (capi:element-interface pane) 'runp)
    (gp:draw-circle pane 100 100 100
                    :foreground *multiple-circle-drawer-outer-color*)
    (gp:draw-circle pane 100 100 50
                    :foreground :blue)))

(defun multiple-circle-drawer-start (self)
  (setf (slot-value self 'runp) t)
  (multiple-circle-drawer-update self))

(defun multiple-circle-drawer-update (self)
  (gp:invalidate-rectangle (slot-value self 'drawing)))

(defun multiple-circle-drawer-set-label (self text)
  (setf (capi:title-pane-text (slot-value self 'label)) text))


;;----------------------------------------------------------------------------
;; The application interface
;;----------------------------------------------------------------------------

(capi:define-interface cocoa-application-interface-multiple (capi:cocoa-default-application-interface)
  ()
  (:menus
   (application-menu
    "Multiple Window CAPI Application"
    ((:component
      (("About Multiple Window CAPI Application"
        :callback 'cocoa-application-interface-multiple-about
        :callback-type :none)))
     (:component
      (("Preferences..."
        :callback 'cocoa-application-interface-multiple-preferences
        :callback-type :none)))
     (:component
      ()
      ;; This is a special named component where the CAPI will
      ;; attach the standard Services menu.
      :name :application-services)
     (:component
      (("Hide Multiple Window CAPI Application"
        :accelerator "accelerator-h"
        :callback-data :hidden)
       ("Hide Others"
        :accelerator "accelerator-meta-h"
        :callback-data :others-hidden)
       ("Show All"
        :callback-data :all-normal))
      :callback #'(setf capi:top-level-interface-display-state)
      :callback-type :data-interface)
     (:component
      (("Quit Multiple Window CAPI Application"
        :accelerator "accelerator-q"
        :callback 'capi:destroy
        :callback-type :interface)))))
   (circle-menu
    "Circle"
    (("New"
      :callback 'multiple-circle-drawer-new
      :callback-type :none))))
  (:menu-bar application-menu circle-menu)
  (:default-initargs
   :title "Multiple Window CAPI Application"
   :application-menu 'application-menu
   :message-callback 'cocoa-application-interface-multiple-message
   :dock-menu 'make-cocoa-application-interface-multiple-dock-menu))

(defun cocoa-application-interface-multiple-about ()
  (capi:display-message-on-screen (capi:convert-to-screen nil)
                                  "Multiple Window CAPI Application is a CAPI demo"))

(defun cocoa-application-interface-multiple-preferences ()
  (multiple-value-bind (color okp)
      (capi:prompt-for-color "Outer circle color"
                             :color *multiple-circle-drawer-outer-color*)
    (when okp
      (set-multiple-circle-drawer-outer-color color)
      (dolist (interface (capi:collect-interfaces 'multiple-circle-drawer))
        (capi:execute-with-interface interface
                                     'multiple-circle-drawer-update
                                     interface)))))

(defun cocoa-application-interface-multiple-message (self message &rest args)
  (declare (ignore self))
  (case message
    (:open-file
     (let ((filename (car args)))
       (when (equal (pathname-type filename) "lwtestmultiple")
         (let ((interface (multiple-circle-drawer-new)))
           (capi:execute-with-interface interface
                                        'multiple-circle-drawer-set-label
                                        interface
                                        filename)))))))

(capi:define-menu make-cocoa-application-interface-multiple-dock-menu (self)
  "Dock Menu"
  ((:component
    ()
    :interaction :single-selection
    :items-function #'(lambda (self)
                        (declare (ignore self))
                        (capi:collect-interfaces 'multiple-circle-drawer))
    :selected-item-function #'(lambda (interface)
                                (capi:screen-active-interface
                                 (capi:convert-to-screen interface)))
    :print-function 'capi:interface-title
    :callback-type :data
    :callback 'capi:activate-pane)))

(defun test-cocoa-application-multiple ()
  (let ((application (make-instance 'cocoa-application-interface-multiple)))
    ;; Set the application interface before using any other CAPI
    ;; functionality.
    (capi:set-application-interface application)
    ;; Start the application with no windows initially.
    (capi:convert-to-screen nil)))

