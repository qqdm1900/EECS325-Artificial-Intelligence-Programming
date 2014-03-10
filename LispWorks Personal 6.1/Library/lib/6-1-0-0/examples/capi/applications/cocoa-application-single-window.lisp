;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/applications:cocoa-application-single-window.lisp,v 1.1.1.1 2011/08/24 13:26:20 davef Exp $" -*-

;;----------------------------------------------------------------------------
;;
;; examples/capi/applications/cocoa-application-single-window.lisp
;;
;; This example shows how you use an application interface object to control
;; the application menu in CAPI with Cocoa on Mac OS X.
;;
;; This is a variant of the capi/applications/cocoa-application.lisp example,
;; but only allows a single window (i.e. supports one open document).  When
;; run outside the LispWorks IDE, the application quits when the document
;; window is closed.
;;
;; To try the example, compile and load this file and then execute:
;;
;;      (CL-USER::TEST-COCOA-APPLICATION-SINGLE)
;;
;; or see the example in delivery/macos/single-window-application.lisp for
;; details on how to build it into a standalone application.
;;
;;----------------------------------------------------------------------------
;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
;;----------------------------------------------------------------------------



(in-package "CL-USER")

;;----------------------------------------------------------------------------
;; The main interface
;;----------------------------------------------------------------------------

(capi:define-interface single-circle-drawer ()
  ((runp :initform nil)
   (application-interface :initarg :application-interface))
  (:panes
   (label capi:title-pane)
   (drawing capi:output-pane
            :display-callback 'single-circle-drawer-display))
  (:layouts
   (main-layout capi:column-layout
                '(label drawing)))
  (:menus
   (action-menu "Action"
                (("Draw Circle"
                  :callback 'single-circle-drawer-start
                  :callback-type :interface))))
  (:menu-bar action-menu)
  (:default-initargs
   :layout 'main-layout
   :best-width 250
   :best-height 250
   :title "Single Circle Drawer"
   :destroy-callback 'single-circle-drawer-quit))

(defun single-circle-drawer-quit (self)
  (when-let (application (slot-value self 'application-interface))
    ;; Set drawing-interface to nil to prevent recursion back from
    ;; application-interface's destroy-callback.
    (setf (cocoa-application-interface-single-drawing-interface application)
          nil)
    ;; Quit by destroying the application interface.
    (capi:destroy application)
    ))

(defun single-circle-drawer-display (pane x y width height)
  (declare (ignore x y width height))
  (when (slot-value (capi:element-interface pane) 'runp)
    (gp:draw-circle pane 100 100 50
                    :foreground :blue)))

(defun single-circle-drawer-start (self)
  (setf (slot-value self 'runp) t)
  (single-circle-drawer-update self))

(defun single-circle-drawer-update (self)
  (gp:invalidate-rectangle (slot-value self 'drawing)))

(defun single-circle-drawer-set-label (self text)
  (setf (capi:title-pane-text (slot-value self 'label)) text))


;;----------------------------------------------------------------------------
;; The application interface
;;----------------------------------------------------------------------------

(capi:define-interface cocoa-application-interface-single (capi:cocoa-default-application-interface)
  ((drawing-interface :initform nil
                      :accessor cocoa-application-interface-single-drawing-interface))
  (:default-initargs
   :title "Single Window CAPI Application"
   :message-callback 'cocoa-application-interface-single-message
   :destroy-callback 'cocoa-application-interface-single-destroyed))

(defun cocoa-application-interface-single-message (application message
                                                               &rest args)
  (case message
    (:open-file
     (let ((filename (car args)))
       (when (equal (pathname-type filename) "lwtestsingle")
         (when-let (drawing-interface
                    (cocoa-application-interface-single-drawing-interface
                     application))
           (capi:execute-with-interface drawing-interface
                                        'single-circle-drawer-set-label
                                        drawing-interface
                                        filename)))))))

(defun cocoa-application-interface-single-destroyed (application)
  (when-let (drawing-interface
             (cocoa-application-interface-single-drawing-interface
              application))
    ;; Set application-interface to nil to prevent recursion back from
    ;; drawing-interface's destroy-callback.
    (setf (slot-value drawing-interface 'application-interface) nil)
    ;; Destroy the single drawing window.  When run as a delivered
    ;; application, this will cause the application to exit because it
    ;; has no more windows.
    (capi:destroy drawing-interface)))

(defun test-cocoa-application-single ()
  (let ((application (make-instance 'cocoa-application-interface-single)))
    ;; Set the application interface before using any other CAPI
    ;; functionality.
    (capi:set-application-interface application)
    ;; Start the application with its single window.
    (let ((drawing-interface (make-instance 'single-circle-drawer
                                            :application-interface application)))
      (setf (cocoa-application-interface-single-drawing-interface application)
            drawing-interface)
      (capi:display drawing-interface))))

