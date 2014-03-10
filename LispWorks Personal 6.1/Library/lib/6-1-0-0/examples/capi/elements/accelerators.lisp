;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/elements:accelerators.lisp,v 1.13.1.1 2011/08/24 13:26:21 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

(in-package "CL-USER")

;;; ----------------------------------------------------------------------
;;;
;;; Example use of accelerators in an application
;;; To try it, compile and load this file and then execute:
;;;
;;;      (CL-USER::TEST-ACCELERATORS)
;;;
;;; On Windows you can try the MDI version
;;;
;;;      (CL-USER::TEST-ACCELERATORS-MDI)
;;;
;;; And dynamically set up the accelerators
;;;
;;;      (CL-USER::TEST-ACCELERATORS-DYNAMIC)
;;;
;;; ----------------------------------------------------------------------

;;; Note that on Windows in the accelerator model, Alt == Meta

;;; Note that items 8, 9 and "no accelerator" have alternative items,
;;; which can be invoked by keeping the Shift key down while
;;; selecting the item, or using the accelerator with shift.
;;; Item 10 has an alternative with Control.
;;; The alternative items are defined by the items with :ALTERNATIVE T. 

;;; Note that some accelerators do not work on some platforms because
;;; they have other standard meanings, for example F1 on Windows.

(capi:define-interface test-accelerators () ()
  (:panes 
   (text-pane capi:text-input-pane)
   (text-pane2 capi:text-input-pane)
   (collector capi:collector-pane :min-height 100 :visible-min-width '(:character 60)))
  (:layouts 
   (main capi:column-layout '(text-pane text-pane2 collector)))
  (:menus 
   (accelerator-menu
    "The Accelerator menu"
    (("item 1" :data "ctrl-a" :accelerator "accelerator-ctrl-a")
     ("item 2" :data "ctrl-shift-a" :accelerator "accelerator-ctrl-shift-a")
     ("item 3" :data "meta-a" :accelerator "accelerator-meta-a")
     ("item 4" :data "meta-shift-a" :accelerator "accelerator-meta-shift-a")
     ("item 5" :data "meta-ctrl-a" :accelerator "accelerator-meta-ctrl-a")
     ("item 6" :data "meta-ctrl-shift-a" :accelerator "accelerator-meta-ctrl-shift-a")
     ("item 7" :data "f1" :accelerator "accelerator-f1")
   
     ("item 8" :data "f2" :accelerator "accelerator-f2")
     ("item 8 alternative" :data "shift-f2" :accelerator "accelerator-shift-f2"
                           :alternative t)
    
     ("item 9" :data "ctrl-f3" :accelerator "accelerator-ctrl-f3")
     ("item 9 alternative" :data "shift-f3" :accelerator "accelerator-shift-f3"
                           :alternative t)

     ("item 10" :data "home" :accelerator "accelerator-home")
     ("item 10" :data "ctrl-home" :accelerator "accelerator-ctrl-home" 
                :alternative t)
     ("item 11" :data "up" :accelerator "accelerator-up")
     ("item 12" :data "a" :accelerator "accelerator-a")
     ("item 13" :data "insert" :accelerator "accelerator-insert")
     ("A very Long menu item" :data "prior" :accelerator "accelerator-prior")
     ("item 15" :data "backspace" :accelerator "accelerator-backspace")
     ("item 16" :data "delete" :accelerator "accelerator-delete")
     ("item 17" :data "return" :accelerator "accelerator-return")
     ;; :alternative of an item without acceslerator, use "accelerator"
     ;; with null
     ("No accelerator" :data "\"No accelerator menu item\"")
     ("No accelerator alternative" :data "Shift \"No accelerator menu item\""
                                   :accelerator "accelerator-shift-null"
                                   :alternative t)
     )
    :callback 'accelerator-pressed
    :callback-type '(:interface :focus :data)))
  (:menu-bar accelerator-menu))

(defun accelerator-pressed (self focus data)
  (with-slots (text-pane text-pane2 collector) self
    (format (capi:collector-pane-stream collector)
            "Pressed ~A over ~A~%"
            data
            (cond ((eq focus text-pane) "top pane")
                  ((eq focus text-pane2) "middle pane")
                  ((eq focus collector) "bottom pane")
                  (t "unknown pane")))))

(defun test-accelerators ()
  (capi:display (make-instance 'test-accelerators)))

#+Win32
(capi:define-interface test-accelerators-mdi (capi:document-frame)
  ()
  (:menus
   (foo
    "The MDI Accelerator menu"
    (("Foo" :data "ctrl-f" :accelerator "ctrl-f"
            :callback #'(lambda () (capi:display-message "foo"))
            :callback-type :none)
     )))
  (:menu-bar foo capi:windows-menu)
  (:default-initargs
   :best-width 500
   :best-height 300))

#+Win32
(defun test-accelerators-mdi ()
  (let ((document-frame
         (capi:display (make-instance 'test-accelerators-mdi))))
    (capi:execute-with-interface 
     document-frame
     #'(lambda (frame)
         (capi:display (make-instance 'test-accelerators) :screen frame)
         (capi:display (make-instance 'capi:interface) :screen frame))
     document-frame)
    document-frame))

(capi:define-interface test-accelerators-dynamic () ()
  (:panes )
  (:layouts 
   (main capi:column-layout '()))
  (:menus 
   (accelerator-menu
    "The Accelerator menu"
    (("item 1" :data "p" :accelerator "accelerator-p")
     )
    :callback #'(lambda () (capi:display-message "Accelerator triggered"))
    :callback-type :none)
   (control-menu
    "Control if accelerators set"
    (("Accelerator menu add" :data t :accelerator "accelerator-a")
     ("Accelerator menu delete" :data nil :accelerator "accelerator-d"))
    :callback #'(lambda (add-control self)
                  ;; Insert or remove accelerator-menu according to
                  ;; the value of add-control.
                  (setf (capi:interface-menu-bar-items self)
                        (loop for item in (capi:interface-menu-bar-items self)
                              unless (eq item accelerator-menu)
                              collect item
                              if (and add-control (eq item control-menu))
                              collect accelerator-menu)))
    :callback-type :data-interface))
  (:menu-bar control-menu))

(defun test-accelerators-dynamic ()
  (capi:display (make-instance 'test-accelerators-dynamic
                               :best-width 500
                               :best-height 200)))
