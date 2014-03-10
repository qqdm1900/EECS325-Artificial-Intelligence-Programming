;; -*- rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/dialogs:mutating-dialog.lisp,v 1.2.15.1 2011/08/24 13:26:21 davef Exp $" -*-

;;----------------------------------------------------------------------------
;;
;; examples/capi/dialogs/mutating-dialog.lisp
;;
;; This file demonstrates the use of (setf capi:layout-description) to
;; dynamically add panes to a dialog/interface.
;;
;;----------------------------------------------------------------------------
;; To run this test as a dialog do:
;;
;;   (CL-USER::TEST-MUTATING-DIALOG)
;;
;; To run it as an interface do:
;;
;;   (CL-USER::TEST-MUTATING-DIALOG :DIALOGP NIL)
;;----------------------------------------------------------------------------
;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
;;----------------------------------------------------------------------------


(in-package "CL-USER")


(capi:define-interface mutating-interface ()
  ()
  (:panes
   (button
    capi:push-button
    :text "Add a new button"
    :callback-type :interface
    :callback 'add-a-new-button)
   (quit-button
    capi:push-button
    :text "Quit"
    :callback-type :interface
    :callback 'abort-mutator))
  (:layouts
   (main-layout
    capi:column-layout
    '(button quit-button)))
  (:default-initargs
   :title "Mutating interface"))
  
(defun add-a-new-button (self)
  (with-slots (main-layout) self
    (setf (capi:layout-description main-layout)
          (append
           (capi:layout-description main-layout)
           (list (make-instance 'capi:push-button :text "New Button"))))))

(defun abort-mutator (self)
  (or (capi:abort-dialog self)		; it was a dialog
      (capi:quit-interface self)	; it was not a dialog
      ))

(defun test-mutating-dialog (&key (dialogp t))
  (let ((interface (make-instance 'mutating-interface)))
    (if dialogp
        (capi:display-dialog interface)
      (capi:display interface))))
