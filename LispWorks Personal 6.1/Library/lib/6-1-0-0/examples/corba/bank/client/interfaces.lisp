;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/43/LISPcorba-doc-bank/RCS/client:interfaces.lisp,v 1.3.7.1 2011/08/24 13:25:59 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

(in-package "CL-USER")

(capi:define-interface account-interface ()
  ((account-ref :initarg :account-ref)
   (account-name :initarg :account-name :accessor account-name)
   (bank-interface :initarg :owner))
  (:panes
   (balance-field capi:title-pane 
                  :title (:initarg :account-name)
                  :min-width '(:character 10)
                  :max-width nil)
   (button-panel capi:push-button-panel 
                 :callbacks '(credit debit)
                 :items '("Credit" "Debit")
                 :callback-type :interface))
  (:layouts 
   (account-layout capi:column-layout '(balance-field button-panel)))
  (:default-initargs :auto-menus nil :max-width t))

(capi:define-interface checking-account-interface (account-interface) ()  
  (:panes
   (limit-field capi:title-pane 
                :min-width '(:character 10)
                :max-width nil))
  (:layouts
   (checking-account-layout capi:column-layout 
                            '(account-layout limit-field))))

(capi:define-interface bank-interface ()
  ((bank-ref :initarg :bank-ref))
  (:menu-bar open-actions)
  (:menus
   (open-actions
    "Action"
    (("Open Account" :callback 'open-account-callback)
     ("Open Checking Account" :callback 'open-checking-account-callback)
     ("Retrieve Account" :callback 'retrieve-account-callback)
     ("Close Account" :callback 'close-account-callback))
    :callback-type :interface))
  (:layouts 
   (accounts-area capi:row-layout () 
                  :accessor accounts-area
                  :horizontal-scroll t))
  (:default-initargs :auto-menus nil :best-width 400))

(defmethod initialize-instance :after ((self account-interface)  &key)
  (with-slots (account-ref balance-field) self
    (when account-ref
      (setf (capi:title-pane-text balance-field)
            (format nil "~A" (op:balance account-ref))))))

(defmethod initialize-instance :after ((self checking-account-interface)  &key)
  (with-slots (account-ref limit-field) self
    (when account-ref
      (setf (capi:title-pane-text limit-field)
            (format nil "~A" (op:limit account-ref))))))

