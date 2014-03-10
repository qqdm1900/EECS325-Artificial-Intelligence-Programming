;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/43/LISPcorba-doc-bank/RCS/server:interface.lisp,v 1.2.13.1 2011/08/24 13:25:59 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

(in-package "CL-USER")

(capi:define-interface server-controller () 
  ((bank-ref :initarg :bank-ref)
   (bank-poa :initarg :bank-poa))
  (:panes
   (stop-button capi:push-button
                :text "Stop server"
                :callback 'stop-server-callback
                :callback-type :interface))
  (:default-initargs 
   :auto-menus nil
   :title "Server controller"))

(defun stop-server-callback (self)
  (with-slots (bank-ref bank-poa) self
    (let ((bank-servant (op:reference_to_servant bank-poa bank-ref)))
      (with-slots (account-impls) bank-servant
        (dolist (account-impl account-impls)
          (op:deactivate_object bank-poa (op:servant_to_id bank-poa account-impl)))))
    (stop-server bank-poa bank-ref))
  (capi:quit-interface self))
    

