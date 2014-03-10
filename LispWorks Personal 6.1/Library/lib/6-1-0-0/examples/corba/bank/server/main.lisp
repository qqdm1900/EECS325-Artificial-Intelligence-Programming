;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/43/LISPcorba-doc-bank/RCS/server:main.lisp,v 1.3.13.1 2011/08/24 13:25:59 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

(in-package "CL-USER")

(defun bank-server ()
  (let* ((orb (op:orb_init nil "Harlequin Common Lisp Orb"))
         (rootPOA (op:resolve_initial_references orb "RootPOA")))
    (let ((bank-impl (make-instance 'bank-implementation 
                                   :connection (connect-to-database)
                                   :poa rootPOA)))
      (let ((bank-ref (op:servant_to_reference rootPOA bank-impl)))
        (object-to-file orb bank-ref)
        (capi:display (make-instance 'server-controller
                                     :bank-poa rootPOA
                                     :bank-ref bank-ref)))
      (op:activate (op:the_poamanager rootPOA)))))

(defun stop-server (bank-poa bank-ref)
  (op:deactivate_object bank-poa 
                        (op:reference_to_id bank-poa bank-ref)))

        
         
