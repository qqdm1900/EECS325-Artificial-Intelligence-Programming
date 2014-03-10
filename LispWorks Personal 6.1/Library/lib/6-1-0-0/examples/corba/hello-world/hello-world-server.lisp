;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/56/LISPcorba-doc-hello-world/RCS/hello-world-server.lisp,v 1.5.12.1 2011/08/24 13:26:00 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

(in-package "CL-USER")

(defclass world-implementation (HelloWorld:world-servant) ())

(corba:define-method op:hello ((self world-implementation))
  (declare (ignore self))
  "Hello World!")

(defun server-startup ()
  (let* ((orb (op:orb_init nil "Harlequin Common Lisp Orb"))
         (poa (op:resolve_initial_references orb "RootPOA"))
         (impl (make-instance 'world-implementation))
         (world (op:narrow 'HelloWorld:world
                           (op:servant_to_reference poa impl))))
    (object-to-file orb world)
    (let ((manager (op:the_poamanager poa)))
      (op:activate manager))))


    
