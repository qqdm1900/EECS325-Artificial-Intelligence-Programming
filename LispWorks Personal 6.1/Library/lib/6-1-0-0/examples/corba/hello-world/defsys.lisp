;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/56/LISPcorba-doc-hello-world/RCS/defsys.lisp,v 1.2.13.1 2011/08/24 13:26:00 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

(in-package "CL-USER")

(require "corba-orb")

(defsystem hello-world-corba-object ()
  :members (
            ("hello-world" :type :idl-file)
            "shared"
            "hello-world-server"
            "hello-world-client"
            )
  :rules ((:in-order-to :compile :all
           (:requires (:load :previous)))))
