;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/43/LISPcorba-doc-bank/RCS/client:defsys.lisp,v 1.3.12.1 2011/08/24 13:25:59 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

(in-package "CL-USER")

(require "corba-orb")

(defsystem bank-client ()
  :members
  (
   ("../bank.idl" :type :idl-file)
   "../shared"
   "interfaces"
   "utilities"
   "callbacks"
   "main"
   )
  :rules ((:in-order-to :compile :all
           (:requires (:load :previous)))))

