;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/43/LISPcorba-doc-bank/RCS/server:servant-classes.lisp,v 1.4.12.1 2011/08/24 13:25:59 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

(in-package "CL-USER")

(defclass bank-implementation (BankingDemo:Bank-servant)
  ((connection :initarg :connection)
   (poa :initarg :poa)
   (account-impls :initform nil)))

(defclass account-implementation (BankingDemo:Account-servant)
  ((bank :initarg :bank)))

(defclass checkingaccount-implementation (Bankingdemo:Checkingaccount-servant account-implementation) ())

