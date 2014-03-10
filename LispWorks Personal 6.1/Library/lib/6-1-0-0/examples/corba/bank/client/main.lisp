;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/43/LISPcorba-doc-bank/RCS/client:main.lisp,v 1.3.13.1 2011/08/24 13:25:59 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

(in-package "CL-USER")

(defun bank-client ()
  (let ((orb (op:orb_init nil "Harlequin Common Lisp Orb")))
    (let ((bank-ref (op:narrow 'BankingDemo:bank
                               (file-to-object orb))))
      (capi:display (make-instance 'bank-interface 
                                   :bank-ref bank-ref
                                   :title "Corba Bank")))))
