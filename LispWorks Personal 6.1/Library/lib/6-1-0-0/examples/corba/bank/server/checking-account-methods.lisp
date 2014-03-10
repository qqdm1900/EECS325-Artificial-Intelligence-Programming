;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/43/LISPcorba-doc-bank/RCS/server:checking-account-methods.lisp,v 1.2.13.1 2011/08/24 13:25:59 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

(in-package "CL-USER")

(corba:define-method op:debit ((self checkingAccount-implementation) amount)
  (with-slots (op:name bank op:balance) self
    (with-slots (connection) bank
      (let ((old-balance (lookup-row-value op:name connection :balance))
            (limit (lookup-row-value op:name connection :limit)))
        (if (< (+ old-balance limit) amount)
            (error 'BankingDemo:Account/Refusal 
                   :reason (format nil "Can't debit ~A because the balance is ~A (limit is ~A)." 
                                   amount old-balance limit))
          (update-database-row op:name connection 
                               :balance (setf op:balance (- old-balance amount))))))))
