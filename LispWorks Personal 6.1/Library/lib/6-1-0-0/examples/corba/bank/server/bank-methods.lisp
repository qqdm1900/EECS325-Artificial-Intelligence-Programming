;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/43/LISPcorba-doc-bank/RCS/server:bank-methods.lisp,v 1.5.12.1 2011/08/24 13:25:59 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

(in-package "CL-USER")

(corba:define-method op:openAccount ((self bank-implementation) name)
  (with-slots (connection poa account-impls) self
    (when (find-database-row name connection)
      (error 'Bankingdemo:Bank/Duplicateaccount))
    (create-database-row name connection)
    (update-database-row name connection :balance 0)
    (let ((new-account (make-instance 'account-implementation
                                      :name name
                                      :bank self
                                      :balance 0)))
      (push new-account account-impls)
      (op:narrow 'BankingDemo:Account
                 (op:servant_to_reference poa new-account)))))

(corba:define-method op:openCheckingAccount ((self bank-implementation) name limit)
  (with-slots (connection poa account-impls) self
    (when (find-database-row name connection)
      (error 'Bankingdemo:Bank/Duplicateaccount))
    (create-database-row name connection)
    (update-database-row name connection :balance 0 :limit limit)
    (let ((new-account (make-instance 'checkingaccount-implementation
                                      :name name
                                      :bank self
                                      :balance 0
                                      :limit limit)))
      (push new-account account-impls)
      (op:narrow 'Bankingdemo:Checkingaccount
                 (op:servant_to_reference poa new-account)))))

(corba:define-method op:retrieveAccount ((self bank-implementation) name)
  (with-slots (connection poa account-impls) self
    (unless (find-database-row name connection)
      (error 'Bankingdemo:Bank/NonExistentAccount))
    (let ((limit (lookup-row-value name connection :limit))
          (balance (lookup-row-value name connection :balance)))
      (if (not limit)
          (let ((account (make-instance 'account-implementation :name name :bank self :balance balance)))
            (push account account-impls)
            (op:narrow 'BankingDemo:Account
                       (op:servant_to_reference
                        poa
                        account)))
        (let ((account (make-instance 'checkingaccount-implementation 
                                      :name name
                                      :bank self
                                      :balance balance
                                      :limit limit)))
          (push account account-impls)
          (op:narrow 'Bankingdemo:Checkingaccount
                     (op:servant_to_reference
                      poa
                      account)))))))

(corba:define-method op:closeaccount ((self bank-implementation) account)
  (with-slots (connection poa account-impls) self
    (let ((servant (op:reference_to_servant poa account)))
      (op:deactivate_object poa (op:reference_to_id poa account))
      (removef account-impls servant)
      (with-slots (op:name) servant
        (delete-database-row op:name connection)))))
