;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/43/LISPcorba-doc-bank/RCS/client:utilities.lisp,v 1.2.13.1 2011/08/24 13:25:59 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

(in-package "CL-USER")

(defun push-new-item (bank-interface title item)
  (declare (ignore title))
  (setf (capi:layout-description (accounts-area bank-interface))
        (cons item (capi:layout-description (accounts-area bank-interface)))))

(defmethod make-account-frame ((self BankingDemo:account) &key bank-interface title)
  (push-new-item bank-interface
                 title
                 (make-instance 'account-interface 
                                :account-ref self
                                :account-name title
                                :owner bank-interface)))

(defmethod make-account-frame ((self BankingDemo:checkingAccount) &key bank-interface title)
  (push-new-item bank-interface
                 title
                 (make-instance 'checking-account-interface 
                                :account-ref self
                                :account-name title
                                :owner bank-interface)))
       

(defun remove-account-frame (self name)
  (setf (capi:layout-description (accounts-area self))
        (remove name
                (capi:layout-description (accounts-area self))
                :test 'string=
                :key 'account-name)))

(defun all-frame-names (self)
  (loop for data in (capi:layout-description (accounts-area self))
        collect (account-name data)))

(defun find-named-frame (self name)
  (find name (capi:layout-description (accounts-area self))
        :test 'string= :key 'account-name))
