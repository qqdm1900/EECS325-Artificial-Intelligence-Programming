;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/43/LISPcorba-doc-bank/RCS/client:callbacks.lisp,v 1.6.7.1 2011/08/24 13:25:59 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

(in-package "CL-USER")

(defun credit (self)
  (with-slots (balance-field account-ref) self
    (let ((amount (capi:prompt-for-integer "Amount?" :min 0)))
      (when amount
        (op:credit account-ref amount)
        (setf (capi:title-pane-text balance-field)
              (format nil "~A" (op:balance account-ref)))))))

(defun debit (self)
  (with-slots (balance-field account-ref) self
    (let ((amount (capi:prompt-for-integer "Amount?" :min 0)))
      (when amount
        (handler-case
            (progn
              (op:debit account-ref amount)
              (setf (capi:title-pane-text balance-field)
                    (format nil "~A" (op:balance account-ref))))
          (BankingDemo:account/refusal 
           (xx)
           (capi:display-message "Debit returned refusal with string: <~A>"
                                 (op:reason xx))))))))

(defun open-account-callback (self)
  (with-slots (bank-ref) self
    (let ((name (capi:prompt-for-string "Name?")))
      (when name
        (handler-case
            (let ((account-ref
                   (op:openaccount bank-ref name)))
              (make-account-frame account-ref :bank-interface self :title name))
          (Bankingdemo:Bank/DuplicateAccount 
           ()
           (capi:display-message "Cannot create another account for ~A" name)))))))


(defun open-checking-account-callback (self)
  (with-slots (bank-ref) self
    (let ((name (capi:prompt-for-string "Name?")))
      (when name
        (let ((limit (capi:prompt-for-integer "Limit?")))
          (when limit
            (handler-case
                (let ((account-ref
                       (op:opencheckingaccount bank-ref name limit)))
                  (make-account-frame account-ref :bank-interface self :title name))
              (Bankingdemo:Bank/DuplicateAccount 
               ()
               (capi:display-message "Cannot create another account for ~A" name)))))))))

(defun retrieve-account-callback (self)
  (with-slots (bank-ref) self
    (let ((name (capi:prompt-for-string "Name?")))
      (when name
        (if (find-named-frame self name)
            (capi:display-message "Already viewing it...")
          (handler-case
              (let ((account-ref
                     (op:retrieveaccount bank-ref name))) 
                (make-account-frame account-ref :bank-interface self :title name))
            (Bankingdemo:Bank/NonExistentAccount 
             ()
             (capi:display-message "No account exists for name ~A" name))))))))

(defun close-account-callback (self)
  (with-slots (bank-ref) self
    (let ((name (capi:prompt-with-list (all-frame-names self)
                                       "Choose account")))
      (when name
        (op:closeaccount bank-ref 
                         (with-slots (account-ref)
                             (find-named-frame self name)
                           account-ref))
        (remove-account-frame self name)))))

                                       
