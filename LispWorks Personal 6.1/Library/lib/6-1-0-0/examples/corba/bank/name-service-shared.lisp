;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/43/LISPcorba-doc-bank/RCS/name-service-shared.lisp,v 1.1.4.1 2011/08/24 13:25:59 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

;; An alternative way to store the Bank's object reference, using an
;; NameService daemon.  The server and client must be configured with
;; suitable initargs to locate the NameService, for example
;;
;; -ORBInitRef NameService=corbaloc::your-hostname:1049/NameService

(defparameter *name-lookup-place* 
  (list (CosNaming:NameComponent :id "Current Bank" :kind "Bank")))

(defparameter *name-service* nil)

(defun get-name-service (orb)
  (or *name-service*
      (let ((ref (op:resolve_initial_references orb "NameService")))
        (and ref
             (setq *name-service*
                   (op:narrow 'cosnaming:namingcontext ref))))))

;; Call this instead of object-to-file.
(defun set-name-service-value (orb object-ref)
  (let ((name-service (get-name-service orb)))
    (unless name-service 
      (warn "No name service found")
      (return-from set-name-service-value nil))
    (handler-case
        (op:bind name-service 
                 *name-lookup-place*
                 object-ref)
      (Cosnaming:Namingcontext/Alreadybound 
       ()
       (op:rebind name-service 
                  *name-lookup-place*
                  object-ref)))))

;; Call this instead of file-to-object.
(defun get-name-service-value (orb)
  (let ((name-service (get-name-service orb)))
    (unless name-service 
      (warn "No name service found")
      (return-from get-name-service-value nil))
    (handler-case
        (op:narrow 'BankingDemo:Bank
                   (op:resolve name-service 
                               *name-lookup-place*))
      (Cosnaming:Namingcontext/Notfound nil))))

;; Could call this when the server is shut down.
(defun remove-name-service-value (orb)
  (let ((name-service (get-name-service orb)))
    (unless name-service 
      (warn "No name service found")
      (return-from remove-name-service-value nil))
    (handler-case
        (op:unbind name-service 
                   *name-lookup-place*)
      (Cosnaming:Namingcontext/NotFound nil))))
