;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/43/LISPcorba-doc-bank/RCS/server:database.lisp,v 1.1.13.1 2011/08/24 13:25:59 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

(in-package "CL-USER")

(defstruct database-row balance limit)

(defun connect-to-database ()
  (make-hash-table :test 'equalp))

(defun disconnect-from-database ())

(defun create-database-row (name connection)
  (setf (gethash name connection) (make-database-row)))

(defun find-database-row (name connection)
  (not (not (gethash name connection))))

(defun delete-database-row (name connection)
  (remhash name connection))

(defun update-database-row (name connection &key balance limit)
  (let ((row (gethash name connection)))
    (unless row (error "Unable to update row ~A as there is no such row in the database" name))
    (when balance (setf (database-row-balance row) balance))
    (when limit (setf (database-row-limit row) limit))
    (setf (gethash name connection) row)))

(defun lookup-row-value (name connection field)
  (let ((row (gethash name connection)))
    (unless row (error "Unable to update row ~A as there is no such row in the database" name))
    (ecase field
      (:balance (database-row-balance row))
      (:limit (database-row-limit row)))))
