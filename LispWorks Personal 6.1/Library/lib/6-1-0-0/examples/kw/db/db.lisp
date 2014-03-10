;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/58/KWdemos/RCS/db:db.lisp,v 1.2.8.1 2011/08/24 13:25:19 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

;;; To run, call (make-vehicles-and-people)
;;; and then (kw:infer :contexts '(database-example)).
;;; To rerun, call (kw:reset) and repeat as before.


(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "kw-sql"))

(in-package "KW-USER")


;;; the vehicle class maps onto the vehicle table in the database
;;; owner is a join slot which looks up the owner person object

(sql:def-view-class vehicle (sql:standard-db-object standard-kb-object)
  ((number-plate :accessor vehicle-number-plate :type (string 8)
          :db-kind :key :column plate)
   (make :accessor vehicle-make :type (string 20)
         :db-kind :base :column make)
   (price :accessor vehicle-price :type integer
          :db-kind :base :column price)
   (owner-name :type (string 20)
               :db-kind :base :column owner)
   (owner :accessor vehicle-owner :db-kind :join
          :db-info (:home-key owner-name
                    :foreign-key name
                    :join-class person
                    :set nil
                    :retrieval :deferred))))

;;; the person class maps onto the person table in the database
;;; vehicle is a join slot which looks up the owned vehicle object
;;; company is a join slot which looks up the company object

(sql:def-view-class person (sql:standard-db-object standard-kb-object)
  ((name :accessor person-name :type (string 20)
         :db-kind :key :column name)
   (salary :accessor person-salary :type integer
           :db-kind :base :column salary)
   (vehicle-number-plate :type (string 8)
                         :db-kind :base :column vehicle)
   (vehicle :accessor person-vehicle :db-kind :join
            :db-info (:home-key vehicle-number-plate
                      :foreign-key number-plate
                      :join-class vehicle
                      :set nil
                      :retrieval :deferred))
   (employer :type (string 20)
             :db-kind :base :column employer)
   (company :accessor person-company :db-kind :join
            :db-info (:home-key employer
                      :foreign-key name
                      :join-class company
                      :set nil
                      :retrieval :deferred))))

;;; the company class maps onto the company table in the database

(sql:def-view-class company (sql:standard-db-object standard-kb-object)
  ((name :accessor company-name :type (string 20)
         :db-kind :key :column name)
   (product :accessor company-product :type (string 10)
            :db-kind :base :column product)))

;;; here we assume we have a database connected with the correct
;;; data in it - if we do we retrieve all the person and vehicle objects
;;; but company objects will be retrieved only when needed by
;;; by querying the company slot of the person objects

(defun make-vehicles-and-people ()
  (if sql:*default-database*
      (progn
        (sql:select 'vehicle)
        (sql:select 'person)
        t)
    (error "~%Please connect to a database with contents created by ~
             file data.sql")))

;;; to store which vehicles a person can drive

(def-kb-struct vehicles-for-person person vehicles)

(defcontext database-example :strategy (priority))

;;; for every person initialise the list of vehicles they can drive

(defrule init-vehicles-for-person :forward
  :context database-example
  (person ?person vehicle nil)
  -->
  (assert (vehicles-for-person ? person ?person vehicles nil)))

;;; for every vehicle a person can drive which hasn't yet been
;;; included in the list, add it to the list

(defrule vehicle-for-person :forward
  :context database-example
  (person ?person vehicle nil)
  (vehicle ?vehicle owner nil)
  (vehicles-for-person ?c-f-p person ?person vehicles ?vehicles)
  (test (not (member ?vehicle ?vehicles)))   ; has it been included?
  -->
  (vehicle-ok-for-person ?vehicle ?person)   ; check if ok to drive vehicle
  (assert (vehicles-for-person ?c-f-p vehicles (?vehicle . ?vehicles))))

;;; rules expressing what vehicles a person can drive:
;;; if they have no employer they can only drive a skoda
;;; otherwise they will refuse to drive a skoda.
;;; anyone will drive a rolls or a jag.
;;; they'll only drive a ford or vauxhall if salary is less
;;; than 40k.

(defrule vehicle-ok-for-person :backward
  ((vehicle-ok-for-person ?vehicle ?person)
   <--
   (person ?person company nil)
   (cut)
   (vehicle ?vehicle make "SKODA"))
  ((vehicle-ok-for-person ?vehicle ?person)
   <--
   (vehicle ?vehicle make "SKODA")
   (cut)
   (fail))
  ((vehicle-ok-for-person ?vehicle ?person)
   <--
   (or (vehicle ?vehicle make "ROLLS")
       (vehicle ?vehicle make "JAGUAR"))
   (cut))
  ((vehicle-ok-for-person ?vehicle ?person)
   <--
   (or (vehicle ?vehicle make "VAUXHALL")
       (vehicle ?vehicle make "FORD"))
   (person ?person salary ?salary)
   (test (< ?salary 40000))))

;;; next to rules are just simple allocation rules, trying
;;; out each possibility until one fits

(defrule alloc-vehicles-to-persons :backward
  ((alloc-vehicles-to-persons ?allocs)
   <--
   (alloc-internal nil nil nil ?allocs)))

(defrule alloc-internal :backward
  ((alloc-internal ?done-persons ?done-vehicles ?allocs ?allocs)
   <--
   (not (and (vehicles-for-person ? person ?person)
             (not (member ?person ?done-persons))))
   (cut))
  ((alloc-internal ?done-persons ?done-vehicles ?allocs-so-far ?allocs)
   <--
   (vehicles-for-person ? person ?person vehicles ?vehicles)
   (not (member ?person ?done-persons))
   (member ?vehicle ?vehicles)
   (not (member ?vehicle ?done-vehicles))
   (alloc-internal (?person . ?done-persons) (?vehicle . ?done-vehicles)
                   ((?person . ?vehicle) . ?allocs-so-far) ?allocs)))

;;; find a solution and print it out

(defrule find-solution :forward
  :context database-example
  :priority 5
  (not (not (vehicles-for-person ?)))
  -->
  (alloc-vehicles-to-persons ?solution)
  ((dolist (pair ?solution)
     (format t "~%~A drives ~A"
            (person-name (car pair)) (vehicle-number-plate (cdr pair))))))

