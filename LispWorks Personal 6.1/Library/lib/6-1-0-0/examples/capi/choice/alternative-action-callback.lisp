;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/choice:alternative-action-callback.lisp,v 1.3.1.1 2011/08/24 13:26:20 davef Exp $" -*-

;;----------------------------------------------------------------------------
;;
;; examples/capi/choice/alternative-action-callback.lisp
;;
;; A simple example of using :ALTERNATIVE-ACTION-CALLBACK in a choice (here 
;; capi:multi-column-list-panel).
;; To try it, compile and load this file and then execute:
;;
;;      (CL-USER::TEST-ALTERNATIVE-ACTION-CALLBACK)
;;
;; This displays a pane showing Days and Cups number. The Action
;; gesture (double-click or Return) causes the Cups number to
;; increase.
;; The Alternative Action gesture (the same but with Shift key on
;; Windows or GTK+, Command key on Cococa) causes the number to
;; decrease.
;;
;; This example also demonstrates using CAPI:CHOICE-UPDATE-ITEM to
;; make an item redisplay when the underlying data changes.
;;----------------------------------------------------------------------------
;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
;;----------------------------------------------------------------------------


(in-package "CL-USER")

;;; Each item in the list is one of these. 
(defstruct day-and-cups-number
  day
  number)

;;; This creates the initail items in the list. 

(defun make-days ()
  (list (make-day-and-cups-number :day "Monday" :number 3)
        (make-day-and-cups-number :day "Tuesday" :number 1)
        (make-day-and-cups-number :day "Wednesday" :number 206)
        (make-day-and-cups-number :day "Thursday" :number 4)
        (make-day-and-cups-number :day "Friday" :number 5)
        (make-day-and-cups-number :day "Saturday" :number 8)
        (make-day-and-cups-number :day "Sunday" :number 0)))

;;; The action callback. 
(defun increment-cup-number (choice item)
  (incf (day-and-cups-number-number item))
  (capi:choice-update-item choice item))

;;; The alternative action callback 
(defun decrement-cup-number (choice item)
  (if (zerop (day-and-cups-number-number item))
      (capi:display-message "Cannot have negative cups number.")
  (decf (day-and-cups-number-number item)))
  (capi:choice-update-item choice item))

(defun test-alternative-action-callback ()
  (let ((choice (make-instance 'capi:multi-column-list-panel
                               :columns '((:title "Day") 
                                          (:title "Cups no." 
                                           :visible-min-width (character 10)))
                               :column-function
                               #'(lambda (item) (list (day-and-cups-number-day item)
                                                      (day-and-cups-number-number item)))
                               :items (make-days)
                               :callback-type :collection-data
                               :action-callback 'increment-cup-number
                               :alternative-action-callback 'decrement-cup-number)))
    (capi:contain choice)))

