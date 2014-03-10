;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/23/LISPexamples/RCS/editor:commands:insert-date.lisp,v 1.2.13.1 2011/08/24 13:26:19 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

(in-package "CL-USER")

;;----------------------------------------------------------------------------
;;
;; examples/editor/commands/insert-date.lisp
;;
;; Example of how to insert text at the current point.
;;
;; This example demonstrates the uses of
;;    editor:bind-key, editor:current-point,
;;    editor:defcommand, editor:insert-string
;;
;; To use either press Meta-Shift-d or call the editor
;; command "Insert Date"


(defvar *month-names* '("Jan" "Feb" "Mar" "Apr"
                        "May" "Jun" "Jul" "Aug"
                        "Sep" "Oct" "Nov" "Dec"))

(editor:defcommand "Insert Date" (p)
     "Insert today's date at the editor current point"
     "Insert today's date at the editor current point"
  (declare (ignore p))
  (multiple-value-bind
      (seconds minutes hours day month year)
      (get-decoded-time)
    (declare (ignore seconds minutes hours))
    (editor:insert-string
     (editor:current-point)
     (format nil "~2,'0D~A~4D"
             day (nth (1- month) *month-names*) year))))

;;; Meta Shift D invokes the command
(editor:bind-key "Insert Date" #\meta-d)
