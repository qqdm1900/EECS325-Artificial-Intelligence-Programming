;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/23/LISPexamples/RCS/delivery:debugger-hook:deliver.lisp,v 1.2.1.1 2011/08/24 13:26:19 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

;;; An example of catching errors in a delivered application. 

;;; See the comments in "application-with-errors.lisp" in this directory.

;;; Use this file as the delivery script in the Application
;;; Builder or on the command line with -build. 

;;; See the Delivery User Guide for details of how to deliver.


(in-package "CL-USER")

(load-all-patches)


;;; Load the "application". 
(compile-file (current-pathname "application-with-errors.lisp" nil) 
              :output-file :temp
              :load t)

;;; Deliver the application.
;;; Because it is interactive, we need to create the bundle on Cocoa.

(let ((save-image-name 
       #-cocoa "~/application-with-errors" 
       #+cocoa (and (save-argument-real-p)
                    (create-macos-application-bundle "~/application-with-errors"))))
  (deliver 'main-function save-image-name 0
           :interface :capi))

