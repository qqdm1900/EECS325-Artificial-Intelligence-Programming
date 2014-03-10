;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/23/LISPexamples/RCS/delivery:othello:deliver.lisp,v 1.5.1.3 2011/09/12 16:31:41 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.


(in-package "CL-USER")

(load-all-patches)

;;; Where we are going to deliver the image. 

(defvar *delivered-image-name* "~/othello")

;;; Load the "application". 

(compile-file (current-pathname "../../capi/applications/othello") 
              :output-file :temp
              :load t)

;;; On Cocoa it is a little bit more complicated, because we need to
;;; create an application bundle. We create the bundle and set
;;; *DELIVERED-IMAGE-NAME* to the value that this returns. We avoid
;;; copying the LispWorks file type associations by passing
;;; :DOCUMENT-TYPES NIL.  When the script is used to create a
;;; universal binary, it is called more than once. To avoid creating
;;; the bundle more than once, we check the result of
;;; SAVE-ARGUMENT-REAL-P before creating the bundle.

;;; Note: As an alternative to CREATE-MACOS-APPLICATION-BUNDLE you
;;; could use the bundle creation code that is supplied with LispWorks
;;; in (example-file "configuration/macos-application-bundle.lisp")
;;; However, that should only be necessary if you want to modify that
;;; bundle creation code.

#+cocoa
(when (save-argument-real-p)
  (setq *delivered-image-name*
        (create-macos-application-bundle "~/Othello.app"   
                                         :document-types nil)))  

;; Deliver.

(deliver 'play-othello *delivered-image-name* 5 :interface :capi)


