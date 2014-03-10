;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/23/LISPexamples/RCS/delivery:macos:single-window-application.lisp,v 1.1.1.2 2011/09/12 16:33:04 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

;; This delivery script creates a standalone CAPI application for
;; processing a file with extension "lwtestsingle".

;; To build this application, use this file as the script in the 
;; Application Builder (or run the LispWorks executable from the
;; command line as described in the LispWorks Delivery User Guide).

;; This will create an application named "Single Window CAPI Application"
;; in your home directory.

;; The files in the "files" subdirectory can be used to test this
;; application.


(in-package "CL-USER")

(load-all-patches)

(defvar *target-application-path* "~/Single Window CAPI Application")

(compile-file
 (current-pathname "../../capi/applications/cocoa-application-single-window.lisp")
 :output-file :temp
 :load t)

;;; Note: As an alternative to CREATE-MACOS-APPLICATION-BUNDLE you
;;; could use the bundle creation code that is supplied with LispWorks
;;; in (example-file "configuration/macos-application-bundle.lisp")
;;; However, that should only be necessary if you want to modify that
;;; bundle creation code.

(deliver 'test-cocoa-application-single
         (create-macos-application-bundle
          *target-application-path*
          :template-bundle (pathname-location
                            (current-pathname "templates/SimpleApplication.app/")))
         0
         :interface :capi)

