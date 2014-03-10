;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/23/LISPexamples/RCS/configuration:save-macos-application.lisp,v 1.9.8.1 2011/08/24 13:26:18 davef Exp $" -*-

;;----------------------------------------------------------------------------
;;
;; examples/configuration/save-macos-application.lisp
;;
;; This example creates a Mac OS application bundle.
;;
;; You can use it to create preconfigured LispWorks image.
;; 
;; This example assumes that you have created a file
;; ~/my-configuration.lisp, which contains the settings
;; which you want to make. Typically it will start as a copy
;; of config/config.lisp, and then modified as desired.
;;
;; NB: When building an application, pass :DOCUMENT-TYPES NIL to
;; WRITE-MACOS-APPLICATION-BUNDLE to ensure that the application does
;; not claim to edit Lisp source files.
;;
;;----------------------------------------------------------------------------
;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
;;----------------------------------------------------------------------------

(in-package "CL-USER")

(load-all-patches)

;; Load the file contains your configuration settings. 
(load "~/my-configuration.lisp") 

(compile-file (current-pathname "macos-application-bundle")
              :output-file :temp
              :load t)

(defvar *new-bundle-name*
  "~/My LispWorks.app"
  #|
  Alternative form which writes the executable into the LispWorks folder.
  (merge-pathnames "My LispWorks.app"
                   (executable-application-bundle-directory
                    :parent t))
  |#
  )

;;; When building a universal binary, this script will be called
;;; more than once, but we don't want to call write-macos-application-bundle
;;; more than once. To avoid that, we call write-macos-application-bundle
;;; only when  SAVE-ARGUMENT-REAL-P return T. 

(save-image (when (save-argument-real-p)
              (write-macos-application-bundle *new-bundle-name*)))

#|

This script can be used "as is" in the application builder to 
build ordinary ("thin") images or build universal binaries.
To build a universal "by hand", use SAVE-UNIVERSAL-FROM-SCRIPT:

(progn 
  (compile-file 
   (example-file "configuration/macos-application-bundle") 
   :output-file :temp
   :load t)
  (SAVE-UNIVERSAL-FROM-SCRIPT 
   (write-macos-application-bundle
    "~/My LispWorks.app"
    #|
    Alternative form which writes the executable into the LispWorks folder.
    (merge-pathnames "My LispWorks.app"
                     (executable-application-bundle-directory
                      :parent t))
    |#
    )
   (example-file 
    "configuration/save-macos-application.lisp") ; the current script 
   )
  )

Note that SAVE-UNIVERSAL-FROM-SCRIPT executes the script twice,
but in both cases SAVE-ARGUMENT-REAL-P will return NIL, so we need
to call WRITE-MACOS-APPLICATION-BUNDLE here.

|#
