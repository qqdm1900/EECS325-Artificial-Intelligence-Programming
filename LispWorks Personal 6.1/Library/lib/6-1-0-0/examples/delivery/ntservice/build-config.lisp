;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/23/LISPexamples/RCS/delivery:ntservice:build-config.lisp,v 1.3.1.1 2011/08/24 13:26:19 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

;;; Build time configuration parameters for the example service.

(in-package "USER")

;;; The name of the file where the "service" writes out whatever it has to say. 

(defparameter *output-file-name* "testapp-lw-out" )

;;; The full path of the output file. 

(defun output-file-full-path ()
  (merge-pathnames *output-file-name* (sys:get-folder-path :common-documents)))


;;; Used in deliver.lisp as the name of the file to save in. 

(defconstant *my-test-service-executable*
  (merge-pathnames "testapp-service.exe" (sys:get-folder-path :documents)))

;;; That prints in the Windows way, which can be executed in a console. 

(defun print-executable-name ()
  (princ (namestring *my-test-service-exectutable*)))

;;; The default access setting on the configuration pipe
;;; :AU is authorized users. 
;;; For details, see "SID strings" in MSDN
;;; http://msdn.microsoft.com/en-us/library/aa379602%28v=VS.85%29.aspx

(defparameter *default-access-setting* :au)

;;; Timeout in seconds. The access reverts back to 
;;; to the default after this period of time. 

(defparameter *configuration-timeout* 10)

;;; The name of the pipe that is used to communicate to the service.
;;; It really should be a string that is "guaranteed" to be unique,
;;; like a specially made GUID. 

(defparameter *configuration-pipe-name* "testapp-lw-configuration")

;;; The name of the service, used in define-service.lisp
;;; That is the name you use when using "net start <name>" and
;;; "net stop <name>" to start and stop the service.

(defparameter *service-name* "my-test-service")

;;; The display name of the service, used in define-service.lisp
;;; That is the name you see in the Services window. 

(defparameter *service-display-name* "My Test Service")


;;; Time in seconds of each iteration in the main loop. 
;;; Just affect how many lines of the form
;;;    "Report at 2011/03/31 14:12:34: iteration 3 "
;;; you get in the file

(defparameter *iteration-time* 100)



