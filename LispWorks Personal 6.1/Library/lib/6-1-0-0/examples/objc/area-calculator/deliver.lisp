;; -*- rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/23/LISPexamples/RCS/objc:area-calculator:deliver.lisp,v 1.1.2.3 2012/01/16 12:17:58 davef Exp $" -*-

;;----------------------------------------------------------------------------
;;
;; examples/objc/deliver.lisp
;;
;; This example demonstrates how to deliver the AreaCalculator example
;; as a standalone executable.
;;
;; Use it with the Application Builder.
;;
;;----------------------------------------------------------------------------
;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
;;----------------------------------------------------------------------------


(in-package "CL-USER")

(load-all-patches)

(compile-file (current-pathname "area-calculator") 
              :output-file :temp
              :load t)

(defvar *delivered-image-name* "~/AreaCalculator")

;;; Note: As an alternative to CREATE-MACOS-APPLICATION-BUNDLE you
;;; could use the bundle creation code that is supplied with LispWorks
;;; in (example-file "configuration/macos-application-bundle.lisp")
;;; However, that should only be necessary if you want to modify that
;;; bundle creation code.

(when (save-argument-real-p)
  (setq *delivered-image-name*
        (create-macos-application-bundle "~/AreaCalculator.app"   
                                         :document-types nil))
  ;; Copy the nib into the application bundle's Resources directory.
  (let ((source (sys:directory-pathname
                 (current-pathname *area-calculator-nib-file*)))
        (dest (sys:directory-pathname
               (merge-pathnames *area-calculator-nib-file*
                                (merge-pathnames "../Resources/"
                                                 *delivered-image-name*)))))
    (dolist (file (directory dest))
      (delete-file file))
    (delete-directory dest :no-error)
    (ensure-directories-exist dest)
    (dolist (file (directory source))
      (copy-file file 
                 (merge-pathnames (make-pathname :name (pathname-name file) 
                                                 :type (pathname-type file))
                                  dest)))))

(deliver 'area-calculator-main *delivered-image-name* 5
         :keep-symbols '(area-controller))
