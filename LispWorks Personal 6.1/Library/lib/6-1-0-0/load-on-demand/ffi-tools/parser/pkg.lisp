;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/5/LISPffi-tools-parser/RCS/pkg.lisp,v 1.6.9.1 2011/08/24 13:26:29 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

(in-package "SYSTEM")


;;; make sure all these symbols actually exist
#-(or lucid harlequin-pc-lisp)
(map nil 'identity
     '(
      ffi::PROCESS-C-FILE
      ffi::PROCESS-FOREIGN-FILE
      ffi::PROCESS-CPP-FILE
      ffi::*PREPROCESSOR*
      ffi::*PREPROCESSOR-OPTIONS*
      ffi::*CASE-SENSITIVE*
      ffi::*C-CODE-CONTROL*
      ffi::*C++-INFORM-USERS*
      ffi::*PROCESSOR-OUTPUT-FUNCTION*
      ffi::c++-destroy
      ffi::process-foreign-code))
#+lucid
(map nil 'identity
     '(system::PROCESS-C-FILE system::PROCESS-FOREIGN-FILE system::PROCESS-CPP-FILE)
    )

(defpackage "FOREIGN-PARSER"

  #-(or lucid harlequin-pc-lisp)
  (:import-from "FFI" 
   "PROCESS-C-FILE"
   "C++-DESTROY"
   "PROCESS-FOREIGN-FILE"
   "PROCESS-CPP-FILE"
   "*PREPROCESSOR*"
   "*PREPROCESSOR-OPTIONS*"
   "*CASE-SENSITIVE*"
   "*C-CODE-CONTROL*"
   "*C++-INFORM-USERS*"
   "*PROCESSOR-OUTPUT-FUNCTION*"
   "PROCESS-FOREIGN-CODE"
   )
  #+lucid
   (:import-from "SYSTEM" 
   "PROCESS-C-FILE"

   "PROCESS-FOREIGN-FILE"
   "PROCESS-CPP-FILE"
   )
  
  (:import-from "FLI" "*LANGUAGE*")
  (:export 
   "PROCESS-FOREIGN-CODE"
   "PROCESS-C-FILE"
   "PROCESS-FOREIGN-FILE"
   "PROCESS-CPP-FILE"
   "*PREPROCESSOR-OPTIONS*"
   "*PREPROCESSOR-FORMAT-STRING*"
   "*C++-INFORM-USERS*"
   "C++-DISPATCH"
   "*C++-COMPILER*"
   "*PREPROCESSOR*"
   "C++-DESTROY"
   )
  
  )


        
               
  
