;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/9/LISPcapi-ps-lib/RCS/lod-defsys.lisp,v 1.4.12.1 2011/08/24 13:25:40 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

(in-package "SYS")

(defsystem capi-ps-lib-afm ()
  :members
  (
   encodings
   afm
   )
  :rules
  (
   (:in-order-to :compile :all (:requires (:load :previous)))
   )
  )

(defsystem capi-ps-lib (:optimize ((fixnum-safety 3))) ;MJS 06May98: uses floats
  :members
  (
   (pkg :source-only t)
   dirs
   ppd-parse
   ppd-language
   ppd-process
   ppd-constraints
   psout
   printers
   save-printers
   load-printers
   document
   print-dialogs
   printer-configure
   query
   (capi-ps-lib-afm :type :system)
   preload-35-afms
   )
  :rules
  (
   (:in-order-to :compile :all (:requires (:load :previous)))
   )
  )



