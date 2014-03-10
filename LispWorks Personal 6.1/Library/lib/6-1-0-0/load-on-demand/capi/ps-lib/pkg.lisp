;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/9/LISPcapi-ps-lib/RCS/pkg.lisp,v 1.4.1.1 2011/08/24 13:25:40 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

(in-package "SYS")

(defpackage "CAPI-POSTSCRIPT-LIBRARY"
  (:nicknames "CAPI-PS-LIB")
  (:export "PS-PRINTER-PORT"
    "PS-PRINTER-PORT-ELEMENT"
    "PS-PRINTER-PORT-GSTATE"
    "PS-PRINTER-PORT-GSTATE-EXTRA"
    "PS-PRINTER-PORT-STACK"
    "PS-PRINTER-PORT-PROCSETS"
    "PS-PRINTER-PORT-FONTS-USED"
    "PS-PRINTER-PORT-GOOD-QUALITY-DRAWING-P"
    "PSOUT"
    "*PROCSET-STANDARD*"
    "*PROCSET-FDH*"
    "*PROCSET-QUERY*"


))
