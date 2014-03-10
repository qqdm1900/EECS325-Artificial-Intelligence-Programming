;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/58/KWdemos/RCS/whist:defpkg.lisp,v 1.1.13.1 2011/08/24 13:25:19 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

(in-package kw-user)

(defpackage "WHIST"
  (:shadow "STATE")
  (:add-use-defaults t)
  (:use "KW")
  (:export "RUN-WHIST"))
