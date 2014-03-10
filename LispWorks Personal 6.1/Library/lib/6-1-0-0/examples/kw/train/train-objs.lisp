;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/58/KWdemos/RCS/train:train-objs.lisp,v 1.1.13.1 2011/08/24 13:25:19 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

(in-package kw-user)

;;; 10 signals

(dotimes (count 11)
  (make-instance 'signal :position count :colour 'red))

;;; and a train

(make-instance 'train :position 0)

