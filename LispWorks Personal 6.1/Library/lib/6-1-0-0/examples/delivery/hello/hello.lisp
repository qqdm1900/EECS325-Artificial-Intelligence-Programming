;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/23/LISPexamples/RCS/delivery:hello:hello.lisp,v 1.2.8.1 2011/08/24 13:26:18 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

(in-package "CL-USER")

;;; Say Hello World, unless given an argument in which case 
;;; says hello to this argument. 

(defun hello-world ()
  (capi:display-message "Hello ~a!" (or (second sys:*line-arguments-list*) "World"))
  (quit))
