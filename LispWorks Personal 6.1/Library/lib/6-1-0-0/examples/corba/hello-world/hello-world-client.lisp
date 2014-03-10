;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/56/LISPcorba-doc-hello-world/RCS/hello-world-client.lisp,v 1.3.13.1 2011/08/24 13:26:00 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

(in-package "CL-USER")

(defun run-client ()
  (let ((orb (op:orb_init nil "Harlequin Common Lisp Orb")))
    (let ((world (op:narrow 'HelloWorld:world (file-to-object orb))))
      (format t "~S~%" (op:hello world)))))
