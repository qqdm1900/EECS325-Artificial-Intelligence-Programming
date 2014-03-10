;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/applications:argument-passing.lisp,v 1.2.12.1 2011/08/24 13:26:21 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

;;;  An example of how to use the :MAKE-INSTANCE-EXTRA-APPLY-ARGS inside
;;; DEFINE-INTERFACE
;;----------------------------------------------------------------------------


(in-package "CL-USER")



(capi:define-interface an-interface ()
  ()
  
  (:panes
   (a-pane
    capi:title-pane
    :text "A string"
    :make-instance-extra-apply-args :a-pane-list-args)))



;;; In this call the TITLE-PANE come out with the background
;;; as defined by the resource file. 

(capi:display (make-instance 'an-interface :background :blue))


;;; In this call the TITLE-PANE come out with background yellow.

(capi:display (make-instance 'an-interface :background :blue
                        :a-pane-list-args '(:background :yellow)))
