;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/layouts:resize-layout.lisp,v 1.10.12.1 2011/08/24 13:26:20 davef Exp $" -*-

;;----------------------------------------------------------------------------
;;
;; examples/capi/layouts/resize-layout.lisp
;;
;; This example demonstrates dynamic resizing of layouts.
;;
;;      (CL-USER::TEST-OUTPUT-PANE-RESIZE)
;;
;;----------------------------------------------------------------------------
;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
;;----------------------------------------------------------------------------

(in-package "CL-USER")

(defun display-circle (self x y width height)
  (declare (ignore x y width height))
  (multiple-value-bind (pane-width pane-height)
      (capi:simple-pane-visible-size self)
    (gp:clear-rectangle self 0 0 pane-width pane-height)
    (gp:draw-ellipse self
                     (ash pane-width -1) (ash pane-height -1) 
                     (1- (ash pane-width -1)) (1- (ash pane-height -1))
                     :filled t)))

(defun circle-pane (foreground background)
  (make-instance 'capi:output-pane
		 :foreground foreground
		 :background background
		 :display-callback 'display-circle
		 :resize-callback 'display-circle))

(defun test-output-pane-resize ()
  (capi:contain
   (make-instance 'capi:row-layout
                  :description
                  (list 
                   (make-instance 
                    'capi:column-layout
                    :description (list (circle-pane :green :red)
                                       :divider
                                       (circle-pane :yellow :blue)))
                   :divider
                   (make-instance 
                    'capi:column-layout
                    :description (list (circle-pane :green :yellow)
                                       :divider
                                       (circle-pane :yellow :orange)))
                   :divider
                   (make-instance 
                    'capi:column-layout 
                    :description (list (circle-pane :blue :yellow)
                                       :divider
                                       (circle-pane :red  :green)))))))





