;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/output-panes:scrolling-without-bar.lisp,v 1.3.9.1 2011/08/24 13:26:19 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

;;; ----------------------------------------------------------------------
;;;
;;; This example demonstrates scrolling without a visible scroll-bar.
;;; To try it, compile and load this file and then execute:
;;;
;;;      (CL-USER::TEST-SCROLLING-PINBOARDS)
;;;
;;; Moving the scroll-bar will cause both pinboards to scroll.
;;;
;;; ----------------------------------------------------------------------

(in-package "CL-USER")

(defun test-scrolling-pinboards ()
  (capi:display (make-instance 'scrolling-pinboards)))

(defvar *long-string1* 
  (with-output-to-string (temp)
    (loop  for x below 30
          do (format temp "Long String ~2,1,0,'0@a     " (- 29 x)))))

(defvar *long-string2*
  (with-output-to-string (temp)
    (loop for x below 30
          do (format temp "Long String ~2,1,0,'0@a     " x))))

(capi:define-interface scrolling-pinboards  ()
  ()
  (:panes
   (top-pinboard-object
    capi:item-pinboard-object
    :text *long-string1*)
   (bottom-pinboard-object
    capi:item-pinboard-object
    :text *long-string2*))
  (:layouts
   (top-pinboard
    capi:pinboard-layout
    '(top-pinboard-object)
    :background :light-red        
    :horizontal-scroll :without-bar)
   (bottom-pinboard
    capi:pinboard-layout
    '(bottom-pinboard-object)
    :background :lightgoldenrod
    :horizontal-scroll t
    :scroll-callback 'bottom-pinboard-scrolled
    )
   (main-layout
    capi:column-layout
    '(top-pinboard bottom-pinboard)
    :default t))
  (:default-initargs
   :best-height '(character 6)
   :best-width 400))

;; The scroll callback moves the slug-position of the top-pinboard to
;; match that of the bottom-pinboard.
(defun bottom-pinboard-scrolled (pane direction kind where &key &allow-other-keys)
  (declare (ignore kind where))
  (when (or (eq direction :horizontal) (eq direction :pan))
    (capi:set-horizontal-scroll-parameters
     (slot-value (capi:top-level-interface pane) 'top-pinboard)
     :slug-position (capi:get-horizontal-scroll-parameters pane :slug-position))))

