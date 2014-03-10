;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/editor:input-callback.lisp,v 1.1.1.1 2011/08/24 13:26:21 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

;;----------------------------------------------------------------------------
;;
;; examples/capi/editor/input-callback.lisp
;;
;; This example demonstrates using the BEFORE-INPUT-CALLBACK
;; and AFTER-INPUT-CALLBACK  of an EDITOR-PANE  
;; To try it, compile and load this file and then execute:
;;

;;; (CL-USER::RUN-EDITOR-INPUT-CALLBACK-EXAMPLE)

;;; You get a window with two panes: the top is an editor, the bottom a read-only
;;; collectoor-pane. The upper pane has input callbacks that print
;;; into the bottom pane what input it gets. You can typing, scrolling,
;;; pasting into the top pane, and see what happens in the bottom. 


;;---------------------------------
(in-package "CL-USER")


;;; The callbacks call this. 
;;; The input may be a list, meaning function application, in this
;;; case we print only the car. For gestures, we use 
;;; sys:print-pretty-gesture-spec, otherwise just print whatever it is.

(defun my-print-editor-input (stream prefix gesture)
  (write-string prefix stream)
  (cond ((sys:gesture-spec-p gesture)
         (sys:print-pretty-gesture-spec gesture stream))
        ((consp gesture)
         (princ (car gesture) stream))
        (t (princ gesture stream)))
  (terpri stream))
            
;;; Creating the editor-pane, with both :before-input-callback and 
;;; :after-input-callback that print to stream. 
(defun my-make-editor-pane-printing-to-stream (stream)
  (make-instance 
   'capi:editor-pane
   :buffer (symbol-name (gensym "Dummy buffer"))
   :flag :output ;;; don't bother to save
   :before-input-callback #'(lambda (pane gesture)
                              (declare (ignore pane))
                              (my-print-editor-input stream "input: " gesture))
   :after-input-callback #'(lambda (pane gesture)
                             (declare (ignore pane))
                             (my-print-editor-input stream "   Done : " gesture))))


(defun run-editor-input-callback-example ()                
  (let* ((output (make-instance 'capi:collector-pane
                                :enabled :read-only))
         (stream (capi:collector-pane-stream output))
         (editor-pane (my-make-editor-pane-printing-to-stream stream)))
    (capi:contain 
     (make-instance
      'capi:column-layout
      :description (list editor-pane output))
     :initial-constraints '(:visible-min-width 500)
     :title "Editor input callback example")))
