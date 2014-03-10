;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/choice:simple-graph-pane.lisp,v 1.1.1.1 2011/08/24 13:26:20 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
;;----------------------------------------------------------------------------
;;
;; examples/capi/choice/simple-graph-pane.lisp
;;
;; A simple example of a graph-pane.
;; Demonstrate using :NODE-PANE-FUNCTION to customize the nodes,
;; in this case give a background depending on the length of the string. 

;; To try it, compile and load this file and then execute:

;;     (cl-user::test-simple-graph-pane)

;; Type into the text-input-pane. It build a graphs
;; as you type. Each graph is different, becase it split 
;; the strings at random. 

;----------------------------------------------------------------------------
;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
;;----------------------------------------------------------------------------


(in-package "CL-USER")


;;; The :children-function of the graph. No children
;;; for one character or empty string, two one-character children
;;; for two-character strings,  split it to two children at random
;;; for longer strings.

(defun string-children (string)
  (let ((length (length string)))
    (cond ((<= length 1) nil)
          ((= length 2) (list (subseq string 0 1) (subseq string 1 2)))
          (t  (let ((middle (1+ (random (- length 2)))))
               (list (subseq string 0 middle)
                     (subseq string middle)))))))

;;; Create the node "pane". We actually create a pinboard object,
;;; which much lighter and normally what you want. We use the
;;; :graphics-args to give it a background depending on the string
;;; length. 

(defun make-node-pane (pane string)
  (make-instance 'capi:item-pinboard-object
                 :text string
                 :graphics-args 
                 (list :background
                       (svref #(:yellow :red :cyan :green :white)
                              (mod (length string) 5)))))

;;; The callback of the text input pane to update the graph. 

(defun text-pane-change-callback (interface text)
  (setf (capi:graph-pane-roots (test-interface-graph-pane interface))
        (list text)))

(capi:define-interface test-interface ()
  ()
  (:panes
   (text-pane
    capi:text-input-pane
    :change-callback-type :interface-data
    :text-change-callback 'text-pane-change-callback)
   (graph-pane
    capi:graph-pane
    :roots nil
    :node-pane-function 'make-node-pane
    :children-function 'string-children
    :accessor test-interface-graph-pane))
  (:layouts
   (main-layout
    capi:column-layout
    '(text-pane graph-pane)))
  (:default-initargs
   :best-width 300
   :best-height 300))

(defun test-simple-graph-pane ()
  (capi:display (make-instance 'test-interface)))
