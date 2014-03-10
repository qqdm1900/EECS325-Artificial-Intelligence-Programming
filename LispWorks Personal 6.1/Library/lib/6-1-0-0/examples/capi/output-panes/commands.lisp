;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/output-panes:commands.lisp,v 1.5.12.1 2011/08/24 13:26:20 davef Exp $" -*-

;;----------------------------------------------------------------------------
;;
;; examples/capi/output-panes/commands.lisp
;;
;; This example demonstrates the uses of commands in CAPI output-panes.
;; To try it, compile and load this file and then execute:
;;
;;      (CL-USER::TEST-COMMANDS)
;;
;;----------------------------------------------------------------------------
;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
;;----------------------------------------------------------------------------


(in-package "CL-USER")

(capi:define-interface command-test ()
  ((old-x :initform 0)
   (old-y :initform 0)
   (colour :initform :red)
   (colours :initform '(:red :white :blue :green :yellow)))
  (:panes
   (o1
    capi:output-pane
    :title "Press Mouse-1 or Shift-Mouse-3:"
    :input-model '((sel1 sel1-function)
		   (mod-sel1 mod-sel1-function)))
   (o2
    capi:output-pane
    :title "Press Mouse-2 or Shift-Mouse-3:"
    :input-model '((sel2 sel2-function)
		   (mod-sel1 mod-sel2-function)))
   (o3
    capi:output-pane
    :title "Mouse motion - no modifiers"
    :input-model (list (list :motion 
                             #'(lambda(self x y)
                                 (motion-function capi:interface self x y)))))
   (o4
    capi:output-pane
    :title "Mouse motion - shift-button-1"
    :input-model (list (list 'but1mot
                             #'(lambda(self x y)
                                 (motion-function capi:interface self x y)))))
   (o5
    capi:output-pane
    :title "Mouse motion - shift-button-1 - colour change after every shift-button-1 release"
    :input-model (list (list 'but1mot
                             #'(lambda(self x y)
                                 (motion-function capi:interface self x y)))
                       (list 'relsh1
                             #'(lambda(self x y)
                                 (declare (ignore x y))
                                 (let ((next (or (first (rest (member colour colours)))
                                                 (first colours))))
                                   (setf colour next)
                                   (setf (capi:simple-pane-foreground self) colour)))))))
                             
  (:default-initargs
   :title "Command Test"
   :best-width 300
   :best-height 300))

;; define the commands

(capi:define-command sel1 (:button-1 :press))

(capi:define-command rel1 (:button-1 :release))

(capi:define-command relsh1 (:shift :button-1 :release))

(capi:define-command sel2 (:button-2 :press))

(capi:define-command but1mot (:shift :button-1 :motion ))

(capi:define-command mod-sel1 (:button-3 :press :shift)
   :translator 'mod-sel1-translate)


;; define the action functions

(defun sel1-function (self x y)
  (gp:draw-character self #\1 x y))

(defun mod-sel1-function (self x y)
  ;; assumes that the coords are negated (see MOD-SEL1-TRANSLATE)
  (gp:draw-character self #\1 (- x) (- y) :foreground :red))

(defun sel2-function (self x y)
  (gp:draw-character self #\2 x y))

(defun mod-sel2-function (self x y)
  ;; assumes that the coords are negated (see MOD-SEL1-TRANSLATE)
  (gp:draw-character self #\2 (- x) (- y) :foreground :red))

(defun motion-function (interface self x y)
  (with-slots (old-x old-y) interface
    (gp:draw-line self old-x old-y x y)
    (setq old-x x old-y y)))

;; define the translators

(defun mod-sel1-translate (self x y)
  (declare (ignore self))
  ;; just negate the coords to match MOD-SEL1-FUNCTION
  (list (- x) (- y)))


;;----------------------------------------------------------------------------
;; A simple test routine
;;----------------------------------------------------------------------------

(defun test-commands ()
  (capi:display (make-instance 'command-test)))
