;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/choice:list-panel-pane-menu.lisp,v 1.2.9.1 2011/08/24 13:26:21 davef Exp $" -*-

;;----------------------------------------------------------------------------
;;
;; examples/capi/choice/list-panel-pane-menu.lisp
;;
;; This example demonstrates the various context menu behaviors available for
;; list panels.
;;
;; To try it, compile and load this file and then execute:
;;
;;      (CL-USER::TEST-LIST-PANEL-PANE-MENU)
;;
;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

(in-package "CL-USER")

(defparameter *items* '(a b c d))

(defun make-menu-item (pane items-to-select)
  (let ((description 
         (apply 'format nil "~#[Deselect All ~;Select ~S~;Select ~S and ~S~
 ~:;Select~@{~#[~; and~] ~S~^,~}~]" items-to-select)))
    (make-instance 'capi:menu-item
                   :text  description
                   :callback-type :none
                   :selection-callback
                   #'(lambda ()
                       (setf (capi:choice-selected-items pane)
                             items-to-select)
                       (format t "~&Selected ~A~%" description)))))

(defun make-pane-menu (pane object x y)
  (declare (ignore x y))
  (make-instance
   'capi:menu
   :items-function
   #'(lambda (interface)
       (declare (ignore interface))
       (list
        (make-instance 'capi:menu-component
                       :items
                       (list
                        (make-menu-item pane '())
                        (make-menu-item pane 
                                        (if (eq (capi:choice-interaction pane) :multiple-selection) 
                                            '(c d)
                                          '(b)))
                        (make-menu-item pane 
                                        (if (eq (capi:choice-interaction pane) :multiple-selection) 
                                            '(a c d)
                                          '(c)))
                        (make-menu-item pane '(d))))
        (make-instance 'capi:menu-component
                       :items
                       (list
                       (make-instance 'capi:menu-item
                                      :text (format nil "Clicked: ~A" object)
                                      :callback-type :none
                                      :selection-callback
                                      #'(lambda ()
                                          (capi:display-message 
                                           "This command does not set the selection in the list-panel")))
                       (make-instance 'capi:menu-item
                                      :text (apply 'format nil "~#[No Selection~;Selection: ~S~;Selection ~S and ~S~
 ~:;Selection~@{~#[~; and~] ~S~^,~}~]" (capi:choice-selected-items pane))
                                      :callback-type :none
                                      :selection-callback
                                      #'(lambda ()
                                          (capi:display-message "This command does not set the selection in the list-panel")))))))))

(defun make-list-panel-pane-menu-example (behavior interaction)
  (make-instance 'capi:list-panel
                 :title (format nil "~(~S~)" behavior)
                 :mnemonic :none
                 :visible-min-height `(:character ,(length *items*))
                 :visible-max-height t
                 :vertical-scroll nil
                 :horizontal-scroll nil
                 :visible-max-width nil
                 :selected-items (if (eq interaction :multiple-selection)
                                     (butlast *items*)
                                   (car *items*))
                 :items *items*
                 :callback-type :data
                 :right-click-selection-behavior behavior
                 :interaction interaction
                 :pane-menu 'make-pane-menu))

(defun test-list-panel-pane-menu ()
  (capi:contain
   (make-instance
    'capi:row-layout
    :gap 10
    :internal-border 10
    :description
    (loop for interaction in '(:single-selection
                               :multiple-selection)
          collect
          (make-instance 'capi:column-layout
                         :title (format nil "~(~S~)" interaction)
                         :mnemonic :none
                         :gap 10
                         :title-adjust :center
                         :description
                         (loop for behavior in '(:no-change
                                                 :clicked/restore/restore 
                                                 :existing-or-clicked/restore/restore
                                                 :clicked/restore/discard
                                                 :existing-or-clicked/restore/discard
                                                 :clicked/discard/discard
                                                 :existing-or-clicked/discard/discard)
                               collect (make-list-panel-pane-menu-example behavior interaction)))))))
