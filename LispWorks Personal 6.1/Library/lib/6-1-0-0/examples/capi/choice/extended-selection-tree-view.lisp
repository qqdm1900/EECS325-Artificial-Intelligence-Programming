;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/choice:extended-selection-tree-view.lisp,v 1.14.1.1 2011/08/24 13:26:22 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

;;----------------------------------------------------------------------------
;;
;;
;; This example demonstrates the use of CAPI:EXTENDED-SELECTION-TREE-VIEW
;;
;; To try it, compile and load this file and then execute:
;;
;;      (CL-USER::TEST-EXTENDED-SELECTION-TREE-VIEW)
;;
;; In the displayed tree, you can extend the selection by Control-Left-Click
;; and Control-Up and Control-Down. You can select all by Control-a. 
;;----------------------------------------------------------------------------


(in-package "CL-USER")


;;----------------------------------------------------------------------------
;; Create an image list for use by the tree-view
;;----------------------------------------------------------------------------

(defvar *extend-tree-view-image-list*
  (make-instance 'capi:image-list
		  :image-sets (list (capi:make-general-image-set
				     :id #.(gp:read-external-image
                                            (current-pathname "tree.bmp")
                                            :transparent-color-index 15)
				     :image-count 4))
		  :image-width 16
		  :image-height 16))

;;----------------------------------------------------------------------------
;; Define the interface extend-tree-view-test
;;----------------------------------------------------------------------------

(defvar *extend-tree-view-test-deleted-items* ())

(capi:define-interface extend-tree-view-test ()
  () 
  (:panes
   (tree capi:extended-selection-tree-view
         :roots '(0)
         :checkbox-status t
         :checkbox-change-callback #'(lambda (tree item status)
                                       (add-a-message (capi:element-interface tree)
                                                        "~&[~{~r~^, ~}] now ~a" 
                                                        item
                                                        (case status
                                                          (0 "Unchecked")
                                                          (1 "Partially checked")
                                                          (2 "Checked"))))
         :pane-menu 'extend-tree-view-test-menu
         :children-function #'(lambda (x)
                                (and (< x 100)
                                     (let ((base (* x 4)))
                                       (sort 
                                        (set-difference ; skip deleted children
                                         (list (+ base 1)
                                               (+ base 2)
                                               (+ base 3)
                                               (+ base 4)) 
                                         *extend-tree-view-test-deleted-items*) '<))))
         :image-lists (list :normal *extend-tree-view-image-list*)
         :image-function #'(lambda (x) (mod (1- x) 4))
         :visible-min-width 200
         :visible-min-height 200
         :retain-expanded-nodes t
         :callback-type :interface-data
         :print-function #'(lambda(x) (format nil "~:(~r~)"  x ))
         :selection-callback #'(lambda (self item)
                                 (add-a-message self  "~&Selected item ~S" item))
         :extend-callback #'(lambda (self item)
                                 (add-a-message self  "~&Extended item ~S" item))
         :retract-callback  #'(lambda (self item)
                                 (add-a-message self "~&Retracted item ~S" item))
         :action-callback 'test-extend-tree-view-action-function 
         :delete-item-callback 'test-extend-tree-view-delete-callback)
   (message-pane capi:collector-pane :height '(character 3) :width '(character 40)))
  (:layouts
   (default-layout
    capi:column-layout
    '(tree message-pane)))
  (:default-initargs
   :title "Tree View Test"))

(defun add-a-message (interface format-string &rest args)
  (let* ((message-pane (slot-value interface  'message-pane))
         (stream (capi:collector-pane-stream message-pane)))
    (apply 'format stream format-string args)))
    
(defun test-extend-tree-view-action-function (self item)
  (with-slots (tree) self
    (capi:tree-view-update-item tree item t))
  (add-a-message self "~&Action item ~S" item))

;;; The undocumented interface in 6.0 for :delete-item-callback
;;; changed in 6.1, and this code show a way of coding
;;; to cope with both interfaces. It works because the items
;;; themselves are not never lists in this example. 

(defun test-extend-tree-view-delete-callback (tree item)
  (if (listp item)   ;; true since 6.1, false until 6.0
      (progn 
        (setq *extend-tree-view-test-deleted-items*
              (union *extend-tree-view-test-deleted-items* item))
        (capi:with-atomic-redisplay (tree)
          (dolist (i-item item)
            (capi:tree-view-update-item tree i-item t))))
    (progn 
      (pushnew item *extend-tree-view-test-deleted-items*)
      (capi:tree-view-update-item tree item t))))

(defun extend-tree-view-test-menu (self data x y)
  (declare (ignorable data x y))
  (in-extend-tree-view-test-menu (capi:top-level-interface self)))

(capi:define-menu in-extend-tree-view-test-menu (self)
  :menu
  (("Delete"
    :callback  #'(lambda (interface)
                   (with-slots (tree) interface
                     (test-extend-tree-view-delete-callback
                      tree (capi:choice-selected-items tree))))
    :enabled-function #'(lambda (interface)
                          (with-slots (tree) interface
                            (capi:choice-selection tree))))
   (:component
    (("Undelete all"
      :callback #'(lambda(interface)
                    (setq *extend-tree-view-test-deleted-items* nil)
                    (with-slots (tree) interface
                      ;;redo the tree
                      (setf (capi:tree-view-roots tree) (capi:tree-view-roots tree))))))))
   :callback-type :interface)




;;----------------------------------------------------------------------------
;; A simple test function
;;----------------------------------------------------------------------------

(defun test-extended-selection-tree-view ()
  (setq *extend-tree-view-test-deleted-items* nil)
  (capi:display (make-instance 'extend-tree-view-test)))
