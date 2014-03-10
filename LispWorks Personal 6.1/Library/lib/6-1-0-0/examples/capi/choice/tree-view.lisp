;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/choice:tree-view.lisp,v 1.8.1.1 2011/08/24 13:26:20 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

;;----------------------------------------------------------------------------
;;
;; examples/capi/choice/tree-view.lisp
;;
;; This example demonstrates the uses of the tree-view in the CAPI.
;; Shows how to setup the images for a tree-view.
;; To try it, compile and load this file and then execute:
;;
;;      (CL-USER::TEST-TREE-VIEW)
;;
;;  Can also try:
;;
;;     (CL-USER::TEST-TREE-VIEW-WITH-STANDARD-IMAGES)
;;
;; Same as above, but uses standard images for some of the items. 

;;----------------------------------------------------------------------------


(in-package "CL-USER")


;;----------------------------------------------------------------------------
;; Create an image list for use by the tree-view
;;----------------------------------------------------------------------------

(defvar *my-image-list*
  (make-instance 'capi:image-list
		  :image-sets (list (capi:make-general-image-set
				     :id #.(gp:read-external-image
                                            (current-pathname"tree.bmp"))
				     :image-count 4))
		  :image-width 16
		  :image-height 16))


;;----------------------------------------------------------------------------
;; Define the interface option-pane-test
;;----------------------------------------------------------------------------

(capi:define-interface tree-view-test ()
  () 
  (:panes
   (tree capi:tree-view
         :roots '(1 2 3 4)
         :children-function #'(lambda (x)
                                (and (< x 100)
                                     (let ((base (* x 4)))
                                       (list (+ base 1)
                                             (+ base 2)
                                             (+ base 3)
                                             (+ base 4)))))
         :image-lists (list :normal *my-image-list*)
         :image-function #'(lambda (x) (mod (1- x) 4))
         :visible-min-width 200
         :visible-min-height 200
         :retain-expanded-nodes t
         :print-function #'(lambda(x) (format nil "~d : ~r" x x ))
         :selection-callback #'(lambda (item self)
                                 (declare (ignore self))
                                 (format t "~&Select item ~S~%" item))
         :action-callback #'(lambda (item self)
                              (declare (ignore self))
                              (format t "~&Activate item ~S~%" item))
         :delete-item-callback #'(lambda (self item)
                                   (declare (ignore self))
                                   (format t "~&Delete item ~S~%" item))))
  (:layouts
   (default-layout
    capi:simple-layout
    '(tree)))
  (:default-initargs
   :title "Tree View Test"))


;;----------------------------------------------------------------------------
;; A simple test function
;;----------------------------------------------------------------------------

(defun test-tree-view ()
  (capi:display (make-instance 'tree-view-test :title "Simple Tree view")))



;;----------------------------------------------------------------------------
;; Add also standard images
;;----------------------------------------------------------------------------

;;; The image function for th etree view with standard images. 
;;; For some of the items return a symbol specifying a standard image. 

(defun my-image-function-with-standard-images (x)
  (case (mod x 9)
    (1 :std-cut)
    (2 :std-file-save)
    #+mswindows (3 :view-parent-folder)
    #+mswindows (4 :hist-favorites)
    (t  (mod (1- x) 4))))

;;; The print function.
;;; For items that use standard image, also print the image name. 

(defun my-print-function-with-standard-images (x)
  (format nil "~d : ~r ~@[ [ ~a ]~]" x x 
          (case (mod x 9)
            (1 "Cut")
            (2 "Save")
            #+mswindows (3 "Parent Folder")
            #+mswindows (4 "Favorites"))))
   

(defun test-tree-view-with-standard-images ()
  (let ((interface (make-instance 'tree-view-test :title "Tree view with standard images")))
    (with-slots (tree) interface
      (setf (capi:tree-view-image-function tree) 'my-image-function-with-standard-images
            (capi:collection-print-function tree) 'my-print-function-with-standard-images))
    (capi:display interface)))

