;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/elements:pane-popup-menu-items.lisp,v 1.1.1.1 2011/08/24 13:26:19 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

(in-package "CL-USER")

;;; An example of dynamically defining the items in the popup menu (context menu)
;;; using capi:PANE-POPUP-MENU-ITEMS. 

;;; To try it, compile and load this file and then execute:

;;;      (CL-USER::RUN-PANE-POPUP-MENU-ITEMS-EXAMPLE)

;;; This creates a window with a list of animals. Select some items
;;; and then  Right-click  on it. This gives a menu that for each selected item
;;; allows you to upcase or downcase it, depending on its current case. 

;;; Also demonstrates using capi:choice-update-item to update the display
;;; on an item in a choice. 
;;; ---------------------------------------------------------

(in-package "CL-USER")

;;; Each item in MYLIST is this structuture. 
;;; In real application it will typically be some entity in the application,

(defstruct an-animal
  string)

;;; The actual items
(defvar *my-objects*
  (loop for name in '("ant"
                      "bee"
                      "cat"
                      "dog"
                      "elephant"
                      "frog"
                      "gnu"
                      "horse"
                      "ichthyosaurus"
                      )
        collect 
        (make-an-animal :string name)))

;;; This is used by the callback in each menu-item to actually do the work. 
;;; Note that it modifies the AN-ANIMAL destructively, but does not change
;;; the items of the LIST-PANEL so it needs to
;;; call capi:choice-update-item to tell the LIST-PANEL to update the display
;;; for it. 

(defun change-an-animal-case (pane an-animal upcase-p)
  (let ((old (an-animal-string an-animal)))
    (setf (an-animal-string an-animal) 
          (if upcase-p
              (string-upcase old)
            (string-downcase old))))
  (capi:choice-update-item pane an-animal))

;;; Create an menu item for each "animal", offering the
;;; user the option to change its case, based on its current case. 

(defun create-menu-item-for-my-item (an-animal)
  (let* ((string (an-animal-string an-animal))
         (first-char (char string 0))
         (upcase-p (lower-case-p first-char))
         (title (format nil "~a ~a" (if upcase-p "Upcase" "Downcase") string))
         (callback #'(lambda (pane)
                       (change-an-animal-case pane an-animal upcase-p))))
    (make-instance 'capi:menu-item :title title
                   :callback callback)))
   



(capi:define-interface my-interface ()
  ()
  (:panes
   (list
    capi:list-panel
    :external-min-height 160
    :external-min-width 320
    :print-function 'an-animal-string
    :interaction :multiple-selection
    :items *my-objects*)))


  
;;; Returns the items for the popup menu (context menu) 
;;; in the list. It specializes on the interface, which is normally 
;;; the simplest way of doing it. This effectively 
;;; assumes that all LIST-PANELs in  my-interface need the same items.
;;; If the interface has more than one LIST-PANEL which needs different 
;;; items, the method needs to check which LIST-PANEL it got, or you 
;;; can sub-class LIST-PANEL and specialize on the sub-class. 

(defmethod capi:pane-popup-menu-items ((pane capi:list-panel) (interface my-interface))
  (loop for item in (capi:choice-selected-items pane)
        collect (create-menu-item-for-my-item item)))

(defun run-pane-popup-menu-items-example ()
  (capi:display (make-instance 'my-interface)))
