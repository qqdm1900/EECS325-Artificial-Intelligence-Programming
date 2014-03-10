;; -*- rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/elements:popup-menu-button.lisp,v 1.1.9.1 2011/08/24 13:26:21 davef Exp $" -*-

;;----------------------------------------------------------------------------
;;
;; examples/capi/elements/popup-menu-button.lisp
;;
;; This example demonstrates the uses of popup-menu-button in the CAPI.
;; To try it, compile and load this file and then execute:
;;
;;      (CL-USER::TEST-POPUP-MENU-BUTTON)
;;
;;----------------------------------------------------------------------------
;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
;;----------------------------------------------------------------------------


(in-package "CL-USER")


(capi:define-interface popup-menu-button-test ()
  ()
  (:panes
   (fixed-popup-menu-button
    capi:popup-menu-button
    :text "Fixed"
    :menu fixed-popup-menu-button-menu)
   (changing-popup-menu-button
    capi:popup-menu-button
    :text "Changing"
    :menu changing-popup-menu-button-menu)
   (stateful-popup-menu-button
    capi:popup-menu-button
    :text "Stateful"
    :menu stateful-popup-menu-button-menu)
   (random-popup-menu-button
    capi:popup-menu-button
    :text "Random"
    :menu-function 'make-random-popup-menu-button-menu))
  (:menus
   (fixed-popup-menu-button-menu
    nil ; title is ignored
    (:item-1
     :item-2
     :item-3)
    :print-function 'string-capitalize
    :callback 'simple-popup-menu-button-menu-callback
    :callback-type :data)
   (changing-popup-menu-button-menu
    nil ; title is ignored
    (:unchanging-item
     (:component
      ())
     ("Disabled if no Changing Items"
      :data "Disabled if no Changing Items"
      :enabled-function 'changing-popup-menu-button-menu-item-2-enabled-p))
    :print-function 'string-capitalize
    :callback 'simple-popup-menu-button-menu-callback
    :callback-type :data
    :popup-callback 'changing-popup-menu-button-menu-popup-callback)
   (stateful-popup-menu-button-menu
    nil ; title is ignored
    ((:component
      (:single-a
       :single-b
       :single-c)
      :interaction :single-selection
      :print-function 'string-capitalize)
     (:component
      (:multi-a
       :multi-b
       :multi-c)
      :interaction :multiple-selection
      :print-function 'string-capitalize))
    :callback 'simple-popup-menu-button-menu-callback
    :callback-type :data))
  (:layouts
   (main-layout
    capi:row-layout
    '(fixed-popup-menu-button
      stateful-popup-menu-button
      changing-popup-menu-button
      random-popup-menu-button)))
  (:default-initargs
   :title "Menu Button Test"
   :layout 'main-layout))


;;----------------------------------------------------------------------------
;; Callbacks
;;----------------------------------------------------------------------------

(defun simple-popup-menu-button-menu-callback (data)
  (capi:display-message "Item ~S was selected." data))

(defun changing-popup-menu-button-menu-popup-callback (menu)
  (let ((items (capi:menu-items menu)))
    (setf (capi:collection-items (second items))
          (loop for index below (random 4)
                collect (format nil "Changing item ~D" index)))))

(defun changing-popup-menu-button-menu-item-2-enabled-p (interface)
  (with-slots (changing-popup-menu-button-menu) interface
    (let ((items (capi:menu-items changing-popup-menu-button-menu)))
      (plusp (length (capi:collection-items (second items)))))))

(defun make-random-popup-menu-button-menu (button)
  (let ((prefix (capi:item-text button)))
    (make-instance 'capi:menu
                   :items (loop repeat 5
                                collect (format nil "~A ~D"
                                                prefix
                                                (random 1000)))
                   :callback 'random-popup-menu-button-menu-callback
                   :callback-type :data-interface)))

(defvar *random-popup-menu-button-titles*
  '("Up" "Down" "Charm" "Strange" "Top" "Bottom"))

(defun choose-random-popup-menu-button-title ()
  (let ((titles *random-popup-menu-button-titles*))
    (nth (random (length titles)) titles)))

(defun random-popup-menu-button-menu-callback (data interface)
  (with-slots (random-popup-menu-button) interface
    (let ((next-title (choose-random-popup-menu-button-title)))
      (capi:display-message "Random item ~S was selected~%Next title will be ~A."
                            data
                            next-title)
      (setf (capi:item-text random-popup-menu-button)
            next-title))))


;;----------------------------------------------------------------------------
;; test-popup-menu-button
;;----------------------------------------------------------------------------

(defun test-popup-menu-button ()
  (capi:display (make-instance 'popup-menu-button-test)))
