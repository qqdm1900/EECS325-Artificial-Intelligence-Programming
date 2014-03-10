;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/choice:list-panel-drag-images.lisp,v 1.5.1.1 2011/08/24 13:26:20 davef Exp $" -*-

;;----------------------------------------------------------------------------
;;
;; examples/capi/choice/list-panel-drag-images.lisp
;;
;; This example demonstrates drag-and-drop of images. 
;; It works on Cocoa and GTK+ later than 2.6. 
;; On Microsoft Windows dragging images between processes is not
;; implemented, but dragging images inside the same process works, so
;; it is possible to drag images between the panes of the example.
;;
;; To try it, compile and load this file and then execute:
;;
;;      (CL-USER::TEST-DRAG-IMAGES)
;;
;; This creates two windows:
;;
;;  1. A window called "Image maker" containing a LIST-PANEL. 
;;     You can drag an item from this list, which drags both the image
;;     and the string, because the :DRAG-CALLBACK (the function DRAG-CALLBACK)
;;     returns a plist with both :IMAGE and :STRING.
;;
;;  2. A window called "Image receiver", which contains a pinboard-layout.
;;     It accepts either images or strings, because it passes '(:image :string)
;;     to capi:set-drop-object-supported-formats in DROP-CALLBACK. It always 
;;     allow copying images or strings, because the :ENTER clause sets the 
;;     drop-object to :COPY when either of them is provided, and the :DRAG 
;;     clause doesn't check at all.
;;     When it drops (the :DROP clause), it checks if there is an
;;     image, and gets it if available, and otherwise gets the string.
;;     In both cases it create a new pinboard-object centered on the
;;     position of the cursor when it drops.

;; Note that in principle when it drops it could get both the image and string if 
;; it wants to, and that the :ENTER and :DRAG can do more complex tests. 

;; See entry of CAPI:SIMPLE-PANE for documentation of :DROP-CALLBACK and :DRAG-CALLBACK.
 
;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

(in-package "CL-USER")

(defvar *list-panel-items* '(First 
                             Second
                             Third
                             Fourth
                             fifth
                             sixth
                             seventh
                             eighth))
          
;;; An image of size 16x64. 

(defvar *lpdi-external-image*
  (gp:read-external-image (current-pathname "tree.bmp")))

;;; Create two windows (by using CAPI:CONTAIN): 
;;; * One with an OUTPUT-PANE that has a :DROP-CALLBACK and
;;;   therefore can receive objects. 
;;; * One with a LIST-PANEL that has a :DRAG-CALLBACK and
;;;   therefore the user can drag from it. 
;;; Note: If you want to drag from an CAPI:OUTPUT-PANE (including
;;; a CAPI:PINBOARD-LAYOUT) you need to use :INPUT-MODEL 
;;; and call CAPI:DRAG-PANE-OBJECT explicitly. See example
;;; in ../output-panes/drag-and-drop.lisp .

(defun test-drag-images ()
  (capi:contain (make-instance 'capi:pinboard-layout
                               :drop-callback 'drop-callback)
                :title "Image receiver"
                :interface-args '(:x 100 :y 100))
  (let (list-panel) 
    ;; The image function needs the pane, so we close
    ;; over it (in LIST-PANEL) before making it. 
    (flet ((image-function  (item)
             (lpdi-image-function list-panel item)))
      (setq list-panel (make-instance 'capi:list-panel
                              :items *list-panel-items*
                              :image-function #'image-function
                              :drag-callback 'drag-callback))
     
      (capi:contain list-panel
                    :title "Image maker"
                    :interface-args '(:x 400 :y 100)))))

;;; This is used in the IMAGE-FUNCTION above. 
;;; We cache the images, they will go away when the pane is destroyed. 

(defun lpdi-image-function (lp item)
  (let ((sub-image-cache (or (capi:capi-object-property lp 'image-cahce)
                             (setf (capi:capi-object-property lp 'image-cahce)
                                   (make-array (length *list-panel-items*)))))
        (index (position item *list-panel-items*)))
    (or (svref sub-image-cache index)
        (setf (svref sub-image-cache index)
              (lpdi-get-sub-image lp index)))))
    
;;; Return an image for an item. We cache the the image
;;; that we load from *lpdi-external-image*, but don't cache
;;; the sub image, because this is used in the drag-callback, 
;;; which automatically destroy the image when it finishes
;;; with it. 
                               
(defun lpdi-get-sub-image (lp index)
  (let ((image (or (capi:capi-object-property lp 'image)
                   (setf (capi:capi-object-property lp 'image)
                         (gp:load-image lp *lpdi-external-image*)))))
    (let ((image-index (mod index 4)))
      (gp:make-sub-image lp image (* image-index 16) 0 16))))

;;; The drag-callback. This is the argument to :DRAG-CALLBACK 
;;; in the "Image maker" LIST-PANEL, and is called whenever the 
;;; user tries to drag out of it. 
;;; Allows dragging of either an image or a string. The receiver will
;;; decide which one it will take. 
;;; Note that this returns a new image each call. The dragging
;;; mechanism frees it when it finishes with it. 

(defun drag-callback (pane selection)
  (let ((index (car selection)))
    (list :image (lpdi-get-sub-image pane index)
          :string (capi:print-collection-item 
                   (capi:get-collection-item pane index)
                   pane))))

;;; The drop-callback. This is the argument to :DROP-CALLBACK 
;;; in the "Image receiver" PINBOARD-LAYOUT. 

(defun drop-callback (pane drop-object stage)
  (case stage
    ;; This happens during creation of the pane. It tells the system that this
    ;; pane accepts either images or strings. 
    (:formats
     (capi:set-drop-object-supported-formats drop-object '(:image :string)))
    ;; This happens while dragging, when the cursor moves into the pane.
    ;; If the source provides either an image or a string, we tell
    ;; the system that we can copy it. 
    (:enter
     (when (or (capi:drop-object-provides-format drop-object :image)
               (capi:drop-object-provides-format drop-object :string))
       (setf (capi:drop-object-drop-effect drop-object) :copy)))
    ;; This happens while dragging, when the cursor moves inside 
    ;; the pane. Here we just always say we can copy it, but it can
    ;; be more complex test, for example checking the X and Y. 
    (:drag
     (setf (capi:drop-object-drop-effect drop-object) :copy))

    ;; This happens when the user drops on the pane. Find the position,
    ;; and then if the source provides image make an image-pinboard-object
    ;; with it, otherwise a text-pinboard-object with the string.
    (:drop
     (let* ((x (capi:drop-object-pane-x drop-object))
            (y (capi:drop-object-pane-y drop-object))
            (new-pi (funcall (if (capi:drop-object-provides-format drop-object :image)
                                 'get-new-image-pinboard-object
                               'get-new-item-pinboard-object)
                             pane drop-object x y)))
       (when new-pi
         (capi:manipulate-pinboard pane new-pi :add-top)
         (setf (capi:drop-object-drop-effect drop-object) :copy))))))


(defun get-new-image-pinboard-object (pane drop-object x y)
  (when-let (image (capi:drop-object-get-object 
                    drop-object pane :image))
    ;; adjust the position to center on the cursor 
    (let ((adjust-x (truncate (gp:image-width image) 2))
          (adjust-y (truncate (gp:image-height image) 2)))
      (decf x adjust-x)
      (decf y adjust-y))
                          
    (make-instance 'capi:image-pinboard-object
                   :image image :x x :y y)))


(defun get-new-item-pinboard-object (pane drop-object x y)
  (when-let (string (capi:drop-object-get-object 
                     drop-object pane :string))
    ;; adjust the position to center on the cursor 
    (multiple-value-bind (left top right bottom)
        (gp:get-string-extent pane string)
      (decf x (truncate (- right left) 2))
      (decf y (truncate (- bottom top) 2)))

    (make-instance 'capi:item-pinboard-object
                   :text string :x x :y y)))
