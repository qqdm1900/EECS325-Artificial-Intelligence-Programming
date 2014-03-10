;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/choice:drag-and-drop.lisp,v 1.6.1.1 2011/08/24 13:26:20 davef Exp $" -*-

;;----------------------------------------------------------------------------
;;
;; examples/capi/choice/drag-and-drop.lisp
;;
;; This example demonstrates the uses of drag and drop in choices.
;;
;; To try it, compile and load this file and then execute:
;;
;;      (CL-USER::TEST-CHOICE-DRAG-AND-DROP)
;;
;; The top pair of lists show how to control where to allow dropping
;; in the destination list.  It does not actually change anything 
;; in the end of the drag and drop operation, but it informs in the bottom
;; whatoperation has happened. It also reports during the dragging what 
;; operation it would perform if the user drops. 
;; The four buttons below the two lists controls where an item can be
;; dropped:
;; On items - it is always dropped on one of the items. 
;; Between items - it is always dropped between items, i.e. either :above
;;                 or :below one of the items.
;; On or between items  - either on items or between items.
;; On the whole pane - it is always on the whole pane. 
;; Programmatically, this is done by passing the right placment to 
;; (setf capi:DROP-OBJECT-COLLECTION-INDEX) in the drop-callback
;; (drag-and-drop-choices-drop-list-callback below). 
;; Note: on Windows there is no visual indication where it is going to be
;;       dropped, it just says it in the bottom. On GTK and Cocoa it
;;       indicates exactly where in the list it is going to be dropped. 

;; The "Rearrangeable items" list allows its items to be rearranged 
;; by dragging them.


;;  The exchangeable list allow their items to be dragged from one list
;;  to the other.

;;----------------------------------------------------------------------------
;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
;;----------------------------------------------------------------------------


(in-package "CL-USER")

(capi:define-interface drag-and-drop-choices ()
  ()
  (:panes
   (drag-list
    capi:list-panel
    :items (loop for x below 8 collect
             (intern (format nil "~:r-drag-item" x) 'keyword))
    :drag-callback 'drag-and-drop-choices-drag-list-callback
    :interaction :extended-selection
    :visible-max-width nil
    :vertical-scroll nil
    :horizontal-scroll nil)
   (drop-list
    capi:list-panel
    :items (loop for x below 8 collect
             (intern (format nil "~:r-drop-item" x) 'keyword))
    :drop-callback 'drag-and-drop-choices-drop-list-callback
    :visible-max-width nil
    :vertical-scroll nil
    :horizontal-scroll nil)
   (drop-list-adjuster
    capi:radio-button-panel
    :items '((:item . "On items")
             (:between . "Between items")
             (:both . "On or between items")
             (:all . "On the whole pane"))
    :print-function 'cdr
    :selection 2
    :title "Right pane allows dropping:"
    :title-position :top)
   (rearrangeable-list
    capi:list-panel
    :items '(:rearrangeable-item-1 :rearrangeable-item-2
             :rearrangeable-item-3 :rearrangeable-item-4
             :rearrangeable-item-5 :rearrangeable-item-6)
    :interaction :extended-selection
    :drag-callback 'drag-and-drop-choices-rearrangeable-list-drag-callback
    :drop-callback 'drag-and-drop-choices-rearrangeable-list-drop-callback
    :visible-max-width nil
    :vertical-scroll nil
    :horizontal-scroll nil)
   (exchangeable-list-1
    capi:list-panel
    :items '(:exchangeable-item-1 :exchangeable-item-2
             :exchangeable-item-3 :exchangeable-item-4)
    :interaction :extended-selection
    :drag-callback 'drag-and-drop-choices-exchangeable-list-drag-callback
    :drop-callback 'drag-and-drop-choices-exchangeable-list-drop-callback
    :visible-min-height '(character 8)
    :visible-max-width nil
    :vertical-scroll nil
    :horizontal-scroll nil)
   (exchangeable-list-2
    capi:list-panel
    :items '(:exchangeable-item-5 :exchangeable-item-6
             :exchangeable-item-7 :exchangeable-item-8)
    :interaction :extended-selection
    :drag-callback 'drag-and-drop-choices-exchangeable-list-drag-callback
    :drop-callback 'drag-and-drop-choices-exchangeable-list-drop-callback
    :visible-min-height '(character 8)
    :visible-max-width nil
    :vertical-scroll nil
    :horizontal-scroll nil)
   (message-pane
    capi:title-pane
    :visible-max-width nil))
  (:layouts
   (drag-lists-row
    capi:row-layout
    '(drag-list drop-list)
    :gap 10
    :title "Drag items from the left pane to the right pane:")
   (drag-lists-layout
    capi:column-layout
    '(drag-lists-row drop-list-adjuster)
    :title "Control over where dropping is allowed"
    :title-position :frame
    :internal-border 10
    :gap 10)
   (rearrangeable-list-layout
    capi:simple-layout
    '(rearrangeable-list)
    :title "Rearrangeable items"
    :title-position :frame
    :internal-border 10)
   (exchangeable-lists-layout
    capi:row-layout
    '(exchangeable-list-1 exchangeable-list-2)
    :title "Drag items from one pane to the other"
    :title-position :frame
    :internal-border 10
    :gap 10)
   (main-layout
    capi:column-layout
    '(drag-lists-layout rearrangeable-list-layout
                        exchangeable-lists-layout message-pane)))
  (:default-initargs
   :layout 'main-layout
   :title "Drag And Drop Choices"))

;;; The drag-callback of left list-panel at the top. Returns
;;; a plist where the keywords are known formats and the values
;;; are the actual data. :STRING is a known format, and can be 
;;; used to drag to another application. :LEFT-RIGHT-ITEMS is
;;; not a known format, and can be used to drag only into the right
;;; list-panel at the top, because its drop-callback (next function)
;;; recognizes it. 

(defun drag-and-drop-choices-drag-list-callback (pane indices)
  (drag-and-drop-choices-show-message
   pane
   (format nil "Dragging left items ~S." indices))
  (and indices
       (list :string (format nil "~{~A~^~%~}"
                             (loop for index in indices
                                   collect
                                   (capi:get-collection-item pane index)))
             :left-right-items indices)))

;;; The drop-callback of the right list-panel at the top. 
;;; When called with :FORMATS it tells the dragndrop mechanism that it 
;;; recognize only one format, :left-right-items. This is is not a known
;;; format, and must come from inside the same program, in this case
;;; from the left list-panel because of the drop-callback above. 

(defun drag-and-drop-choices-drop-list-callback (pane drop-object stage)
  (case stage
    (:formats
     (capi:set-drop-object-supported-formats drop-object '(:left-right-items)))
    ((:enter :drag)
     (when (and (capi:drop-object-provides-format drop-object :left-right-items)
                (capi:drop-object-allows-drop-effect-p drop-object :copy))
       (multiple-value-bind (index placement)
           (capi:drop-object-collection-index drop-object)
         (let* ((message (format nil "Dropping to item ~a" index 
                                 (capi:get-collection-item pane index)))
                (drop-list-adjust (car (capi:choice-selected-item
                                        (slot-value (capi:top-level-interface pane)
                                                    'drop-list-adjuster))))
                (effect :copy))
           (if (eq drop-list-adjust :all)
               (progn
                 ;; Force dropping on the whole pane in this case.
                 (setf (capi:drop-object-collection-index drop-object)
                       (values nil :all))
                 (setq message (format nil "~A placement ~S" message :all)))
             (if (eq placement :all)
                 (progn
                   ;; Disallow dropping on the whole pane in this case.
                   (setq effect nil)
                   (setq message (format nil "~A placement not allowed" message)))
               (if (eq drop-list-adjust :both)
                   ;; Adjust the placement as specified.
                   (setq message (format nil "~A placement ~S" message placement))
                 (let ((new-placement (if (eq drop-list-adjust :item)
                                            :item
                                          (if (eq placement :item) :above placement))))
                     (setf (capi:drop-object-collection-index drop-object)
                           (values index new-placement))
                     (setq message (format nil "~A placement ~S" message new-placement))))))
           (drag-and-drop-choices-show-message pane message)
           (setf (capi:drop-object-drop-effect drop-object) effect)))))
    (:drop
     (when (and (capi:drop-object-provides-format drop-object :left-right-items)
                (capi:drop-object-allows-drop-effect-p drop-object :copy))
       (multiple-value-bind (index placement)
           (capi:drop-object-collection-index drop-object)
         (drag-and-drop-choices-show-message
          pane
          (format nil "Dropped right item: ~S ~S." index placement)))
       (setf (capi:drop-object-drop-effect drop-object) :copy)))))


(defun drag-and-drop-choices-rearrangeable-list-drag-callback (pane indices)
  (drag-and-drop-choices-show-message
   pane
   (format nil "Dragging rearrangeable items ~S." indices))
  (and indices
       (list :rearrangeable-items indices)))

(defun drag-and-drop-choices-rearrangeable-list-drop-callback (pane drop-object stage)
  (case stage
    (:formats
     (capi:set-drop-object-supported-formats drop-object '(:rearrangeable-items)))
    ((:enter :drag)
     (when (and (capi:drop-object-provides-format drop-object :rearrangeable-items)
                (capi:drop-object-allows-drop-effect-p drop-object :move))
       (multiple-value-bind (dropped-index placement)
           (capi:drop-object-collection-index drop-object)
         (unless (and (eq placement :item) (eq stage :drag))
           (setf (capi:drop-object-drop-effect drop-object) :move)
           (drag-and-drop-choices-show-message
            pane 
            (format nil "Dropping ~A rearrangeable item ~D: ~a"
                    (string-downcase placement) dropped-index
                    (capi:get-collection-item pane dropped-index)))))))
    (:drop
     (when (and (capi:drop-object-provides-format drop-object :rearrangeable-items)
                (capi:drop-object-allows-drop-effect-p drop-object :move))
       (multiple-value-bind (dropped-index placement)
           (capi:drop-object-collection-index drop-object)
         (drag-and-drop-choices-show-message
          pane
          (format nil "Dropped ~A rearrangeable item ~D: ~a"
                  (string-downcase placement) dropped-index 
                  (capi:get-collection-item pane dropped-index)))
         (let* ((dragged-indices (capi:drop-object-get-object
                                  drop-object pane :rearrangeable-items))
                (dragged-items (loop for index in dragged-indices
                                     collect (capi:get-collection-item pane index)))
                (new-items (list-panel-modified-items
                            pane
                            :remove-indices dragged-indices
                            :add-items dragged-items
                            :add-index dropped-index
                            :add-placement placement)))
           (setf (capi:collection-items pane) new-items)
           (setf (capi:drop-object-drop-effect drop-object) :move)))))))


(defun drag-and-drop-choices-exchangeable-list-drag-callback (pane indices)
  (drag-and-drop-choices-show-message
   pane
   (format nil "Dragging exchangeable items ~S." indices))
  (and indices
       (let ((provided-format
              (if (drag-and-drop-choices-exchangeable-list-1-p pane)
                  :exchangeable-items-1
                :exchangeable-items-2)))
         (list provided-format indices))))

(defun drag-and-drop-choices-exchangeable-list-drop-callback (pane drop-object stage)
  (case stage
    (:formats
     (capi:set-drop-object-supported-formats
      drop-object
      (if (drag-and-drop-choices-exchangeable-list-1-p pane)
          '(:exchangeable-items-2)
        '(:exchangeable-items-1))))
    ((:enter :drag)
     (let ((wanted-format (if (drag-and-drop-choices-exchangeable-list-1-p pane)
                              :exchangeable-items-2
                            :exchangeable-items-1)))
       (when (and (capi:drop-object-provides-format drop-object wanted-format)
                  (capi:drop-object-allows-drop-effect-p drop-object :move))
         (multiple-value-bind (dropped-index placement)
             (capi:drop-object-collection-index drop-object)
           (unless  (and (eq placement :item) 
                         (eq stage :drag))
             (setf (capi:drop-object-drop-effect drop-object) :move)
             (drag-and-drop-choices-show-message
              pane
              (format nil "Dropping ~A exchangeable item ~D: ~a"
                      (string-downcase placement) dropped-index 
                      (capi:get-collection-item pane dropped-index))))))))
    (:drop
     (let ((wanted-format (if (drag-and-drop-choices-exchangeable-list-1-p pane)
                              :exchangeable-items-2
                            :exchangeable-items-1)))
       (when (and (capi:drop-object-provides-format drop-object wanted-format)
                  (capi:drop-object-allows-drop-effect-p drop-object :move))
         (multiple-value-bind (dropped-index placement)
             (capi:drop-object-collection-index drop-object)
           (drag-and-drop-choices-show-message
            pane
            (format nil "Dropped ~A rearrangeable item ~D: ~a"
                    (string-downcase placement) dropped-index 
                    (capi:get-collection-item pane dropped-index)))
           (let* ((dragged-indices (capi:drop-object-get-object
                                    drop-object pane wanted-format))
                  (source-pane (slot-value (capi:top-level-interface pane)
                                           (if (eq wanted-format :exchangeable-items-1)
                                               'exchangeable-list-1
                                             'exchangeable-list-2)))
                  (dragged-items (loop for index in dragged-indices
                                       collect (capi:get-collection-item source-pane index)))
                  (new-items (list-panel-modified-items
                              pane
                              :add-items dragged-items
                              :add-index dropped-index
                              :add-placement placement))
                  (new-source-items (list-panel-modified-items
                                     source-pane
                                     :remove-indices dragged-indices)))
             (setf (capi:collection-items pane) new-items
                   (capi:collection-items source-pane) new-source-items)
             (setf (capi:drop-object-drop-effect drop-object) :move))))))))

(defun drag-and-drop-choices-exchangeable-list-1-p (pane)
  (eq (capi:capi-object-name pane) 'exchangeable-list-1))

(defun list-panel-modified-items (pane &key remove-indices
                                       add-items add-index (add-placement :above))
  (let ((item-count (capi:count-collection-items pane)))
    (if (zerop item-count)
        add-items
      (if add-index
          (let ((adjusted-add-index (if (eq add-placement :above)
                                        add-index
                                      (1+ add-index))))
            (loop for index from 0 to item-count
                  if (= index adjusted-add-index)
                  append add-items
                  unless (or (member index remove-indices)
                             (= index item-count))
                  collect (capi:get-collection-item pane index)))
        (loop for index from 0 below item-count
              unless (member index remove-indices)
              collect (capi:get-collection-item pane index))))))

(defun drag-and-drop-choices-show-message (pane title)
  (let ((message-pane (slot-value (capi:element-interface pane) 'message-pane)))
    (setf (capi:title-pane-text message-pane) title)))

(defun test-choice-drag-and-drop ()
  (capi:display (make-instance 'drag-and-drop-choices)))
