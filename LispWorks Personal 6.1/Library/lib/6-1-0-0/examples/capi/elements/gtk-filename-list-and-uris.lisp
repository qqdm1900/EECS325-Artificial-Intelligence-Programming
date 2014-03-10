;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/elements:gtk-filename-list-and-uris.lisp,v 1.2.1.1 2011/08/24 13:26:20 davef Exp $" -*-

;;----------------------------------------------------------------------------
;;
;; examples/capi/elements/gtk-filename-list-and-uris.lisp
;;
;; This example demonstrates using :FILENAME-LIST and :URIS
;; on GTK (requires GTK 2.6 or later). 
;;
;; To try it, compile and load this file and then execute:
;;
;;      (CL-USER::TEST-FILENAME-LIST-AND-URIS)
;;  
;; This should open two windows, one titled "With URIS", and
;; the other "With FILENAME-LIST". 
;;
;; Then open a system File Broswer or another application from
;; which you can drag file names. Drag few filenames into 
;; either of the windows above. Note that in the With FILENAME-LIST
;; window  you get the actual path. In the one With URIS,
;; you get file://<actual path>, which is the file uri.
;; 
;; You can open a another application from which you can 
;; drag URLs (e.g. Firefox: "Bookmarks"-> "Organize Bookmarks", 
;; expand on the right and select some URLS). Drag several URLs
;; to the windows. You will see that the URLs appear in the 
;; With URIS window, but not in the With FILENAMES window.

;; You can try to drag from either of the windows into an
;; application that can receive a list of URIS (e.g. firefox). 

;; The keywords :DROP-CALLBACK and :DRAG-CALLBACK are documented 
;; in the entry for CAPI:SIMPLE-PANE. 

;;----------------------------------------------------------------------------
;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
;;----------------------------------------------------------------------------

(defun test-filename-list-and-uris ()
  (test-internal :uris)
  (test-internal :filename-list))

(defun test-internal (list-keyword) 
  (let ((lp (make-instance 'capi:list-panel
                           :interaction :multiple-selection
                           :drop-callback 'drop-callback
                           :drag-callback 'drag-callback)))
    (setf (capi:capi-object-property lp 'drag-and-drop-list-keyword) 
          list-keyword)
    (capi:contain lp
                  :title (format nil "With ~a" list-keyword))))

(defun drag-callback (pane selection)
  (let ((keyword (capi:capi-object-property pane 'drag-and-drop-list-keyword)))
    (when selection
      (let* ((selected-items (capi:choice-selected-items pane))
             (strings (loop for item in selected-items
                            collect (capi:print-collection-item  item pane))))
        (list keyword  strings)))))

(defun drop-callback (pane drop-object stage)
  (let ((keyword (capi:capi-object-property pane 'drag-and-drop-list-keyword)))
    (case stage
      (:formats
       (capi:set-drop-object-supported-formats drop-object (list keyword)))
      ((:drag :enter)
       (setf (capi:drop-object-drop-effect drop-object) :copy))
      (:drop
       (capi:append-items pane (capi:drop-object-get-object
                                drop-object pane keyword))))))

