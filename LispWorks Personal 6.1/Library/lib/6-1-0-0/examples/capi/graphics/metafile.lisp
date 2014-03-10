;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/graphics:metafile.lisp,v 1.3.1.1 2011/08/24 13:26:21 davef Exp $" -*-

;;----------------------------------------------------------------------------
;;
;; examples/capi/graphics/metafile.lisp
;;
;; This example demonstrates the use of internal metafiles, for
;; drawing and clipboard access.
;; This is only implemented on Windows and Mac OS X with Cocoa.
;;
;; To try it, compile and load this file and then execute:
;;
;;      (CL-USER::TEST-METAFILE)
;;
;; Draw some lines by dragging in the pane.
;;
;; Choose Edit > Copy to copy the drawing to the clipboard.  It can
;; then be viewed using an external viewer.
;;
;; Click right in the pane to clear the current drawing.
;;
;; Choose Edit > Paste to use the current clipboard contents as a
;; scalable background-metafile for the pane.
;;
;;----------------------------------------------------------------------------
;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
;;----------------------------------------------------------------------------

(in-package "CL-USER")

(capi:define-interface metafile-test ()
  ()
  (:panes
   (metafile-pane metafile-pane))
  (:menus
   (file-menu "File"
              (("Print..." :callback 'metafile-test-print)
               ("Exit" :callback 'capi:quit-interface))
              :callback-type :interface)
   (edit-menu "Edit"
              (("Copy" :callback 'metafile-test-copy)
               ("Paste" :callback 'metafile-test-paste
                        :enabled-function 'metafile-test-paste-p))
              :callback-type :interface))
  (:menu-bar file-menu edit-menu)
  (:default-initargs
   :title "Metafile Test"
   :best-width 200
   :best-height 200))

(defun metafile-test-print (self)
  (with-slots (metafile-pane) self
    (with-slots (background-metafile) metafile-pane
      (when background-metafile
        (when-let (printer (capi:print-dialog))
          (capi:with-print-job (print-pane :pane metafile-pane
                                           :printer printer)
            (multiple-value-bind (vwidth vheight)
                (capi:simple-pane-visible-size print-pane)
              (capi:with-page-transform (0 0 vwidth vheight)
                (capi:draw-metafile print-pane background-metafile
                                    0 0 vwidth vheight)))))))))

(defun metafile-test-copy (self)
  (if (capi:can-use-metafile-p self)
      (with-slots (metafile-pane) self
        (capi:with-geometry metafile-pane
          (let* ((width capi:%width%)
                 (height capi:%height%)
                 (metafile (capi:with-internal-metafile
                               (metafile-output
                                :pane metafile-pane
                                :bounds (list 0 0 width height))
                             (draw-metafile-pane metafile-pane
                                                 metafile-output))))
            (capi:set-clipboard metafile-pane nil nil
                                (list :metafile metafile
                                      :string "A metafile"))
            (capi:free-metafile metafile))))
    (error "Cannot use metafiles with ~S" self)))

(defun metafile-test-paste (self)
  (with-slots (metafile-pane) self
    (let ((metafile (capi:clipboard metafile-pane :metafile)))
      (set-drawing-background metafile-pane metafile))))

(defun metafile-test-paste-p (self)
  (with-slots (metafile-pane) self
    (not (capi:clipboard-empty metafile-pane :metafile))))


(defvar *input-models* 
  '(((:button-1 :press) start-drawing)
    ((:button-1 :motion) continue-drawing)
    ((:button-1 :release) end-drawing)
    ((:button-3 :release) clear-drawing)))

(defclass metafile-pane (capi:output-pane)
  ((background-metafile :initform nil)
   (complete-lines :initform nil)
   (current-line :initform nil)
   (current-point :initform nil))
  (:default-initargs 
   :display-callback 'repair-drawing
   :input-model *input-models*))

(defmethod start-drawing ((self metafile-pane) x y)
  (with-slots (current-point) self
    (setf current-point (cons x y))))

(defmethod continue-drawing ((self metafile-pane) x y)
  (with-slots (current-line current-point) self
    (when current-point
      (let ((old-x (car current-point))
            (old-y (cdr current-point)))
        (gp:draw-line self old-x old-y x y)
        (setf current-line `(,old-x ,old-y ,x ,y))))))

(defmethod end-drawing ((self metafile-pane) x y)
  (declare (ignore x y))
  (with-slots (complete-lines current-line current-point) self
    (push current-line complete-lines)
    (setf current-line nil
          current-point nil)
    (gp:invalidate-rectangle self)))

(defmethod set-drawing-background ((self metafile-pane) metafile)
  (with-slots (background-metafile) self
    (when background-metafile
      (capi:free-metafile background-metafile))
    (setf background-metafile metafile)
    (gp:invalidate-rectangle self)))


;; this callback is "simple" in that it does not limit itself to the
;; "dirty" region

(defmethod repair-drawing ((self metafile-pane) x y width height)
  (declare (ignore x y width height))
  (draw-metafile-pane self self))

(defun draw-metafile-pane (self output-pane)
  (with-slots (background-metafile current-line complete-lines) self
    (when background-metafile
      (multiple-value-bind (vwidth vheight)
          (capi:simple-pane-visible-size output-pane)
        (capi:draw-metafile output-pane background-metafile
                            0 0 vwidth vheight)))
    (gp:draw-lines output-pane current-line)
    (dolist (line complete-lines)
      (gp:draw-lines output-pane line))))

(defmethod clear-drawing ((self metafile-pane) x y)
  (declare (ignore x y))
  (with-slots (background-metafile complete-lines current-line current-point) self 
    (when background-metafile
      (capi:free-metafile background-metafile)
      (setf background-metafile nil))
    (setf current-line nil
          current-point nil
          complete-lines nil))
  (gp:clear-graphics-port self))


(defun test-metafile ()
  (capi:display (make-instance 'metafile-test)))

