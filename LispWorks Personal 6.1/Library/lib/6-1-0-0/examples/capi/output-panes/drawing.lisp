;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/output-panes:drawing.lisp,v 1.3.1.2 2011/11/04 20:43:04 davef Exp $" -*-

;;----------------------------------------------------------------------------
;;
;; examples/capi/output-panes/drawing.lisp
;;
;; This example demonstrates the input model of an output pane
;; with a simple redisplay callback.
;; It also demostrates the use of an image on the clipboard (not supported
;; on Motif).
;;
;; To try it, compile and load this file and then execute:
;;
;;      (CL-USER::TEST-DRAWING)
;;
;; You then "draw" by pressing left button and dragging around. You
;; can clear everything by right-click. The drawing isimplemnetd
;; by using a sub-class of OUTPUT-PANE, DRAWING-PANE, with :INPUT-MODEL
;; and :DISPLAY-CALLBACK and some additional slots to keep the relevant
;; informatiom.

;; Once you have "drawing" you can copy it from the "Edit" menu. 
;; You can also copy it with a specific background color. 
;; Once there is an image in the clipboard you can also paste it into the pane,
;; which means putting it in the "background" (really means it is drawn first
;; and then the rest of the drawing). 

;; The images can be copied/pasted/ from/to other applications. 

;;----------------------------------------------------------------------------
;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
;;----------------------------------------------------------------------------

(in-package "CL-USER")

(capi:define-interface drawing-test ()
  ()
  (:panes
   (drawing-pane drawing-pane))
  (:menus
   (file-menu "File"
              (("Exit" :callback 'capi:quit-interface))
              :callback-type :interface)
   (edit-menu "Edit"
              (("Copy" :callback 'drawing-test-copy)
               ("Copy With color" :callback 'drawing-test-copy-with-color)
               ("Paste" :callback 'drawing-test-paste
                        :enabled-function 'drawing-test-paste-p))
              :callback-type :interface))
  (:menu-bar file-menu edit-menu)
  (:default-initargs
   :title "Drawing Test"
   :best-width 200
   :best-height 200))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Edit menu callbacks ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun drawing-test-paste (self)
  (with-slots (drawing-pane) self
    (let ((image (capi:clipboard drawing-pane :image)))
      (drawing-pane-set-drawing-background drawing-pane image))))

(defun drawing-test-paste-p (self)
  (with-slots (drawing-pane) self
    (not (capi:clipboard-empty drawing-pane :image))))


(defun drawing-test-copy (self)
  (internal-drawing-test-copy self nil))

(defun drawing-test-copy-with-color (self)
  (multiple-value-bind (color ok-p)
      (capi:prompt-for-color "Select background for the copied image")
    (when ok-p
      (internal-drawing-test-copy self color))))

;;; In principle when background is NIL we can just call gp:make-image-from-port
;;; on the drawing-pane itself, but that copy from the screen, and if there is
;;; anything on top of the drawing-pane it will copy it too. By always
;;; calling gp:make-image-from-port on a fresh port we can be sure that
;;; that the image is what it is supposed to be. 

(defun internal-drawing-test-copy (self background)
  (with-slots (drawing-pane) self
    (let ((width (gp:port-width drawing-pane))
          (height (gp:port-height drawing-pane)))

      (let ((image 
             (gp:with-pixmap-graphics-port (port drawing-pane width height
                                                 :clear t :background background)
               (display-drawing-pane drawing-pane port 0 0 width height)
               (gp:make-image-from-port port))))
        (capi:set-clipboard drawing-pane nil nil (list :image image
                                                       :string "An image"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Drawing-pane implementation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *drawing-pane-input-model* 
  '(((:button-1 :press) drawing-pane-start-drawing)
    ((:button-1 :motion) drawing-pane-continue-drawing)
    ((:button-1 :release) drawing-pane-end-drawing)
    ((:button-3 :release) drawing-pane-clear-drawing)))

(defclass drawing-pane (capi:output-pane)
  ((background-image :initform nil)
   (complete-lines :initform nil)
   (current-line :initform nil)
   (current-point :initform nil))
  (:default-initargs 
   :display-callback 'drawing-pane-display-callback
   :input-model *drawing-pane-input-model*))

;;; Input-mode functions. 

(defun drawing-pane-start-drawing (self x y)
  (with-slots (current-point) self
    (setf current-point (cons x y))))


(defun drawing-pane-continue-drawing (self x y)
  (with-slots (current-line current-point) self
    (when current-point
      (let ((old-x (car current-point))
            (old-y (cdr current-point)))
        (gp:draw-line self old-x old-y x y)
        (setf current-line `(,old-x ,old-y ,x ,y ,@current-line))))
    (setf current-point (cons x y))))

(defun drawing-pane-end-drawing (self x y)
  (declare (ignore x y))
  (with-slots (complete-lines current-line current-point) self
    (push current-line complete-lines)
    (setf current-line nil
          current-point nil)))

(defun drawing-pane-clear-drawing (self x y)
  (declare (ignore x y))
  (with-slots (background-image complete-lines current-line current-point) self 
    (when background-image
      (gp:free-image self background-image)
      (setf background-image nil))
    (setf current-line nil
          current-point nil
          complete-lines nil))
  (gp:clear-graphics-port self))



(defun drawing-pane-set-drawing-background (self image)
  (with-slots (background-image) self
    (when background-image
      (gp:free-image self background-image))
    (setf background-image image)
    (gp:invalidate-rectangle self)))


;;; Drawing th epane
;;; This drawing function is "simple" in that it does not limit itself to the
;;; "dirty" region
;;; We have here argument DRAW-TO so it can also be used in 
;;; internal-drawing-test-copy

(defun display-drawing-pane (self draw-to x y width height)
  (declare (ignore x y width height))
  (with-slots (background-image current-line complete-lines) self
    (when background-image
      (gp:draw-image draw-to background-image 0 0))
    (gp:draw-lines draw-to current-line)
    (dolist (line complete-lines)
      (gp:draw-lines draw-to line))))

;;;  Display-callback of th epane. 

(defmethod drawing-pane-display-callback ((self drawing-pane) x y width height)
  (declare (ignore x y width height))
  (display-drawing-pane self self x y width height))



;;;

(defun test-drawing ()
  (capi:display (make-instance 'drawing-test)))

