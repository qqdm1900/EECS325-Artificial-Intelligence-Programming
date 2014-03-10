;; -*- rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/graphics:catherine-wheel.lisp,v 1.4.1.1 2011/08/24 13:26:20 davef Exp $" -*-

;;----------------------------------------------------------------------------
;;
;; examples/capi/graphics/catherine-wheel.lisp
;;
;; This example shows the difference between old (LispWorks 6.0 and
;; earlier) drawing-mode and QUALITY drawing mode when drawing
;; strings.  To see the example, compile and load this file and then
;; execute:
;;
;; (CL-USER::CREATE-CATHERINE-WHEEL :QUALITY)
;;
;; (CL-USER::CREATE-CATHERINE-WHEEL :COMPATIBLE) ; which doesn't really work.
;;
;; With the :QUALITY mode, you get a simple "catherine wheel" drawing. 
;; With the :COMPATIBLE mode, the strings are not actually rotated, so
;; they end up all on top of each other, and only one is visible. 

;; You can change the size of the font by selecting a different font
;; from the OPTION-PANE at the bottom.
;;----------------------------------------------------------------------------
;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
;;----------------------------------------------------------------------------

(in-package "CL-USER")

(defun create-catherine-wheel (drawing-mode)
  (let* ((pane (make-instance 'capi:output-pane 
                              :draw-with-buffer t
                              :drawing-mode drawing-mode
                              :name 'catherine
                              :initial-constraints '(:min-width 400 :min-height 400)
                              :display-callback 'draw-catherine-wheel))
         (option-pane (make-instance
                       'capi:option-pane 
                       :items '(7 8 9 10 11 12 14 16 18 20 24 28 32 36 40)
                       :callback-type :data
                       :selected-item 12
                       :selection-callback #'(lambda (value)
                                               (setf (capi:capi-object-property pane 'size) value)
                                               (gp:invalidate-rectangle pane)))))
    (setf (capi:capi-object-property pane 'size) 12)
        
    (capi:contain (make-instance 
                   'capi:column-layout
                   :description
                   (list  pane option-pane))
                  :title (if (eq drawing-mode :quality)
                             "Catherine wheel with :QUALITY mode"
                           "Trying Catherine wheel with :COMPATIBLE mode"))))

;;; The display callback. 

(defun draw-catherine-wheel (pane &rest x)
  (declare (ignore x))
  (let ((font (gp:find-best-font pane 
                                 (gp:make-font-description 
                                  :size (capi:capi-object-property pane 'size)))))
    (gp:with-graphics-translation (pane 200 200)
      (dotimes (x 10)
        (let ((background (svref #(:yellow :blue :green :red
                                   :purple :white :cyan :gold3
                                   :pink :lightblue)
                                 x))
              (angle (* x  (/ pi 5))))
          (gp:with-graphics-rotation (pane angle)
            (gp:draw-string pane "The lazy fox" 0 0 
                            :font font :block t 
                            :background background)))))))
       
