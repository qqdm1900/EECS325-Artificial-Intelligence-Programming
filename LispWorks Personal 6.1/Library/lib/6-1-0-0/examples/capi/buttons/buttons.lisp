;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/buttons:buttons.lisp,v 1.16.1.1 2011/08/24 13:26:20 davef Exp $" -*-

;;----------------------------------------------------------------------------
;;
;; examples/capi/buttons/buttons.lisp
;;
;; This example demonstrates the use of buttons and button-panels in the
;; CAPI.
;; To try it, compile and load this file and then execute:
;;
;;      (CL-USER::TEST-BUTTONS)
;;
;; This display two interfaces. One is the BUTTON-TEST, and demonstarte
;; the callbacks. The other one is an IMAGE-BUTTON-EXAMPLE, and demostrates
;; using images in buttons. 

;;----------------------------------------------------------------------------
;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
;;----------------------------------------------------------------------------


(in-package "CL-USER")


;;----------------------------------------------------------------------------
;; Define the interface button-test
;;----------------------------------------------------------------------------

(capi:define-interface button-test ()
  ()
  (:panes
   (push-button
    capi:push-button
    :text "Push"
    :data :push-button
    :callback-type :data-interface
    :selection-callback 'button-selection-callback
    :extend-callback    'button-extend-callback
    :retract-callback   'button-retract-callback
    ;; PUSH-BUTTON has alternate callback that can be invoked
    ;; by selecting with CONTROL (on Windows and GTK) or Command
    ;; (on Cocoa)
    :alternate-callback 'button-alternate-callback
    )
   (check-button
    capi:check-button
    :text "Check"
    :data :check-button
    :callback-type :data-interface
    :selection-callback 'button-selection-callback
    :extend-callback    'button-extend-callback
    :retract-callback   'button-retract-callback)
   (radio-button
    capi:radio-button
    :text "Radio"
    :data :radio-button
    :callback-type :data-interface
    :selection-callback 'button-selection-callback
    :extend-callback    'button-extend-callback
    :retract-callback   'button-retract-callback)
   (push-button-panel
    capi:push-button-panel
    :items '("push 1" "push 2" "push 3")
    :callback-type :data-interface
    :selection-callback 'button-selection-callback
    :extend-callback    'button-extend-callback
    :retract-callback   'button-retract-callback)
   (check-button-panel
    capi:check-button-panel
    :items '("check 1" "check 2" "check 3")
    :callback-type :data-interface
    :selection-callback 'button-selection-callback
    :extend-callback    'button-extend-callback
    :retract-callback   'button-retract-callback)
   (radio-button-panel
    capi:radio-button-panel
    :items '("Radio 1" "Radio 2" "Radio 3")
    :callback-type :data-interface
    :selection-callback 'button-selection-callback
    :extend-callback    'button-extend-callback
    :retract-callback   'button-retract-callback)
   (op
    capi:collector-pane
    :initial-constraints '(:visible-min-width (character 25)))
    )
  (:layouts
   (row
    capi:row-layout
    '(push-button radio-button check-button)
    :y-adjust :centre)
   (default-layout
    capi:column-layout
    '(row push-button-panel check-button-panel radio-button-panel op)))
  (:default-initargs
   :layout 'default-layout
   :title "Button Test"))


;;----------------------------------------------------------------------------
;; A generic callback
;;----------------------------------------------------------------------------

(defun button-callback (type data interface)
  (format (capi:collector-pane-stream(slot-value interface 'op))
          "~S ~a~%" data type))

(defun button-selection-callback (&rest args)
  (apply 'button-callback "selected" args))

(defun button-extend-callback (&rest args)
  (apply 'button-callback "extended" args))

(defun button-retract-callback (&rest args)
  (apply 'button-callback "retracted" args))

(defun button-alternate-callback (&rest args)
  (apply 'button-callback "alternate selected" args))

;;----------------------------------------------------------------------------
;; Image Buttons
;;----------------------------------------------------------------------------

;; These forms were derived via something like:
;; (let ((external-image (gp:read-external-image <file>)))
;;  (gp:compress-external-image external-image)
;;  (setf (gp:external-image-transparent-color-index external-image) 0)
;;  (make-load-form external-image))
;; where <file> contains the DIB format representation of an image.

(defparameter *dont-button*
  (make-instance 
   'gp:external-image
   :data
   #(66 77 66 2 0 0 0 0 0 0 66 0 0 0 40 0 0 0 32 0 0 0 32 0 0 0 1 0 4 0 2 0
        0 0 54 1 0 0 109 11 0 0 109 11 0 0 3 0 0 0 3 0 0 0 0 0 0 0 255 0 0 0 0 255
        0 0 32 0 0 0 32 0 0 0 32 0 0 0 32 0 0 0 32 0 0 0 11 0 0 4 34 32 5 0 2 34
        10 0 0 0 10 0 0 4 34 32 6 0 4 34 8 0 0 0 11 0 0 4 34 32 4 0 0 4 34 32 9 0
        0 0 11 0 0 4 34 32 4 0 0 4 34 32 9 0 0 0 12 0 0 10 34 32 0 34 32 0 10 0 0 0
        2 1 10 17 0 10 34 33 17 34 33 0 7 17 0 3 0 0 0 0 2 0 11 17 0 8 34 33 34 33 9 17
        2 0 0 0 13 0 7 34 12 0 0 0 10 0 0 4 32 0 5 34 13 0 0 0 10 0 0 4 34 0 4 34
        14 0 0 0 10 0 0 4 34 0 4 34 4 0 2 34 8 0 0 0 11 0 0 12 34 2 34 32 0 34 9 0
        0 0 11 0 0 12 34 2 34 32 2 32 9 0 0 0 12 0 6 34 0 4 2 32 10 0 0 0 12 0 8 34
        12 0 0 0 13 0 6 34 13 0 0 0 14 0 4 34 14 0 0 0 15 0 0 4 34 32 13 0 0 0 15 0
        4 34 13 0 0 0 14 0 6 34 12 0 0 0 14 0 6 34 12 0 0 0 15 0 4 34 13 0 0 0 16 0
        2 34 14 0 0 0 32 0 0 0 32 0 0 0 32 0 0 0 32 0 0 0 0 1)
   :transparent-color-index 0))

(defparameter *do-button*
  (make-instance 
   'gp:external-image
   :data
   #(66 77 66 2 0 0 0 0 0 0 66 0 0 0 40 0 0 0 32 0 0 0 32 0 0 0 1 0 4 0 2 0
        0 0 14 1 0 0 109 11 0 0 109 11 0 0 3 0 0 0 3 0 0 0 0 0 0 0 255 0 0 0 0 0
        255 0 32 0 0 0 32 0 0 0 32 0 0 0 2 0 29 17 1 0 0 0 2 0 29 17 1 0 0 0 32 0
        0 0 12 0 0 8 34 32 34 32 12 0 0 0 12 0 0 8 34 32 34 32 12 0 0 0 12 0 0 8 34 32
        34 32 12 0 0 0 12 0 0 8 34 32 34 32 12 0 0 0 12 0 0 8 34 32 34 32 12 0 0 0 12 0
        0 8 34 32 34 32 12 0 0 0 12 0 0 8 34 32 34 32 12 0 0 0 12 0 0 8 34 32 34 32 12 0
        0 0 12 0 0 8 34 32 34 32 12 0 0 0 10 0 11 34 11 0 0 0 10 0 11 34 11 0 0 0 10 0
        11 34 11 0 0 0 10 0 11 34 11 0 0 0 10 0 11 34 11 0 0 0 10 0 11 34 11 0 0 0 10 0
        11 34 11 0 0 0 10 0 11 34 11 0 0 0 11 0 9 34 12 0 0 0 14 0 0 4 34 32 14 0 0 0
        13 0 5 34 14 0 0 0 13 0 5 34 14 0 0 0 13 0 5 34 14 0 0 0 14 0 0 4 34 32 14 0
        0 0 32 0 0 0 32 0 0 0 32 0 0 0 0 1) :transparent-color-index 0))

(defparameter *dont-dis-button*
  (make-instance 
   'gp:external-image
   :data
   #(66 77 86 2 0 0 0 0 0 0 86 0 0 0 40 0 0 0 32 0 0 0 32 0 0 0 1 0 4 0 2 0
        0 0 132 1 0 0 109 11 0 0 109 11 0 0 8 0 0 0 8 0 0 0 0 0 0 0 126 84 169 0 169 169
        169 0 212 169 169 0 169 255 169 0 169 89 174 0 84 174 174 0 126 92 177 0 32 0 0 0 32 0 0 0 32 0
        0 0 32 0 0 0 11 0 0 4 85 80 5 0 2 85 10 0 0 0 10 0 0 14 84 68 80 0 5 68 85 0
        8 0 0 0 9 0 0 6 84 4 80 0 4 0 0 6 84 4 69 0 7 0 0 0 10 0 0 14 84 4 80 0
        84 4 85 0 8 0 0 0 10 0 0 14 84 4 80 0 84 4 80 0 8 0 0 0 11 102 0 12 116 4 102 100
        4 22 6 102 0 3 0 6 0 0 2 99 10 51 0 10 64 67 51 64 67 0 7 51 0 3 96 0 0 0 2 6
        11 51 0 8 64 67 64 67 9 51 2 96 0 0 2 0 11 102 0 8 64 4 0 69 10 102 1 0 0 0 9 0
        0 12 84 82 84 0 4 80 11 0 0 0 9 0 0 16 84 69 84 0 69 0 5 80 7 0 0 0 9 0 0 16
        84 5 84 0 69 0 84 69 7 0 0 0 10 0 0 14 80 69 64 4 80 84 69 0 8 0 0 0 10 0 0 14
        84 5 64 4 85 68 80 0 8 0 0 0 11 0 0 12 80 64 0 69 68 80 9 0 0 0 11 0 2 84 6 0
        2 69 11 0 0 0 12 0 2 84 4 0 2 69 12 0 0 0 13 0 0 6 84 0 69 0 13 0 0 0 14 0
        0 6 84 4 80 0 12 0 0 0 14 0 0 6 84 0 69 0 12 0 0 0 13 0 2 84 4 0 2 69 11 0
        0 0 13 0 2 84 4 0 2 69 11 0 0 0 14 0 0 6 84 0 69 0 12 0 0 0 15 0 0 4 84 69
        13 0 0 0 16 0 2 85 14 0 0 0 32 0 0 0 32 0 0 0 32 0 0 0 0 1) :transparent-color-index 0))

(defparameter *do-dis-button*
  (make-instance
   'gp:external-image
   :data
   #(66 77 78 2 0 0 0 0 0 0 78 0 0 0 40 0 0 0 32 0 0 0 32 0 0 0 1 0 4 0 2 0
        0 0 106 1 0 0 109 11 0 0 109 11 0 0 6 0 0 0 6 0 0 0 0 0 0 0 255 255 116 0 227 135
        135 0 230 230 138 0 161 207 207 0 135 135 227 0 32 0 0 0 32 0 0 0 2 0 29 68 1 0 0 0 2 4
        29 34 1 64 0 0 2 4 29 34 1 64 0 0 2 0 29 68 1 0 0 0 11 0 0 10 53 85 53 85 48 0
        11 0 0 0 11 0 0 10 53 5 53 5 48 0 11 0 0 0 11 0 0 10 53 5 53 5 48 0 11 0 0 0
        11 0 0 10 53 5 53 5 48 0 11 0 0 0 11 0 0 10 53 5 53 5 48 0 11 0 0 0 11 0 0 10
        53 5 53 5 48 0 11 0 0 0 11 0 0 10 53 5 53 5 48 0 11 0 0 0 11 0 0 10 53 5 53 5
        48 0 11 0 0 0 10 0 0 12 51 80 83 80 83 48 10 0 0 0 9 0 0 14 53 80 0 80 0 85 48 0
        9 0 0 0 9 0 2 53 9 0 2 83 10 0 0 0 9 0 2 53 9 0 2 83 10 0 0 0 9 0 2 53
        9 0 2 83 10 0 0 0 9 0 2 53 9 0 2 83 10 0 0 0 9 0 2 53 9 0 2 83 10 0 0 0
        9 0 2 53 9 0 2 83 10 0 0 0 9 0 2 53 9 0 2 83 10 0 0 0 10 0 0 12 53 85 0 5
        85 48 10 0 0 0 11 0 0 10 51 53 5 19 48 0 11 0 0 0 12 0 0 8 53 0 5 48 12 0 0 0
        12 0 0 8 53 0 5 48 12 0 0 0 12 0 0 8 53 0 5 48 12 0 0 0 13 0 0 6 53 85 48 0
        13 0 0 0 14 0 0 4 51 48 14 0 0 0 32 0 0 0 32 0 0 0 0 1) :transparent-color-index 0))



;;------------------------------------------------------------

;; Register the images in the image translation table.
;; From henceforth they can be referenced via the ids

(mapc #'(lambda (id image)
          (gp:register-image-translation id image))
      '(:do :dont :do-disabled :dont-disabled)
      (list *do-button* *dont-button* *do-dis-button* *dont-dis-button*))

;; This shows the effects of button classes with and without indicators.
;; Note that :indicator, :disabled-images, :selected-disabled-images are ignored on Windows.
;; The images in the buttons are used shared resources.

(capi:define-interface image-button-example ()
  ()
  (:panes 
   (buttons1 capi:radio-button-panel
             :items '(1 2)
             :images (list :do :do)
             :disabled-images (list :do-disabled :do-disabled)              
             :selected-images (list :dont :dont)
             :selected-disabled-images (list :dont-disabled :dont-disabled) 
             :indicator T)                                                  
   (buttons2 capi:radio-button-panel
             :items '(1 2)
             :images (list :do :do)
             :disabled-images (list :do-disabled :do-disabled)              
             :selected-images (list :dont :dont)
             :selected-disabled-images (list :dont-disabled :dont-disabled) 
             :indicator nil)                                                
   (buttons3 capi:check-button-panel
             :items '(1 2)
             :images (list :do :do)
             :disabled-images (list :do-disabled :do-disabled)              
             :selected-images (list :dont :dont)
             :selected-disabled-images (list :dont-disabled :dont-disabled) 
             :indicator T)                                                  
   (buttons4 capi:check-button-panel
             :items '(1 2)
             :images (list :do :do)
             :disabled-images (list :do-disabled :do-disabled)
             :selected-images (list :dont :dont)
             :selected-disabled-images (list :dont-disabled :dont-disabled)
             :indicator nil)
   (disabler capi:radio-button-panel :items '("Enable" "Disable")
             :callback-type :none
             :layout-class 'capi:column-layout
             :selection-callback #'(lambda ()
                                     (mapc #'(lambda (b)
                                               (setf (capi:simple-pane-enabled b) (not (capi:simple-pane-enabled b))))
                                           (list buttons1 buttons2 buttons3 buttons4))))
   (image-explainer capi:output-pane
                    :visible-min-height 160
                    :visible-max-height t
                    :title "Key to images"
                    :title-position :frame
                    :visible-border nil
                    :visible-min-width 180
                    :display-callback 
                    'display-images-with-titles))
  (:layouts 
   (one capi:column-layout '(buttons1 buttons2) :title "Radio" :title-position :frame)
   (two capi:column-layout '(buttons3 buttons4) :title "Check" :title-position :frame)
   (interactive-layout  capi:column-layout  '(one two disabler) )
   (main  capi:row-layout  '(interactive-layout image-explainer) :default t))
  (:default-initargs
   :title "Image Button Example 2"))

(defun display-image-with-title (pane top-y title image)
  (gp:draw-string pane title 50 (+ top-y 30))
  (gp:draw-image pane (gp:load-image pane image) 10 (+ top-y 5)))

(defun display-images-with-titles (pane x y width height)
  (declare (ignore x y width height))
  (display-image-with-title pane 0 "Default image" :do)
  (display-image-with-title pane 40 "Selected image" :dont)
  (if (member (capi:default-library) '(:GTK :Cocoa))
      (gp:draw-string pane "Automatic disabling" 10 110)
    (progn 
      (display-image-with-title pane 80 "Default disabled" :do-disabled)
      (display-image-with-title pane 120 "Selcted disabled" :dont-disabled))))
;;----------------------------------------------------------------------------
;; Test button-test
;;----------------------------------------------------------------------------

(defun test-buttons ()
  (let(( first (capi:display (make-instance 'button-test))))
    (multiple-value-bind (x y width height)
        (capi:top-level-interface-geometry first)
      (declare (ignore x width height))
      (let ((second (make-instance 'image-button-example)))
        (capi:display second)
        (capi:set-top-level-interface-geometry second :y (+ y 300))))))


