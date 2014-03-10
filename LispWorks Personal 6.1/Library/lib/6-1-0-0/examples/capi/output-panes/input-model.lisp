;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/output-panes:input-model.lisp,v 1.12.2.1 2011/08/24 13:26:20 davef Exp $" -*-

;;----------------------------------------------------------------------------
;;
;; examples/capi/output-panes/input-model.lisp
;;
;; This example demonstrates the input model of an output pane
;; To try it, compile and load this file and then execute:
;;
;;      (CL-USER::TEST-INPUT-MODEL)
;;
;; 
;; Type characters at the black "input", or click or drag
;; the mouse. All the display is done by the callbacks in 
;; the input-model. 
;;
;; A character input is displayed at the point where the cursor
;; is when it the input is received. When the key also matches
;; one of the entries for keys with a non-nil colour, it displays 
;; a rectangle in this colour. 
;;
;; A button press is marked by P, and a button release by R.

;; In the input model, the function is always called *-input-callback. 
;; The first argument after the implicit arguments is always a string
;; which is used by the function as the callback-name. For the character
;; and key entries, there is additional argument which is a colour to use. 

;; In the display-panes below the black pane it displays for the three 
;; callback types the name of the last callback of this type. For the key
;; and character callbacks it also displays the actual key/character that
;; the callback received. 

;; When event is received 

;;----------------------------------------------------------------------------
;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
;;----------------------------------------------------------------------------


(in-package "CL-USER")

(capi:define-interface test-input-model
    (capi:interface)

  ()
  (:panes
   (typein-pane
    capi:output-pane
    :title "Input"
    :title-position :frame
    :background :black
    :foreground :white
    :input-model `(
                   ;; Character gestures.
                   ;; These take precedence over the :character gesture.
                   ((#\A) 
                    char-input-callback
                    "A" :red)
                   ((#\a) ;this is the one you will see
                    char-input-callback
                    "a" :red)
                   ((#\control-\a) ;this is the one you will see
                    char-input-callback
                    "Ctrl-a" :red)

                   ;; remaining character gestures.
                   (:character
                    char-input-callback
                    "Any char" :green)

                   ;; Key gestures.
                   ;; These take precedence over the :key :press gesture.
                   ((#\a :press)
                    key-input-callback
                    "a" :lightblue)
                   ((#\a :press :meta)
                    key-input-callback
                    "Meta-a" :lightblue)
                   ((#\a :press :meta :shift)
                    key-input-callback
                    "Meta-Shift-a" :lightblue)
                   ((:key :up :press)
                    key-input-callback
                    "Up" :red)
                   ((#\Backspace :press)
                    key-input-callback
                    "Backspace" :red)
                   ((#\Delete :press)
                    key-input-callback
                    "Delete" :lightblue)

                    ;; remaining key gestures
                   ((:key :press)
                    key-input-callback
                    "Any key" nil)

                   ;; Button gestures.

                   ((:button-1 :press)
                    button-input-callback
                    "Button-1 press" t)
                   ((:button-1 :press :shift)
                    button-input-callback
                    "Shift Button-1 press" t)
                   ((:button-1 :press :control)
                    button-input-callback
                    "Control Button-1 press" t)

                   ((:button-1 :release)
                    button-input-callback
                    "Button-1 release" nil)
                   ((:button-1 :release :shift)
                    button-input-callback
                    "Shift Button-1 release" nil)
                   ((:button-1 :release :control)
                    button-input-callback
                    "Control Button-1 release" nil)

                   ;; Motion gestures.

                   ((:motion :button-1)
                    motion-input-callback
                    "Button-1 drag")
                   ))
                   
   (last-key-pane
    capi:display-pane
    :visible-max-width nil)
   (last-char-pane
    capi:display-pane
    :visible-max-width nil)
   (last-mouse-pane
    capi:display-pane
    :visible-max-width nil))
  (:layouts
   (info-layout
    capi:grid-layout
    '("Last key:"
      last-key-pane
      "Last char:"
      last-char-pane
      "Last mouse:"
      last-mouse-pane)
    :has-title-column-p t
    :title "Callback Info"
    :title-position :frame)
   (main-layout
    capi:column-layout
    '(typein-pane info-layout)))
  (:default-initargs
   :layout 'main-layout
   :title "Input Model Test"
   :best-width 400
   :best-height 400))

(defun char-input-callback (self x y character callback-name colour)
  (display-message self (format nil "~A   [ ~A ]" 
                                callback-name 
                                (if (simple-char-p character)
                                    character
                                  (with-output-to-string (out)
                                    (sys:print-pretty-gesture-spec
                                     (sys:coerce-to-gesture-spec character)
                                     out))))
                   'last-char-pane)
  (gp:draw-character self character x y :graphics-args `(:foreground ,colour)))

(defun motion-input-callback (self x y callback-name)
  (display-message self callback-name 'last-mouse-pane)
  (gp:draw-point self x y))

(defun button-input-callback (self x y callback-name press-p)
  (display-message self callback-name 'last-mouse-pane)
  (gp:draw-character self (if press-p #\P #\R) x y))

(defun key-input-callback (self x y key callback-name colour)
  (let ((message (with-output-to-string (out)
                   (format out  "~A    [ " callback-name)
                   (sys:print-pretty-gesture-spec
                    key out
                    :force-shift-for-upcase nil)
                   (format out " ]"))))
    (display-message self message 'last-key-pane)
    (when colour
      (gp:draw-rectangle self (- x 5) (- y 5) 10 10 
                         :filled t :graphics-args `(:foreground ,colour)))))

(defun display-message (self message pane-slot)
  (let ((interface (capi:element-interface self)))
    (let ((display-pane (slot-value interface pane-slot)))
      (setf (capi:display-pane-text display-pane)
            message))))

(defun test-input-model ()
  (capi:contain (make-instance 'test-input-model)))
