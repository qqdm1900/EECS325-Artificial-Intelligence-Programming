;; -*- rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/elements:text-input-choice.lisp,v 1.4.1.1 2011/08/24 13:26:21 davef Exp $" -*-

;;----------------------------------------------------------------------------
;;
;; examples/capi/elements/text-input-choice.lisp
;;
;; This example demonstrates the uses of text-input-choice in the CAPI.
;; To try it, compile and load this file and then execute:
;;
;;      (CL-USER::TEST-TEXT-INPUT-CHOICE)
;;
;; At the top there is a text-input-choice titled "Enter text" with three items: 
;; "Item 0", "Item 1" and "Item 2".

;; Below there are three display panes, "Selection", "Text" and "Caret", which
;; display the index of the last selected item, the actual text in the
;; text-input-choice, and the caret position in the text-input-choice. They are 
;; updated when the text-input-choice is modified by the user. 

;; Below the display panes there is an option-pane "Max chars.", which lets
;; you set the maximum number of characters in the text-input-choice.
;; There is also a radio button panel which lets you enable or disable the
;; text-input-choice, or make it read-only (cannot type, but can select from
;; the menu and by arrow keys, and can also select part of the text and copy). 

;; Below that there is a button that insert random text into the text-input-choice. 

;; At the bottom there is another text-input-choice in a framed title "Random items".
;; Each time you pop up the menu it display a random it displays a random set of
;; common-lisp symbols.  
;;----------------------------------------------------------------------------
;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
;;----------------------------------------------------------------------------


(in-package "CL-USER")


(defvar *max-chars* 20)

(capi:define-interface text-input-choice-test ()
  () ;
  (:panes
   (text-input-choice
    capi:text-input-choice
    :accessor text-input-choice
    :items '("Item 0" "Item 1" "Item 2")
    :change-callback 'text-input-choice-test-change-callback
    :max-characters *max-chars*
    :callback-type :interface-data
    :callback 'text-input-choice-test-callback
    :selection-callback 'text-input-choice-test-selection-callback)
   (selection-pane
    capi:display-pane
    :accessor selection-pane
    :text "0")
   (text-pane
    capi:display-pane
    :accessor text-pane
    :min-height '(character 1)
    :max-width nil)
   (caret-pane
    capi:display-pane
    :accessor caret-pane
    :min-height '(character 1)
    :max-width nil)
   (max-characters-pane
    capi:option-pane
    :items '(2 3 5 8 13 20 30 50 80 130)
    :selected-item *max-chars*
    :callback-type :interface-data
    :selection-callback 'set-choice-max-characters)
   (disable-button
    capi:radio-button-panel
    :title "Enabled"
    :title-position :left
    :items '(:yes :no :read-only)
    :print-function 'string-capitalize
    :selected-item :yes
    :callback-type :interface-data
    :selection-callback 'update-text-input-choice-state)
   (randomize-button
    capi:push-button
    :text "Randomize Text"
    :callback 'randomize-text-input-choice-test
    :callback-type :interface)
   (random-text-input-choice
    capi:text-input-choice
    :reader random-text-input-choice-pane
    :title "Random items"
    :title-position :frame
    :items '(1+ replace sin)
    :popup-callback 'text-input-choice-popping-up))
  (:layouts
   (text-pane-layout
    capi:grid-layout
    '("Enter text:" text-input-choice
      "Selection:" selection-pane
      "Text:" text-pane
      "Caret:" caret-pane
      "Max Chars:" max-characters-pane)
    :y-adjust :center)
   (callback-layout
    capi:column-layout
    '(text-pane-layout disable-button randomize-button)
    :title "Callbacks"
    :title-position :frame)
   (main-layout
    capi:column-layout
    '(callback-layout random-text-input-choice)))
  (:default-initargs
   :title "Text Input Pane Test"
   :best-width 300
   :layout 'main-layout))


;;----------------------------------------------------------------------------
;; Callbacks
;;----------------------------------------------------------------------------

(defun text-input-choice-test-callback (interface text)
  (declare (ignore interface))
  (capi:display-message "~S" text))

(defun text-input-choice-test-change-callback (text pane interface caret-position)
  (declare (ignore pane))
  (setf (capi:display-pane-text (caret-pane interface))
        (princ-to-string caret-position))
  (setf (capi:display-pane-text (text-pane interface))
        text))

(defun text-input-choice-test-selection-callback (interface text)
  (let ((selection (position text (capi:collection-items
                                   (text-input-choice interface))
                             :test 'equal)))
    (setf (capi:display-pane-text (selection-pane interface))
          (if selection
              (prin1-to-string selection)
            ""))))

(defun update-text-input-choice-state (self enabled)
  (setf (capi:text-input-pane-enabled (text-input-choice self))
        (case enabled
          (:yes t)
          (:no nil)
          (otherwise :read-only))))

(defun text-input-choice-set-selection (self selection-text)
  (let ((index (parse-integer selection-text :junk-allowed t))
        (choice (text-input-choice self)))
    (if (and index
             (>= (capi:count-collection-items choice) index 0))
        (setf (capi:choice-selection choice) index)
      (capi:display-message "Not a valid index (must be integer in the inclusive ange 0-2: ~a" selection-text))))

#-:Win32
(defun validate-choice-text (text pane interface caret-position)
  (declare (ignore pane interface caret-position))
  (multiple-value-bind
      (integer length)
      (parse-integer text :junk-allowed t)
    (declare (ignore integer))
    (eq length (length text))))

(defun set-choice-max-characters (self value)
  (let ((pane (text-input-choice self)))
    (setf (capi:text-input-pane-max-characters pane) value)
    
    ;; Setting the maximum may truncate the text, but
    ;; it does not invoke the callback, so we need to update
    ;; the other panes by hand.
    (setf (capi:display-pane-text (text-pane self))
        (capi:text-input-pane-text pane)
       (capi:display-pane-text (caret-pane self))
       (princ-to-string (capi:text-input-pane-caret-position pane)))))

(defun randomize-text-input-choice-test (self)
  (let ((pane (text-input-choice self)))
    (if (zerop (random 2))
        (setf (capi:text-input-pane-text pane)
              (with-output-to-string (text)
                (loop repeat (+ 2 (random 8))
                      do
                      (write-char (code-char (+ (char-code #\a)
                                                (random 26)))
                                  text))))
      (setf (capi:text-input-pane-caret-position pane)
            (random (length (capi:text-input-pane-text pane)))))))

(defun text-input-choice-popping-up (pane)
  (let ((random-x (+ 100 (random 100))))
    (setf (capi:collection-items pane)
          (loop for symbol being the external-symbols of "COMMON-LISP"
                with next-index = (random random-x) do (decf next-index)
                when (=  next-index -1)
                do (setq next-index (random random-x)) and 
                collect symbol))))


;;----------------------------------------------------------------------------
;; test-text-input-choice
;;----------------------------------------------------------------------------

(defun test-text-input-choice ()
  (capi:display (make-instance 'text-input-choice-test)))
