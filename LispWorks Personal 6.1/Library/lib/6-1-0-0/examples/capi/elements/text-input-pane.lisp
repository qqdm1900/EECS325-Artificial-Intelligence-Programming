;; -*- rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/elements:text-input-pane.lisp,v 1.8.2.1 2011/08/24 13:26:20 davef Exp $" -*-

;;;----------------------------------------------------------------------------
;;;
;;; examples/capi/elements/text-input-pane.lisp
;;;
;;; This example demonstrates the uses of text-input-panes in the CAPI.
;;; To try it, compile and load this file and then execute:
;;;
;;;      (CL-USER::TEST-TEXT-INPUT-PANE)
;;;
;;; The top pane is the test pane. The state of the pane is displayed
;;; below it. "Editing" is true when it has the focus, so keyboard input
;;; changes the text. You can set the maximum characters that the test
;;; pane accepts and its enabled state. The "Randomize" buttons set
;;; the text to a random string, or the position of the caret to a random
;;; position. 
;;; The three panes at the bottom (inside "visible Borders") show the
;;; apperace of a TEXT-INPUT-PANE with different kinds of borders. 
;;; 
;;; The example can also show MULTI-LINE-TEXT-INPUT-PANE by executing:
;;; 
;;;      (CL-USER::TEST-MULTI-LINE-TEXT-INPUT-PANE)
;;;----------------------------------------------------------------------------
;;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
;;;----------------------------------------------------------------------------


(in-package "CL-USER")


(defvar *max-chars* 20)

(capi:define-interface text-input-pane-test ()
  ((the-pane :accessor text-input-pane-test-pane))
  (:panes
   (text-input-pane
    capi:text-input-pane
    :Title "Input pane:"
    :change-callback 'test-change-callback
    :max-characters *max-chars*
    :callback-type :data
    :editing-callback 'test-editing-callback
    :callback 'test-callback)
   (multi-line-text-input-pane
    capi:multi-line-text-input-pane
    :Title "Input pane:"
    :change-callback 'test-change-callback
    :max-characters *max-chars*
    :callback-type :data
    :visible-min-height '(:character 3)
    :editing-callback 'test-editing-callback)
   (text-pane
    capi:title-pane
    :accessor text-pane
    :foreground :red
    :text "<empty>"
    :min-height '(character 1)
    :min-width nil
    :max-width nil)
   (caret-pane
    capi:title-pane
    :foreground :red
    :text "0"
    :accessor caret-pane
    :min-height '(character 1)
    :max-width nil)
   (max-characters-pane
    capi:text-input-pane
    :title "Maximum Characters:"
    :text (princ-to-string *max-chars*)
    #-:Win32 :confirm-change-function #-:Win32 'validate-text ; not supported on Windows
    :callback-type :interface-data
    :visible-min-width :text-width
    :editing-callback #'(lambda (pane state)
                          (when (eq state :end)
                            (set-max-characters (capi:top-level-interface pane)
                                                (capi:text-input-pane-text pane))))
    :callback 'set-max-characters)
   (editing-pane
    capi:title-pane
    :foreground :red
    :accessor editing-pane
    :max-width nil)
   (disable-button
    capi:radio-button-panel
    :title "Enabled"
    :title-position :left
    :items '(:yes :no :read-only)
    :print-function 'string-capitalize
    :selected-item :yes
    :callback-type :interface-data
    :selection-callback 'update-text-input-pane-state)
   (randomize-text-button
    capi:push-button
    :text "Text"
    :callback 'randomize-text-input-pane-test-text
    :callback-type :interface)
   (randomize-position-button
    capi:push-button
    :text "Position"
    :callback 'randomize-text-input-pane-test-position
    :callback-type :interface))
  (:layouts
   (state-layout
    capi:grid-layout
    '(
      "Text:" text-pane
      "Caret:" caret-pane
      "Editing:" editing-pane
     
     )
    :Title "State"
    :title-position :frame
    :y-adjust :center)
   (text-pane-layout
    capi:column-layout
    '( text-input-pane state-layout))
   
   (randomize-layout
    capi:row-layout
    '(randomize-text-button randomize-position-button)
    :title "Randomize: "
    :title-position :left)
   (callback-layout
    capi:column-layout
    '(text-pane-layout  max-characters-pane disable-button randomize-layout)
    :title "Callbacks"
    :title-position :frame
    :y-gap 10)
   (border-layout
    capi:grid-layout
    (loop for (border . title) in '((:default . "Default")
                                    (:outline . "Outline")
                                    (nil . "None"))
          collect title
          collect (make-instance 'capi:text-input-pane
                                 :visible-border border))
    :y-adjust :center
    :title "Visible Borders"
    :title-position :frame)
   (main-layout
    capi:column-layout
    '(callback-layout border-layout)))
  (:default-initargs
   :title "Text Input Pane Test"
   :best-width 300
   :best-height nil
   :layout 'main-layout))

(defmethod initialize-instance :after ((interface text-input-pane-test) &key multi-line)
  (with-slots (text-pane-layout text-input-pane multi-line-text-input-pane
                                main-layout
                                #+capi-gtk-lib max-characters-pane) interface
    (if multi-line
        (progn 
          (setf (capi:layout-description text-pane-layout)
                '(multi-line-text-input-pane state-layout)
                (text-input-pane-test-pane interface)
                multi-line-text-input-pane)
          (setf (capi:layout-description main-layout)
                '(callback-layout))
          #+capi-gtk-lib ;;; limit not implemented for multi-line on gtk
          (setf (capi:simple-pane-enabled max-characters-pane) nil
                (capi:text-input-pane-text max-characters-pane) "<GTK: unsupported>"))

      (setf (text-input-pane-test-pane interface) text-input-pane))))


;;----------------------------------------------------------------------------
;; Callbacks
;;----------------------------------------------------------------------------

(defun test-callback (text)
  (capi:display-message "~S" text))

(defun test-change-callback (text pane interface caret-position)
  (declare (ignore pane))
  (setf (capi:title-pane-text (caret-pane interface))
        (princ-to-string caret-position))
  (setf (capi:title-pane-text (text-pane interface))
        (if (eql (length text) 0)
            "<empty>"
          text)))

(defun update-the-state-pane (interface)
  (let* ((pane (text-input-pane-test-pane interface))
         (text (capi:text-input-pane-text pane))
         (pos (capi:text-input-pane-caret-position pane)))
    (test-change-callback text pane interface pos)))

(defun test-editing-callback (text-input-pane state)
  (let ((display-pane (editing-pane (capi:top-level-interface text-input-pane))))
    (setf (capi:title-pane-text display-pane)
          (if (eq state :end) "No" "Yes"))))

(defun update-text-input-pane-state (self enabled)
  (setf (capi:text-input-pane-enabled (text-input-pane-test-pane self))
        (case enabled
          (:yes t)
          (:no nil)
          (otherwise :read-only))))

#-:Win32
(defun validate-text (text pane interface caret-position)
  (declare (ignore pane interface caret-position))
  (multiple-value-bind
      (integer length)
      (parse-integer text :junk-allowed t)
    (declare (ignore integer))
    (eq length (length text))))

(defun set-max-characters (self text)
  (let ((value (parse-integer text :junk-allowed t))
        (pane (text-input-pane-test-pane self)))
    (if (integerp value)
        (setf (capi:text-input-pane-max-characters pane) value)
      (capi:display-message "Non-integer for max-characters: ~S" text))))

(defun randomize-text-input-pane-test-text (self)
  (let ((pane (text-input-pane-test-pane self)))
    (let ((new-string (with-output-to-string (text)
                        (loop repeat (+ 2 (random 8))
                              do
                              (write-char (code-char (+ (char-code #\a)
                                                        (random 26)))
                                          text)))))
      (format t "TEXT-INPUT-PANE-TEST: Setting the text  to ~a~%" new-string)
      (setf (capi:text-input-pane-text pane) new-string)
      ;; The change-callback does not happen when we programmatically 
      ;; set the text, but we want to update the state anyway. 
      (update-the-state-pane self))))



(defun randomize-text-input-pane-test-position (self)
  (let* ((pane (text-input-pane-test-pane self))
         (text (capi:text-input-pane-text pane))
         (new-position (random (length text))))
    (format t "TEXT-INPUT-PANE-TEST: Setting the caret-position  to ~a~%" 
            new-position)
    (setf (capi:text-input-pane-caret-position pane) new-position)
    ;; The change-callback does not happen when we programmatically 
    ;; set the text, but we want to update the state anyway. 
    (update-the-state-pane self)))



;;----------------------------------------------------------------------------
;; test-text-input-panes
;;----------------------------------------------------------------------------

(defun test-text-input-pane ()
  (capi:display (make-instance 'text-input-pane-test)))

(defun test-multi-line-text-input-pane ()
  (capi:display (make-instance 'text-input-pane-test :multi-line t)))
