;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/choice:prompt-with-buttons.lisp,v 1.3.1.1 2011/08/24 13:26:21 davef Exp $" -*-

;;----------------------------------------------------------------------------
;;
;; examples/capi/choice/prompt-with-buttons.lisp
;;
;; This example illustrates how you can modify CAPI:PROMPT-WITH-LIST
;; to use choice classes other than CAPI:LIST-PANEL.
;;
;; To try it, compile and load this file and then execute:
 
;;; (PROMPT-WITH-LIST-WITH-BUTTONS (APROPOS-LIST "PRINT" 'CL) 4 :PUSH)
;;; (PROMPT-WITH-LIST-WITH-BUTTONS (APROPOS-LIST "LIST" 'CL) 2 :CHECK)
;;; (PROMPT-WITH-LIST-WITH-BUTTONS (REGEXP-FIND-SYMBOLS "^C.*R$" :PACKAGES '(CL)) 3 :RADIO)

;;; With :PUSH, clicking any button causes it to use that item, and dismisses the dialog.
;;; With :CHECK and :RADIO you need to press OK after selecting items.
;;; Once the dialog is dismissed it displays a message with the selection. 

;;----------------------------------------------------------------------------
;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
;;----------------------------------------------------------------------------

(in-package "CL-USER")

;;; PROMPT-WITH-LIST normally uses LIST-PANEL, but here we have a
;;; special case. The class is defined as some BUTTON-PANEL, and here
;;; we compute the arguments for it.

;;; a) To be able to have more then one column we tell it
;;;    to use a GRID-LAYOUT. 
;;; b) Tell it not to use a vertical scroll.
;;; c) Pass LAYOUT-ARGS for the GRID-LAYOUT. These contain:
;;;   i)   column number
;;;   ii)  y-gap
;;;   iii) Correct minimum height. That is not really possible,
;;;   because currently CAPI does not give the programmer a way of
;;;   finding the height of a button in a button panel. Instead, the
;;;   code here takes the character height, adds the y-gap, and hacks in
;;;   another few pixels as a guess for the difference between the
;;;   height of a button and the height of the text.
;;;   iv) In case the min-height is wrong, make it use a vertical scroll bar.

(defun construct-button-panel-args (items col-num kind)
  (let* ((row-number (ceiling  (length items) col-num))
         (y-gap 2)
         (extra-height (* y-gap row-number))
         (hack-extra-gap (* row-number (if (eq kind :push) 12 4) )))
    `(:vertical-scroll nil
      :layout-class capi:grid-layout
      :layout-args (:columns ,col-num
                    :vertical-scroll t
                    :y-gap ,y-gap
                    :visible-min-height 
                    (max 200 (min 400 (+  ,extra-height ,hack-extra-gap (character , row-number))))))))

;;; calling PROMPT-WITH-LIST: 
;;; We pass it the CHOICE-CLASS to get a BUTTON-PANEL rather than the
;;; default LIST-PANEL. 
;;; When it is :PUSH we want clicking a button to take it and dismiss the 
;;; dialog, so we pass a SELECTION-CALLBACK that does this, and we eliminate
;;; the OK button. 
;;; The INTERACTION is :MULTIPLE for :CHECK.
;;; We pass some special arguments for the BUTTON-PANEL via :PANE-ARGS. 

(defun prompt-with-list-with-buttons (items col-num kind)
  (multiple-value-bind (res ok-p)
      (capi:prompt-with-list 
       items (format nil "Prompting ~d items with kind ~a"
                     (length items) kind)
       :choice-class (case kind
                       (:push 'capi:push-button-panel)
                       (:check 'capi:check-button-panel)
                       (:radio 'capi:radio-button-panel))
       :popup-args (when (eq kind :push) '(:ok-button nil)) ; The selection-callback does it
       :interaction (if (eq kind :check) :multiple-selection :single-selection)
       :selection-callback (when (eq kind :push)
                             #'(lambda (item confirmer)
                                 (declare (ignore confirmer))
                                 (capi:exit-dialog item)))
       :print-function 'string-capitalize
       :pane-args (construct-button-panel-args items col-num kind))
    (when ok-p
      (capi:display-message "Got ~a" res))))
