;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/editor:change-callback.lisp,v 1.4.1.1 2011/08/24 13:26:20 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

;;----------------------------------------------------------------------------
;;
;; examples/capi/editor/change-callback.lisp
;;
;; This example demonstrates using the CHANGE-CALLBACK
;;; of an EDITOR-PANE  
;; To try it, compile and load this file and then execute:
;;
;;      (CL-USER::TEST-EDITOR-PANE-CHANGE-CALLBACK)
;;
;; Then type some text into the editor-pane, and when you type
;; a space it colours the last word according to its length:
;; 1-2    Red
;; 3-5    Blue
;; 6-8    Purple
;; 9-13   Yellow
;; >14    Green
;; 
;;---------------------------------

(in-package "CL-USER")


(defvar *faces-array*
  (vector (editor:make-face :red :foreground :red :if-exists t)
          (editor:make-face :blue :foreground :blue :if-exists t)
          (editor:make-face :purple :foreground :purple :if-exists t)
          (editor:make-face :yellow :foreground :yellow :if-exists t)
          (editor:make-face :green :foreground :green :if-exists t)))

;;; Arbitrary algorithm to decide what color
(defun face-for-length (len)
  (svref *faces-array*
         (if (> len 14)
             4
           (truncate len 3))))

(defun colour-last-word (pane point old-len new-len)
  (declare (ignore pane))
  (editor:with-point-locked
      (point :errorp nil)
    (when  (and (= old-len 0)
                (= new-len 1) ;;inserted one character
                (eq (editor:character-at point 0) #\space));; which is a space
      
      (let ((minus-len -1))
      
        ;;; find the length of the sequence of alphanumeric characters backwards
        (loop 
         (let ((char (editor:character-at point minus-len)))
           (unless (and char  ;; char is NIL if beginning of buffer
                        (alphanumericp char))
             (incf minus-len)
             (return)))
         (decf minus-len))

        (editor:with-point ((start point))
          (editor:character-offset start minus-len) ;; start to the beginning of the word
          (editor:put-text-property  ;; colour the word
                                     start point 
                                     'editor:face 
                                     (face-for-length (- minus-len) )))))))
                                        

(defun test-editor-pane-change-callback ()
  (capi:contain (make-instance 
                 'capi:editor-pane
                 :change-callback 'colour-last-word
                 :buffer (format nil "~a" (gensym "new buffer ")) ;;unique buffer
                 :flag :output  ;;; don't bother to save
                 )))        
