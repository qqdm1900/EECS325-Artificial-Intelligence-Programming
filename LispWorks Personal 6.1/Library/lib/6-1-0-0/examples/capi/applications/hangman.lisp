;; -*- rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/applications:hangman.lisp,v 1.6.11.1 2011/08/24 13:26:21 davef Exp $ -*-

;;---------------------------------------------------------------------
;;
;; examples/capi/applications/hangman.lisp
;;
;; This example creates a hangman game.
;;
;; To try it, compile and load this file and then execute:
;;
;;      (CL-USER::HANGMAN)
;;
;;---------------------------------------------------------------------
;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
;;---------------------------------------------------------------------


(in-package "CL-USER")

(defstruct (hangman (:print-function %print-hangman))
  gp
  tl
  guessed
  full-word
  full-word-list
  display-word
  wrong-so-far
  (wrong-guesses-allowed 10)
  buttons)

(defun %print-hangman (o s d)
  (declare (ignore d))
  (format s "#<hangman ~a>" (hangman-display-word o)))  

(defun make-new-hangman (&optional old-hangman) 
 (labels ((think-of-a-random-word () 
  ;; on unix, can look in /usr/dict/words, what about pc?
  (let ((words #("harlequin" "lispworks" "brilliant"
                 "common" "lisp" "object" "system"
                 "symbolic" "processing" "paradigm"
                 "barrington" "hall" "cambridge" "boston" "russelsheim"
                 "powerful" "integrated" "development" "environment"
                 "lambda" "closure" "binding" "function"
                 "generous" "academic" "discounts" "available"
                 "everton" "xanalys")))
    (aref words (random (length words))))))
  (let ((man (or old-hangman (make-hangman)))
        (word (string-upcase (think-of-a-random-word))))
    (setf (hangman-wrong-so-far man) 0
          (hangman-full-word man) word
          (hangman-full-word-list man) (coerce word 'list)
          (hangman-guessed man) nil
          (hangman-display-word man) (make-string (length word) :initial-element #\-))
    man)))

(defun hangman-callback (hangman args)
 (labels ((guess (hangman string)
  (unless (or (string-equal (hangman-display-word hangman)
                            (hangman-full-word hangman))
              (>= (hangman-wrong-so-far hangman) 
                  (hangman-wrong-guesses-allowed hangman)))
    (let ((letter (char string 0))) ;; string is of length 1
      (unless (find letter (hangman-guessed hangman))
        (if (member letter (hangman-full-word-list hangman))
	    (dotimes (i (length (hangman-full-word hangman)))
	      (when (char= letter (char (hangman-full-word hangman) i))
	        (setf (char (hangman-display-word hangman) i)
		      (char (hangman-full-word hangman) i))))
          (incf (hangman-wrong-so-far hangman)))
        (setf (hangman-guessed hangman) (push letter (hangman-guessed hangman))))))))
 (let ((key (if (listp args)(car args) args)))
    (guess hangman key)))
  (redraw-hangman hangman))

(defun redraw-hangman (man &optional x y wid hei)
  (declare (ignore x y wid hei))
  (labels ((hangman-line-number (gp i)
    (let ((yscale 2))
      (multiple-value-bind (from-x from-y to-x to-y)
          (case i
            ((0) (values 100 70 10 70))
            ((1) (values  10 70 10 20))
            ((2) (values  10 20 50 20))
            ((3) (values  50 20 50 30))
            ((4 5) (values  10 30 30 20))
            ((5) (values  50 35 50 35))
            ((6) (values  50 40 50 50))
            ((7) (values  50 50 57 60))
            ((8) (values  50 50 43 60))
            (t (values  40 45 60 45)))
        (gp:draw-line gp from-x (* yscale from-y) to-x (* yscale to-y))
        (when (= 5 i)(gp:draw-circle gp 50 (* yscale 35) 10))))))
  (gp:clear-graphics-port (hangman-gp man))
  (gp:draw-string (hangman-gp man)
   (format nil "Word=~a  ~a"
      (hangman-display-word man)
      (if (string-equal (hangman-display-word man)
                        (hangman-full-word man))
          (format nil "guessed in ~d" (hangman-wrong-so-far man))
        (if (= (hangman-wrong-so-far man)
               (hangman-wrong-guesses-allowed man))
            (format nil "word was ~a" (hangman-full-word man))
          (format nil "wrong guesses=~d" (hangman-wrong-so-far man)))))
   3 20)
  (dotimes (i (hangman-wrong-so-far man))
    (hangman-line-number (hangman-gp man) i))))

(defun hangman ()
  (let* ((this-hangman (make-new-hangman))
         (callback #'(lambda (x) (hangman-callback this-hangman x)))
         (new-game-callback #'(lambda () (redraw-hangman (setf this-hangman (make-new-hangman this-hangman)))))
         buttons gp toplevel
         (column
          (make-instance 
           'capi:column-layout
           :visible-min-width 350
           :description
           (list
            (setq buttons
                  (make-instance
                   'capi:push-button-panel
                   :selection-callback callback
                   :callback-type :data
                   :layout-args '(:rows 4)
                   :items '("A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K"
                                "L" "M" "N" "O" "P" "Q" "R" "S" "T" "U"
                                "V" "W" "X" "Y" "Z")
                   :layout-class 'capi:grid-layout
                   :background :yellow
                   :foreground :black))
	    (setq gp (make-instance 'capi:output-pane
                                    :font (gp:make-font-description :size 12)
                                    :background :black
                                    :foreground :white
                                    :display-callback #'(lambda (&rest args)
                                                          (declare (ignore args))
                                                          (redraw-hangman this-hangman))
                                    :visible-min-height 150))))))
    (setf (hangman-gp this-hangman) gp
          (hangman-buttons this-hangman) buttons
          toplevel (capi:contain column 
                                 :title "Hangman" 
                                 :menu-bar-items 
                                 (list (make-instance 'capi:menu 
                                                      :title "Game" 
                                                      :items (list (make-instance 'capi:menu-item 
                                                                                  :title "New" 
                                                                                  :callback new-game-callback
                                                                                  :callback-type :none)))))
          (hangman-tl this-hangman) toplevel)
    this-hangman))
