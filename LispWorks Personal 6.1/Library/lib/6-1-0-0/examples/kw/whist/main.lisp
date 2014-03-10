;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/58/KWdemos/RCS/whist:main.lisp,v 1.1.13.1 2011/08/24 13:25:20 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.

#| WHIST
The following is a very rudimentary 13-card 2-player
whist playing game.

Type (whist:run-whist) to start the game.
Moves are entered by pointing at the required card:

The replies are completely algorithmic. The inference engine is
invoked only when a lead is required.

Updating an Inference Tracer window after every lead will give
some insight into the process of selection.
|#

(in-package "WHIST")

(def-kb-class player ()
  ((name :initarg :name :reader player-name)
   (kind :initarg :kind :reader player-kind)
   (cards :initarg :cards :accessor player-cards)
   (lengths :initarg :lengths :accessor player-lengths)
   (score :initarg :score :accessor player-score)
   (short-suit :initarg :short-suit :accessor player-short-suit)
   (trumps :initarg :trumps :accessor player-trumps)))

(def-kb-class state ()
  ((player :initarg :player :accessor current-player)
   (round :initarg :round :accessor current-round)))

(def-kb-class suit ()
  ((name :initarg :name :reader suit-name)
   (trumps  :writer (setf suit-trumps))
   (trumped :writer (setf suit-trumped))))

(def-kb-class card ()
  ((suit :initarg :suit :accessor card-suit)
   (number :initarg :number :accessor card-number)
   (player :initarg :player :initform nil :accessor card-player)
   (score :initarg :score :initform nil :accessor card-score)
   (played :initarg :played :writer (setf card-played))))

(defconstant *number-of-cards* 52)

(defconstant *number-in-suit* 13)

(defconstant *number-in-hand* 13)

(defconstant *numbers* (loop for i from 2 upto (1+ *number-in-suit*) collect i))

(defvar *suits* nil)

(defvar *pack* nil)

(defvar *played* nil)

(defvar *trumps* nil)

(defvar *players* nil)
(defvar *player-loop* nil)
(defvar *player-count* 0)
(defvar *number-of-players* 2)

(defvar *state* nil) ; for communication with inference engine

(defmacro lookup (key table)
  `(cdr (assoc ,key ,table :test 'eq)))

(defun make-suit-table (&optional initial-value)
  (loop for suit in *suits*
        collect (cons suit initial-value)))

(defun init-stuff ()
  (setf *state* (make-instance 'state :player nil :round 1))
  (setf *players* (cons (make-instance 'player :kind 'manual :name "you")
		        (loop for id from 1 to (1- *number-of-players*) collect
			      (make-instance 'player
                                             :kind 'automatic
					     :name (if (= id 1)
                                                       "me"
                                                     (concatenate 'string
							          "me-"
							          (princ-to-string id)))))))
  (setf *player-loop* (let ((players (copy-list *players*))) (setf (cdr (last players)) players)))
  (setf *suits* (list (make-instance 'suit :name 'hearts)
                      (make-instance 'suit :name 'clubs)
                      (make-instance 'suit :name 'diamonds)
	              (make-instance 'suit :name 'spades)))
  (setf *pack* (make-array *number-of-cards*))
  (loop with i = -1
	for suit in *suits* do
	(loop for number of-type fixnum in *numbers* do
	      (setf (svref *pack* (incf i))
		    (make-instance 'card
				   :suit suit
				   :number number)))))

(defun reset-stuff ()
  (setf *played* nil
        (current-player *state*) nil
        (current-round *state*) 1)
  (loop for p in *players* do
        (setf (player-score p) 0
              (player-lengths p) (make-suit-table 0)
              (player-cards p) (make-suit-table)
              (player-short-suit p) nil
	      (player-trumps p) nil))
  (loop for s in *suits* do
        (setf (suit-trumps s) nil
              (suit-trumped s) nil))
  (loop for c across *pack* do
        (setf (card-score c) nil
              (card-played c) nil
              (card-player c) nil)))


(defun shuffle-pack ()
  (loop for i of-type fixnum from 0 to (1- *number-of-cards*)
        as j = (+ i (random (- *number-of-cards* i)))
	do (rotatef (svref *pack* i) (svref *pack* j))))

(defun deal-cards ()
  (loop for p in *players*
        as start upfrom 0 by *number-in-hand*
	do (loop for index from start to (+ start (1- *number-in-hand*))
		 as card = (svref *pack* index)
		 as suit = (card-suit card)
		 do (setf (card-player card) p)
		 do (push card (lookup suit (player-cards p)))
		 do (incf (lookup suit (player-lengths p)))))
  (loop for p in *players* do
        (loop for suit in *suits* do
              (setf (lookup suit (player-cards p))
                    (sort (lookup suit (player-cards p)) #'> :key #'card-number))))
  (setf *trumps* (card-suit (svref *pack* (random *number-of-cards*))))
  (loop for s in *suits*
        do (setf (suit-trumps s) (eq *trumps* s))))

(defun manual-play-card (card top-level-window)
  (if (eq card :start-game)
      (update-players :start)
    (progn
      (unless (legal-play card) (return-from manual-play-card :illegal-card))
      (update-card-info card)
      (display-played *played* top-level-window)))
  (loop do (when (= *player-count* *number-of-players*) ; when end of trick
	     (let* ((winner (card-player (winning-card))))
	       (update-players :trick winner)
	       (incf (player-score winner))
	       (display-scores *players* top-level-window)
	       (setf *played* nil)
	       (case (current-round *state*)
		 (13 (return-from manual-play-card :game-over))
		 (t  (display-round (incf (current-round *state*)) top-level-window)))))
        do (let ((player (current-player *state*)))
             (case (player-kind player)
               (automatic (display-thinking top-level-window)
			  (automatic-play-card player)
			  (display-played *played* top-level-window))
               (manual    (display-player player top-level-window)
			  (display-hand player top-level-window)
			  (return-from manual-play-card :next-card))))))

(defun update-players (flag &optional winner)
  (setf *player-loop* (case flag
                        (:start (nthcdr (random *number-of-players*) *player-loop*))
                        (:trick (member winner *player-loop* :test #'eq))
                        (:card (rest *player-loop*)))
	*player-count* (case flag
                         ((:start :trick) 0)
                         (:card (1+ *player-count*)))
	(current-player *state*) (first *player-loop*)))

(defun legal-play (card)
  (or (null *played*)
      (let ((follow-suit (card-suit (first (last *played*)))))
        (or (eq (card-suit card) follow-suit)
            (= 0 (lookup follow-suit (player-lengths (card-player card))))))))

(defun update-card-info (card)
  (update-players :card)
  (let* ((suit (card-suit card))
	 (player (card-player card))
         (follow-card (first (last *played*)))
         (follow-suit (and follow-card (card-suit follow-card))))
    (decf (lookup suit (player-lengths player)))
    (setf (lookup suit (player-cards player))
          (delete card (lookup suit (player-cards player)) :count 1 :test 'eq))
    (when (and follow-card (eq suit *trumps*) (not (eq follow-suit *trumps*)))
      (setf (suit-trumped suit) t))
    (setf (card-played card) t)
    (push card *played*)))

(defun update-trick-info ()
)

(defun winning-card ()
  (loop with winning-card
        for card in (nreverse *played*) do
	(when (or (not winning-card)
		  (and (eq (card-suit card)
			   (card-suit winning-card))
		       (> (card-number card)
			  (card-number winning-card)))
		  (and (not (eq (card-suit winning-card) *trumps*))
                       (eq (card-suit card) *trumps*)))
	  (setf winning-card card))
        finally (return winning-card)))

(defun automatic-play-card (player)
  (let ((card (if *played*
                  (automatic-get-reply player)
                (automatic-get-lead player))))
    (update-card-info card)
    card))

(defun automatic-get-reply (player)
  (print-info player)
  (let* ((follow-card (first (last *played*)))
         (follow-suit (card-suit follow-card))
         (winning-card (winning-card))
         (winning-suit (card-suit winning-card))
         (winning-number (card-number winning-card))
         (empty-follow-suit (= 0 (lookup follow-suit (player-lengths player)))))
    (loop with best-card = nil
          and best-score = -999
          for suit in *suits*
          do (loop for card in (lookup suit (player-cards player))
 	           as number = (card-number card)
	           as score = (cond
		               (empty-follow-suit (if (and (eq suit *trumps*)
                                                           (or (and (eq winning-suit *trumps*)
                                                                    (> number winning-number))
                                                               (not (eq winning-suit *trumps*))))
                                                      (- 15 number)
					            (- number)))
		               ((eq suit follow-suit) (if (> number winning-number)
				                          (- 15 number)
				                        (- number)))
		               (t -999))
	           when (> score best-score) do (setq best-score score
                                                      best-card card))
	  finally (return best-card))))

(defun automatic-get-lead (player)
  (print-info player)
  (loop for suit in *suits*
        do (loop for card in (lookup suit (player-cards player))
                 do (setf (card-score card) 0)))
  (infer :contexts '(lead))
  (loop with best-card = nil
	and best-score = -999
	for suit in *suits*
	do (loop for card in (lookup suit (player-cards player))
		 as score = (card-score card)
		 when (> score best-score)
                 do (setq best-score score best-card card))
	finally (return (or best-card (error "inference failure - no card chosen.")))))

;; Utilities

(defun print-info (player)
  (format t "~2%Round: ~D, Played: ~{~A, ~}"
	  (current-round *state*) *played*)
  (format t "~%Player Count: ~A, Current Player: ~A"
	  *player-count* (player-name player))
  (loop for suit in *suits*
	do (format t "~%~10A: ~{~A ~}"
		   (suit-name suit)
		   (mapcar #'card-name
			   (sort (copy-list (lookup suit (player-cards player)))
				 #'>
				 :key #'card-number))))
  (values))


(defun card-name (card)
  (let ((n (card-number card)))
    (case n
      (14 "A")
      (13 "K")
      (12 "Q")
      (11 "J")
      (t (princ-to-string n)))))



(defmethod print-object ((self card) stream)
    (format stream "~A of ~:(~A~)" 
            (card-name self)
            (suit-name (card-suit self))))

(defmethod print-object ((self suit) stream)
  (print-unreadable-object (self stream)
    (format stream "SUIT ~S" 
            (suit-name self))))

