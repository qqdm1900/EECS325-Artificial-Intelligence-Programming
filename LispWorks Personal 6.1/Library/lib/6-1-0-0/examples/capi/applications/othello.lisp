;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/33/LISPcapi-examples/RCS/applications:othello.lisp,v 1.13.9.1 2011/08/24 13:26:22 davef Exp $" -*-

;;----------------------------------------------------------------------------
;;
;; examples/capi/applications/othello.lisp
;;
;; This example creates a complete othello application.
;;
;; To try it, compile and load this file and then execute:
;;
;;      (CL-USER:PLAY-OTHELLO)
;;
;;----------------------------------------------------------------------------
;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.
;;----------------------------------------------------------------------------



(in-package "CL-USER")

(export '(
	  othello-board
          play-othello
	  ))


;;----------------------------------------------------------------------------
;; Othello square
;;
;; This is a pinboard-object that draws itself as a square with an optional
;; coloured piece upon it.
;;----------------------------------------------------------------------------

(color:define-color-alias :othello-square-background :red)

(defclass othello-square (capi:drawn-pinboard-object)
  ((number :initform nil :initarg :number :accessor othello-square-number)
   (piece :initform nil :accessor othello-square-piece :initarg :piece)
   (background :initform :othello-square-background :initarg :background))
  (:default-initargs
   :visible-min-width 20
   :visible-min-height 20
   :display-callback 'draw-othello-square))

(defmethod draw-othello-square (output-pane (self othello-square) &rest ignore)
  (declare (ignore ignore))
  (capi:with-geometry self
    (with-slots (piece background) self
      (gp:draw-rectangle output-pane
                         capi:%x% capi:%y% capi:%width% capi:%height%
			 :foreground background :filled t)
      (when piece
	(draw-piece output-pane self)))))

(defun draw-piece (output-pane self)
  (capi:with-geometry self
    (with-slots (piece background) self
      (let ((half-width  (floor capi:%width% 2))
            (half-height (floor capi:%height% 2)))
        (gp:draw-ellipse output-pane
                         (+ capi:%x% half-width)
                         (+ capi:%y% half-height)
                         (floor capi:%width%  3)
                         (floor capi:%height% 3)
                         :foreground (or piece background)
                         :filled t)))))

(defmethod (setf othello-square-piece) :after (piece (self othello-square))
  (when-let (output-pane (capi:pinboard-object-pinboard self))
    (draw-othello-square output-pane self)))


;;----------------------------------------------------------------------------
;; Othello game
;;
;; This is an abstract class that represents the current state of the game.
;;----------------------------------------------------------------------------

(defclass othello-game ()
  ((size :initform 8 :initarg :size)
   (player :initform :black)
   (players :initform 1)
   (black-algorithm :initform (default-algorithm))
   (white-algorithm :initform (default-algorithm))
   (squares :initform nil)))

(defun initial-pieces (game)
  (with-slots (size) game
    (let* ((right-coord (floor size 2))
           (left-coord  (1- right-coord)))
      `((,(coordinate-to-square-number left-coord  left-coord  size) :white)
	(,(coordinate-to-square-number right-coord right-coord size) :white)
        (,(coordinate-to-square-number right-coord left-coord  size) :black)
        (,(coordinate-to-square-number left-coord  right-coord size) :black)))))

(defun square-number-to-coordinate (number size)
  (multiple-value-bind
      (row col)
      (floor number size)
    (values col row)))

(defun coordinate-to-square-number (x y size)
  (+ (* size y) x))

(defun name-for-player (game player)
  (declare (ignore game))
  (string-capitalize player))

(defun other-player (player)
  "Choose the other player."
  (if (eq player :black)
      :white
    :black))


;;----------------------------------------------------------------------------
;; Calculate the pieces to be taken for a particular square
;;----------------------------------------------------------------------------

(defun squares-to-take-in-direction (squares player square x-offset y-offset)
  (let ((size (floor (sqrt (length squares)))))
    (multiple-value-bind
	(row col)
	(square-number-to-coordinate square size)
      (let (squares-to-take)
	(loop for x = (+ row x-offset) then (+ x x-offset)
	      for y = (+ col y-offset) then (+ y y-offset)
	      for n = (when (and (>= x 0) (< x size)
				 (>= y 0) (< y size))
			(coordinate-to-square-number x y size))
	      for p = (when (and n square)
			(aref squares n))
	      unless p
	      return nil
	      when (eq p player)
	      return squares-to-take
	      do
	      (push n squares-to-take))))))

(defun integer-sqrt (integer)
  (case integer
    (64 8)
    (t (floor (sqrt integer)))))

(defun squares-to-take-in-direction-p (squares player square x-offset y-offset)
  (let ((size (integer-sqrt (length squares))))
    (multiple-value-bind
	(row col)
	(square-number-to-coordinate square size)
      (let (found-opposing-piece-p)
	(loop for x = (+ row x-offset) then (+ x x-offset)
	      for y = (+ col y-offset) then (+ y y-offset)
	      for n = (when (and (>= x 0) (< x size)
				 (>= y 0) (< y size))
			(coordinate-to-square-number x y size))
	      for p = (when (and n square)
			(aref squares n))
	      unless p
	      return nil
	      when (eq p player)
	      return found-opposing-piece-p
	      do
              (setq found-opposing-piece-p t))))))

(defvar *othello-directions*
    '((-1 . -1) (0  . -1) (1  . -1)
      (-1 .  0)           (1  .  0)
      (-1 .  1) (0  .  1) (1  .  1)))

(defun squares-to-take (square squares player)
  (and (null (aref squares square))
       (when-let (squares-to-take
	          (loop for (x-offset . y-offset) in *othello-directions*
                        nconcing (squares-to-take-in-direction
			          squares player square x-offset y-offset)))
	 (cons square squares-to-take))))

(defun squares-to-take-p (square squares player)
  (and (null (aref squares square))
       (loop for (x-offset . y-offset) in *othello-directions*
	     thereis (squares-to-take-in-direction-p
		      squares player square x-offset y-offset))))


;;----------------------------------------------------------------------------
;; Calculate the best move for a player
;;----------------------------------------------------------------------------

(defvar *algorithms*
    '((:most-pieces "Most Pieces")
      (:most-pieces-aware-of-corners "Most Pieces Aware Of Corners")
      (:least-pieces "Least Pieces")
      (:least-pieces-aware-of-corners "Least Pieces Aware Of Corners")
      (:random "Random")
      (:minimize-opponents-choices "Minimize Opponents Choices")
      (:minimize-opponents-choices-aware-of-corners
       "Minimize Opponents Choices Aware Of Corners")))

(defun default-algorithm ()
  :minimize-opponents-choices-aware-of-corners)

(defun corner-piece-p (piece game)
  "The predicate for whether a square is a corner square."
  (with-slots (size) game
    (or (eq piece 0)
        (eq piece (1- size))
        (eq piece (1- (* size size)))
        (eq piece (- (* size size) size)))))

(defun nearest-corner (piece game)
  "Returns the corner square nearest to the given square." 
  (with-slots (size) game
    (multiple-value-bind
        (x y)
        (square-number-to-coordinate piece size)
      (coordinate-to-square-number (if (< x (floor size 2))
                                       0
                                     (1- size))
                                   (if (< y (floor size 2))
                                       0
                                     (1- size))
                                   size))))

(defun next-to-corner-piece-p (piece game)
  "The predicate for whether a square is next to a corner square."
  (and (not (corner-piece-p piece game))
       (with-slots (size) game
         (multiple-value-bind
             (x y)
             (square-number-to-coordinate piece size)
           (and (or (< x 2)
                    (> x (- size 3)))
                (or (< y 2)
                    (> y (- size 3))))))))

(defun next-to-untaken-corner-piece-p (piece game player squares)
  (and (next-to-corner-piece-p piece game)
       (let ((corner (nearest-corner piece game)))
         (not (eq (svref squares corner) player)))))

(defun all-corner-moves (all-moves game)
  (remove-if-not #'(lambda (piece)
		     (corner-piece-p piece game))
		 all-moves
		 :key 'first))

(defun all-but-next-to-untaken-corner-moves (all-moves game player squares)
  (remove-if #'(lambda (piece)
		 (next-to-untaken-corner-piece-p piece game player squares))
	     all-moves
	     :key 'first))

(defmethod choose-move-for-player ((algorithm (eql :random))
                                   player game squares all-moves)
  (let* ((length (length all-moves)))
    (case length
      (0 nil)
      (t (nth (random length) all-moves)))))

(defmethod choose-move-for-player ((algorithm (eql :most-pieces))
                                   player game squares all-moves)
  (let* ((best-length
          #+:Lucid (let ((sofar -1))
                     (dolist (move all-moves)
                       (when (> (length move) sofar)
                         (setq sofar (length move))))
                     sofar)
          #-:Lucid (loop for move in all-moves
                         maximizing (length move)))
	 (best-moves
	  (remove-if-not #'(lambda (len)
			     (= len best-length))
			 all-moves
			 :key 'length)))
    (choose-move-for-player :random player game squares best-moves)))

(defmethod choose-move-for-player ((algorithm (eql :least-pieces))
                                   player game squares all-moves)
  (let* ((worst-length
          #+:Lucid (let ((sofar 100000))
                     (dolist (move all-moves)
                       (when (< (length move) sofar)
                         (setq sofar (length move))))
                     sofar)
          #-:Lucid (loop for move in all-moves minimizing (length move)))
	 (best-moves
	  (remove-if-not #'(lambda (len)
			     (= len worst-length))
			 all-moves
			 :key 'length)))
    (choose-move-for-player :random player game squares best-moves)))

(defun copy-board (new-squares old-squares)
  (loop for i below (length old-squares)
	do
	(setf (svref new-squares i) (svref old-squares i)))
  new-squares)

(defun perform-move (move player squares)
  (loop for piece in move
	do
	(setf (svref squares piece) player)))

(defmethod choose-move-for-player ((algorithm (eql :minimize-opponents-choices))
                                   player game squares all-moves)
  (let* ((no-of-squares (length squares))
	   (temp-squares (make-array no-of-squares))
           (other-player (other-player player))
           (minimum-choices-for-opponent no-of-squares)
           best-moves)
      (loop for move in all-moves
	    collect
	    (progn
              (copy-board temp-squares squares)
              (perform-move move player temp-squares)
	      (let ((length
		     (number-of-possible-moves-for-player other-player temp-squares)))
		(when (< length minimum-choices-for-opponent)
		  (setq minimum-choices-for-opponent length)
                  (setq best-moves nil))
                (when (= length minimum-choices-for-opponent)
                  (push move best-moves)))))
      (choose-move-for-player :random player game squares best-moves)))

(defun play-algorithm-aware-of-corners (algorithm player game squares all-moves)
  (or (choose-move-for-player algorithm player game squares 
                              (all-corner-moves all-moves game))
      (choose-move-for-player algorithm player game squares
                              (all-but-next-to-untaken-corner-moves all-moves game
                                                                    player squares))
      (choose-move-for-player algorithm player game squares all-moves)))

(defmethod choose-move-for-player ((algorithm (eql :most-pieces-aware-of-corners))
                                   player game squares all-moves)
  (play-algorithm-aware-of-corners :most-pieces player game squares all-moves))

(defmethod choose-move-for-player ((algorithm (eql :least-pieces-aware-of-corners))
                                   player game squares all-moves)
  (play-algorithm-aware-of-corners :least-pieces player game squares all-moves))

(defmethod choose-move-for-player ((algorithm (eql :minimize-opponents-choices-aware-of-corners))
                                   player game squares all-moves)
  (play-algorithm-aware-of-corners :minimize-opponents-choices
                                   player game squares all-moves))
              

;;----------------------------------------------------------------------------
;; Support for different algorithms
;;----------------------------------------------------------------------------

(defun all-possible-moves-for-player (player squares)
  (let ((no-of-squares (length squares)))
    (loop for i below no-of-squares
	  for squares-to-take = (squares-to-take i squares player)
	  when squares-to-take
	  collect squares-to-take)))

(defun number-of-possible-moves-for-player (player squares)
  (loop for i below (length squares)
	counting (squares-to-take-p i squares player)))

(defun game-over-p (game)
  (with-slots (squares) game
    (and (null (all-possible-moves-for-player :white squares))
         (null (all-possible-moves-for-player :black squares)))))

(defun algorithm-for-player (game player)
  "Returns the algorithm for the given player."
  (with-slots (black-algorithm white-algorithm) game
    (case player
      (:white white-algorithm)
      (:black black-algorithm))))

(defun play-move-for-player (game player)
  (with-slots (squares) game
    (when-let (squares-to-take
	       (choose-move-for-player (algorithm-for-player game player)
                                       player game squares
                                       (all-possible-moves-for-player player squares)))
      (dolist (square squares-to-take)
        (setf (aref squares square) player)))))

(defun play-moves-for-player (board game player other-player)
  (with-slots (squares) game
    (loop do
          (progn
	    (play-move-for-player game player)
            (update-othello-board board))
	  until (or (all-possible-moves-for-player other-player squares)
		    (null
		     (all-possible-moves-for-player player squares))))))


;;----------------------------------------------------------------------------
;; Play the piece the player requests
;;----------------------------------------------------------------------------

(defun othello-game-play-square (board game piece)
  (with-slots (squares player players) game
    (let ((squares-to-take
	   (and piece
		(squares-to-take (othello-square-number piece) squares player)))
          (other-player (other-player player)))
      (cond
       (squares-to-take
	(dolist (piece squares-to-take)
	  (setf (aref squares piece) player))
        (update-othello-board board)
        (case players
          (2
           (setf player other-player))
          (t
           (play-moves-for-player board game other-player player)))
        t)))))

(defun score-for-player (game player)
  (with-slots (squares) game
    (let ((score 0))
      (loop for i below (length squares)
	    for piece = (aref squares i)
	    when (eq piece player)
            do (incf score))
      score)))


;;----------------------------------------------------------------------------
;; Othello board
;;
;; Now that we've implemented the game in abstract, we can define the class
;; othello-board which will be the main interface to the game.
;;----------------------------------------------------------------------------

(capi:define-interface othello-board ()
  ((game :initform nil :accessor othello-board-game)
   (board-background :initarg :board-background
                     :initform :othello-square-background)
   (timer :initform nil))
  (:panes
   (buttons
    capi:push-button-panel
    :items '("New Game")
    :callbacks '(othello-board-new-game)
    :callback-type :interface)
   (message-box
    capi:display-pane
    :visible-min-width '(:character 25)))
  (:layouts
   (main-layout
    capi:column-layout
    '(row pinboard)
    :default t)
   (row
    capi:row-layout
    '(buttons message-box))
   (pinboard
    capi:simple-pinboard-layout
    '(grid)
    :visible-border nil
    :input-model '(((:button-1 :release) play-square)))
   (grid
    capi:grid-layout
    ()
    :columns 8))
  (:menu-bar game-menu)
  (:menus
   (game-menu
    "Game"
    (("New" :callback 'othello-board-new-game :callback-type :interface)
     (:component
      (("Black v Computer"    :data :black)
       ("White v Computer"    :data :white)
       ("Computer v Computer" :data :computers)
       ("Two Players"         :data :two-players))
      :callback 'othello-board-set-players
      :callback-type :interface-data
      :selected-item-function #'(lambda (self)
                                  (declare (ignore self))
                                  (case(slot-value game 'players)
                                    (0 :computers)
                                    (1 (slot-value game 'player))
                                    (2 :two-players)))
      :interaction :single-selection)
     black-algorithm-menu
     white-algorithm-menu
     (:component
      (("Size: 8"
        :title-function #'(lambda (self)
                            (declare (ignore self))
                            (with-slots (size) game
                              (format nil "Size: ~D" size)))
        :callback 'othello-board-change-size
        :callback-type :interface)))))
   (black-algorithm-menu
    "Black Algorithm"
    ((:component
      ()
      :items-function #'(lambda (self) (declare (ignore self)) *algorithms*)
      :print-function 'second
      :selected-item-function #'(lambda (self)
                                  (declare (ignore self))
                                  (find (slot-value game 'black-algorithm) *algorithms*
                                        :key 'first))
      :callback-type :data
      :callback #'(lambda (algorithm)
                    (setf (slot-value game 'black-algorithm) (first algorithm)))
      :interaction :single-selection)))
   (white-algorithm-menu
    "White Algorithm"
    ((:component
      ()
      :items-function #'(lambda (self) (declare (ignore self)) *algorithms*)
      :print-function 'second
      :selected-item-function #'(lambda (self)
                                  (declare (ignore self))
                                  (find (slot-value game 'white-algorithm) *algorithms*
                                        :key 'first))
      :callback-type :data
      :callback #'(lambda (algorithm)
                    (setf (slot-value game 'white-algorithm) (first algorithm)))
      :interaction :single-selection))))
  (:default-initargs
   :title "Othello"
   :layout 'main-layout
   :visible-max-width t
   :visible-max-height t
   :destroy-callback 'othello-board-destroyed))

(defun play-square (pinboard x y)
  (let* ((board (capi:element-interface pinboard))
         (square (capi:pinboard-object-at-position pinboard x y))
         (game (othello-board-game board)))
    (unless (othello-game-play-square board game square)
      (capi:beep-pane pinboard))
    (cond
     ((game-over-p game)
      (display-final-score board))
     (t
      (display-current-score board)))))

(defmethod initialize-instance :after ((self othello-board)
                                       &key size &allow-other-keys)
  (othello-board-new-size self (or size 8)))

(defun othello-board-new-size (self size)
  (with-slots (grid pinboard game) self
    (setf game (make-instance 'othello-game :size size))
    (setf (capi:layout-description pinboard)
          (list (setf grid (make-instance 'capi:grid-layout :columns size)))))
  (make-othello-squares self))

(defun othello-board-change-size (self)
  (with-slots (game) self
    (with-slots (size) game
      (when-let (new-size 
                 (capi:prompt-for-integer "Enter size"
                                          :initial-value size
                                          :min 4
                                          :max 20
                                          :ok-check 'evenp))
        (setf size new-size)
        (othello-board-new-size self new-size)))))

(defun make-othello-squares (self)
  (with-slots (game grid board-background pinboard) self
    (with-slots (size squares) game
      (let* ((no-of-squares (* size size)))
        (setf squares (make-array no-of-squares))
        (setf (capi:layout-description grid)
              (loop for i below no-of-squares
                    collect (make-instance 'othello-square
                                           :number i
                                           :background board-background
				           :visible-min-width  40
				           :visible-min-height 40))))
      (gp:invalidate-rectangle pinboard)
      (othello-board-new-game self))))

(defun othello-board-destroyed (self)
  (remove-play-timer self))

(defun update-othello-board (self &optional squares)
  (with-slots (grid game) self
    (loop for piece
          #+:Lucid in
          #+:Lucid (coerce (or squares (slot-value game 'squares)) 'list)
          #-:Lucid across
          #-:Lucid (or squares (slot-value game 'squares))
          for pinboard-object in (capi:layout-description grid)
          unless (eq (othello-square-piece pinboard-object) piece)
          do
          (setf (othello-square-piece pinboard-object) piece))))

(defun othello-board-new-game (self)
  (remove-play-timer self)
  (with-slots (game) self
    (with-slots (size squares player players) game
      (let ((initial-pieces (initial-pieces game)))
        (loop for piece below (* size size)
	      do
              (setf (aref squares piece)
                    (second (assoc piece initial-pieces))))
        (when (eq players 2)
          (setf player :black))
        (update-othello-board self)
        (start-game self)))))

(defun othello-board-message (self message &rest format-args)
  (with-slots (message-box) self
    (setf (capi:display-pane-text message-box)
          (apply 'format nil message format-args))))

(defun othello-board-set-players (board type)
  (with-slots (game) board
    (with-slots (player players) game
      (let ((game-over-p (game-over-p game)))
        (case type
	  ((:white :black)
	   (unless (and (eq players 2)
                        (eq player type))
	     (setf players 1)
	     (setf player type)
	     (play-moves-for-player board game (other-player type) type)))
	  (:computers
	   (setf players 0)
           (if game-over-p
               (othello-board-new-game board)
	     (play-both-players board))
           (setf game-over-p nil))
	  (:two-players
	   (setf players 2)))
        (when game-over-p
	  (othello-board-new-game board))))))
     
(defvar *play-delay* 0.5)

(defun remove-play-timer (board)
  (with-slots (timer) board
    (when timer
      (mp:unschedule-timer (prog1 timer (setf timer nil))))))

(defun play-both-players (board)
  (with-slots (game timer) board
    (with-slots (player) game
      (unless timer
        (let ((current-player player)
              (other-player (other-player player)))
            
          (flet ((play-once ()
                   (when timer
                     (if (game-over-p game)
                         (progn
                           (display-final-score board)
                           (remove-play-timer board))
                       (progn
                         (play-moves-for-player board game current-player other-player)
                         (rotatef current-player other-player)
                         (mp:schedule-timer-relative timer *play-delay*))))))
            (setf timer
                  (mp:make-timer 'capi:execute-with-interface
                                 board #'play-once))
            (play-once)))))))

(defun start-game (board)
  (with-slots (game) board
    (with-slots (player players) game
      (case players
        (0
         (play-both-players board))
        (1
         (if (eq player :white)
	     (play-moves-for-player board game :black :white)
           (display-current-score board)))
        (t
         (display-current-score board))))))

(defun display-current-score (board)
  (let ((game (othello-board-game board)))
    (othello-board-message
     board
     "~A: ~S, ~A: ~S"
     (name-for-player  game :white)
     (score-for-player game :white)
     (name-for-player  game :black)
     (score-for-player game :black))))

(defun display-final-score (board)
  (with-slots (game) board
    (let* ((white-score (score-for-player game :white))
           (black-score (score-for-player game :black)))
      (cond
       ((eq white-score black-score)
        (othello-board-message
         board
         "Game drawn: ~A pieces each"
         white-score))
       (t
        (let ((winner (if (> white-score black-score) :white :black)))
          (othello-board-message
           board
           "~A has won: ~A to ~A"
           (name-for-player game winner)
           (score-for-player game winner)
           (score-for-player game (other-player winner)))))))))

(defun play-othello ()
  "The entry point."
  (capi:display (make-instance 'othello-board)))
