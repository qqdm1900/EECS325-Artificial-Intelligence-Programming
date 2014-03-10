;; -*- Mode: Lisp; rcs-header: "$Header: /hope/lwhope1-cam/hope.0/compound/58/KWdemos/RCS/whist:tool.lisp,v 1.4.1.1 2011/08/24 13:25:19 davef Exp $" -*-

;; Copyright (c) 1987--2012 LispWorks Ltd. All rights reserved.


(in-package "WHIST")

(defun run-whist nil
  (capi:find-interface 'whist-tool))

(capi:define-interface whist-tool ()
  ()
  (:panes
   (player-item capi:display-pane
                :title "Player: "
                :max-width nil)
   (trumps-item capi:display-pane
                :title "Trumps: "
                :max-width nil)
   (round-item capi:display-pane
               :title "Round: "
               :max-width nil)
   (score-item capi:display-pane
               :title "Score: "
               :max-width nil)
   (played-pane capi:display-pane
                :min-width '(character 20)
                :max-width nil
                :max-height nil)
   (message-pane capi:display-pane
                 :min-width '(character 10)
                 :max-width nil
                 :max-height '(character 1)))
  (:layouts
   (hand-grid capi:grid-layout
              nil
              :rows 4
              :columns 11
              :min-width '(character 40)
              :min-height '(character 5)
              :max-width t
              :max-height t)
   (hand-menu capi:simple-pinboard-layout
              '(hand-grid)
              :input-model '(((:button-1 :press) play-card-callback))
              :background :pink)
   (top-grid capi:grid-layout
          '(player-item trumps-item round-item score-item)
          :columns 2)
   (row-3 capi:row-layout
          '(hand-menu played-pane))
   (main-layout capi:column-layout
                '(top-grid row-3 message-pane)
                :default t))
  (:menus
   (game-menu "Game"
              (("Restart" :callback 'reset-command
                          :callback-type :interface))))
  (:menu-bar game-menu)
  (:default-initargs
   :message-area t
   :title "Whist"))

(defmethod initialize-instance :after ((self whist-tool) &rest args)
  (setq *pack* nil)
  (reset))


(defun reset-command (top-level-window)
  (with-slots (hand-menu message-pane played-pane trumps-item) top-level-window
    (display-message played-pane " ")
    (display-message message-pane "Initializing...")
    (unless *pack* (init-stuff))
    (reset-stuff)
    (display-message message-pane "Shuffling...")
    (shuffle-pack)
    (display-message message-pane "Dealing...")
    (deal-cards)
    (setf (capi:display-pane-text trumps-item)
          (format nil "~:(~A~)" (suit-name *trumps*)))
    (display-message message-pane "Assessing...")
    (infer :contexts '(assess))
    (display-round (current-round *state*) top-level-window)
    (display-scores *players* top-level-window)
    (play-card :start-game top-level-window)))

(defun play-card-callback (pinboard x y)
  (when-let (item (capi:pinboard-object-at-position pinboard x y))
    (let ((data (capi:item-data item)))
      (unless (stringp data) ; user clicked on the a name of sit
        (play-card data
                   (capi:top-level-interface pinboard))))))

(defun play-card (card top-level-window)
  (with-slots (message-pane hand-grid hand-menu) top-level-window
    (ecase (manual-play-card card top-level-window)
      (:illegal-card
       (display-message message-pane "Illegal card!"))
      (:game-over
       (setf (capi:layout-description hand-grid) nil)
       (display-scores *players* top-level-window t))
      (:next-card
       (display-message message-pane "Choose a card...")))))

(defun display-player (player top-level-window)
  (with-slots (player-item) top-level-window
    (display-message player-item (princ-to-string (player-name player)))))

(defun display-played (played top-level-window)
  (with-slots (played-pane) top-level-window
    (display-message played-pane "~{~%~A~}" (reverse played))))

(defun display-round (round top-level-window)
  (with-slots (round-item) top-level-window
    (display-message round-item (princ-to-string round))))

(defun display-scores (players top-level-window &optional finalp)
  (with-slots (score-item message-pane) top-level-window
    (let ((string (format nil "~{~:(~R~)~^ / ~}" (mapcar #'player-score players))))
      (display-message score-item string)
      (when finalp (display-message message-pane
                                    (concatenate 'string "Final scores: " string)))
      string)))

(defun display-hand (player top-level-window)
  (with-slots (hand-grid) top-level-window
    (let ((grid-array (make-array '(4 14)))
          (cards (player-cards player)))
      (loop for row from 0
	    for suit in *suits*
            do
            (setf (aref grid-array row 0)
                   (make-instance 'capi:item-pinboard-object
                                       :data  (format nil "~:(~A~):" (suit-name suit))
                                       ))

	    (loop for column from 1
                  for card in (lookup suit cards)
                  do
                  (setf (aref grid-array row column)
                        (make-instance 'capi:item-pinboard-object
                                       :data card
                                       :print-function 'card-name))))
      (setf (capi:layout-description hand-grid)
            grid-array
            )
      )))

(defun display-thinking (top-level-window)
  (with-slots (message-pane) top-level-window
    (display-message message-pane "Thinking...")))

(defun display-message (contact string &rest args)
  (let ((text (apply #'format nil string args)))
    (if (typep contact 'capi:display-pane)
        (setf (capi:display-pane-text contact) text)
      (setf (capi:display-pane-text contact) text))))

