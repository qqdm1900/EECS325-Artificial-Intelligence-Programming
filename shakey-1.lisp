(in-package #:shakey-tests)



;;; Variable naming conventions:
;;;   sloc - shakey-1 location
;;;   bloc - box location
;;;   gloc - goal (destination)


(defparameter *general-planning-kb*
  '(
    (plan-for ?goal ?goal nil)
    
    (<- (plan-for ?state1 ?goal (cons ?action ?actions))
        (action-for ?state1 ?goal ?action)
        (results ?state1 ?state2 ?action)
        (plan-for ?state2 ?goal ?actions))
    ))


(defparameter *room-kb*
  '(
    (-> (all-different (cons ?l3 (cons ?l2 ?l1)))
        (all-different (cons ?l2 ?l1))
        (all-different (cons ?l3 ?l1))
        (different ?l3 ?l2))
    
    (all-different room1 room2 room3 hall)

    (-> (connected ?x ?y) (connected ?y ?x))
    
    (connected room1 hall)
    (connected room2 hall)
    (connected room3 hall)
    )

(defparameter *shakey-1-kb*
  '(
    (results (v1-state ?box-loc-1 ?box-loc-1)
             (v1-state ?box-loc-2 ?box-loc-2)
             (push-box ?box-loc-1 ?box-loc-2))
        
    (results (v1-state ?robot-loc-1 ?box-loc)
             (v1-state ?robot-loc-2 ?box-loc)
             (move-to ?robot-loc-2))
    

    (<- (action-for (v1-state ?box-loc ?box-loc)
                    (v1-state ?robot-loc ?goal-loc)
                    (push-box ?box-loc ?goal-loc))
        (different ?box-loc ?goal-loc)
        (connected ?box-loc ?goal-loc))
   

    (<- (action-for (v1-state ?box-loc ?box-loc)
                    (v1-state ?robot-loc ?goal-loc)
                    (push-box ?box-loc hall))
        (different ?box-loc ?goal-loc)
        (not (connected ?box-loc ?goal-loc)))


    (<- (action-for (v1-state ?robot-loc-1 ?box-loc)
                    (v1-state ?robot-loc-2 ?goal-loc)
                    (move-to ?box-loc))
        (different ?robot-loc-1 ?box-loc)
        (connected ?robot-loc-1 ?box-loc))


    (<- (action-for (v1-state ?robot-loc-1 ?box-loc)
                    (v1-state ?robot-loc-2 ?goal-loc)
                    (move-to hall))
        (different ?robot-loc ?box-loc)
        (not (connected ?robot-loc ?box-loc)))
    )))


7777777777777777777777777777777777777777
(defparameter *general-planning-kb*
  '(
    ;; No action needed if start = goal
    (plan-for ?goal ?goal nil)
    (<- (plan-for ?state1 ?goal (cons ?action ?actions))
        (action-for ?state1 ?goal ?action)
        (results ?state1 ?state2 ?action)
        (plan-for ?state2 ?goal ?actions))
    ))

;;; Variable naming conventions:
;;;   sloc - shakey-1 location
;;;   bloc - box location
;;;   gloc - goal (destination)

(defparameter *shakey-1-kb*
  '(
    ;; ACTION RESULT RULES
   
    ;; push-box changes the location of shakey-1 and the box.
    ;; Shakey-1 has to be at the box.
    (results (v1-state ?bloc-1 ?bloc-1)
             (v1-state ?bloc-2 ?bloc-2)
             (push-box ?bloc-1 ?bloc-2))
        
    ;; move-to changes the location of shakey-1.
    (results (v1-state ?sloc-1 ?bloc)
             (v1-state ?sloc-2 ?bloc)
             (move-to ?sloc-2))
    
    ;; ACTION SELECTION RULES

    ;; push-box to the room through hall if box not in the final destination
    (<- (action-for (v1-state ?bloc ?bloc)
                    (v1-state ?sloc ?gloc)
                    (push-box ?bloc ?gloc))
        (different ?bloc ?gloc)
        (connected ?bloc ?gloc))
   
    ;; push-box to hall if box at a room other than he destination
    (<- (action-for (v1-state ?bloc ?bloc)
                    (v1-state ?sloc ?gloc)
                    (push-box ?bloc hall))
        (different ?bloc ?gloc)
        (not (connected ?bloc ?gloc)))

    ;; move-to another room if not at box location
    ;; shakey at hall
    (<- (action-for (v1-state ?sloc ?bloc);sloc
                    (v1-state ?sloc-2 ?gloc) ; 2 bloc
                    (move-to ?bloc))
        (different ?sloc ?bloc)
        (connected ?sloc ?bloc))

    ;; move-to hall if not at box location 
    ;; shakey at room 
    (<- (action-for (v1-state ?sloc ?bloc)
                    (v1-state ?sloc-2 ?gloc)
                    (move-to hall))
        (different ?sloc ?bloc)
        (not (connected ?sloc ?bloc)))
    ))


;; 4 locations, 3 rooms and 1 hall




