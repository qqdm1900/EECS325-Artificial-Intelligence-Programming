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
    (-> (different ?x ?y) (different ?y ?x))


    (all-different (cons room1 (cons room2  (cons room3 (cons hall nil)))))


    (-> (connected ?x ?y) (connected ?y ?x))

    (connected room1 hall)
    (connected room2 hall)
    (connected room3 hall)
   ))

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

    (<- (action-for (v1-state ?robot-loc ?box-loc)
                    (v1-state ?robot-loc-2 ?goal-loc)
                    (move-to ?box-loc))
        (different ?robot-loc ?box-loc)
        (connected ?robot-loc ?box-loc))

    (<- (action-for (v1-state ?robot-loc ?box-loc)
                    (v1-state ?robot-loc-2 ?goal-loc)
                    (move-to hall))
        (different ?robot-loc ?box-loc)
        (not (connected ?robot-loc ?box-loc)))
    ))



