(defparameter *general-planning-kb*
  '(
    (plan-for (v2-state ?sloc-1 room1 ?unlocked-1)
              (v2-state ?sloc-2 room1 ?unlocked-2) 
              nil)
    (<- (plan-for ?state1 ?goal (cons ?action ?actions))
        (action-for ?state1 ?state2 ?action)
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
   ))

(defparameter *member-kb*
  '((member ?x (cons ?x ?l1))
    (<- (member ?x (cons ?l3 (cons ?l1 ?l2))) (member ?x (cons ?l1 ?l2)))))



(defparameter *shakey-2-kb*
  '(
    (results (v2-state ?bloc-1 ?bloc-1 ?unlocked)
             (v2-state ?bloc-2 ?bloc-2 ?unlocked)
             (push-box ?bloc-1 ?bloc-2))

    (results (v2-state ?sloc-1 ?bloc ?unlocked)
             (v2-state ?sloc-2 ?bloc ?unlocked)
             (move-to ?sloc-2))

    (results (v2-state ?sloc-1 ?bloc ?unlocked)
             (v2-state ?sloc-1 ?bloc (cons ?room ?unlocked))
             (unlock ?room))
    
    (<- (action-for (v2-state hall ?bloc ?unlocked)
                    (v2-state hall ?bloc 
                              (cons ?bloc ?unlocked))
                    (unlock ?bloc))
        (different ?bloc hall)
        (not (member ?bloc ?unlocked)))

    (<- (action-for (v2-state hall hall ?unlocked)
                    (v2-state hall hall 
                              (cons room1 ?unlocked))
                    (unlock room1))
        (not (member room1 ?unlocked)))

    (<- (action-for (v2-state ?sloc ?bloc ?unlocked)
                    (v2-state hall ?bloc ?unlocked)
                    (move-to hall))
        (member ?sloc ?unlocked)
        (all-different (cons ?sloc (cons ?bloc (cons hall nil)))))


    (<- (action-for (v2-state hall ?bloc ?unlocked)
                    (v2-state ?bloc ?bloc ?unlocked)
                    (move-to ?bloc))
        (member ?bloc ?unlocked))
    

    (<- (action-for (v2-state hall hall ?unlocked)
                    (v2-state ?gloc ?gloc ?unlocked) 
                    (push-box hall ?gloc))
        (member ?gloc ?unlocked))

    (<- (action-for (v2-state ?bloc ?bloc ?unlocked)
                    (v2-state hall hall ?unlocked)
                    (push-box ?bloc hall))
        (member ?bloc ?unlocked)
        (all-different (cons ?sloc (cons ?bloc (cons hall nil)))))
))