(defparameter *member-kb*
  '((member ?x (list ?x))
    (<- (member ?x (list* ?l3 ?l1 ?l2)) (member ?x (list* ?l1 ?l2)))
    (<- (member ?x (list* ?x ?l1 ?l2)) (member ?x (list ?x)))))


(in-package :ddr-tests)
(defparameter *member-kb*
  '((member ?x (cons ?x nil))
    (<- (member ?x (cons ?l3 (cons ?l1 ?l2))) (member ?x (cons ?l1 ?l2))) 
    (<- (member ?x (cons ?x (cons ?l1 ?l2))) (member ?x (cons ?x nil)))))

;;;only 2 needed
(defparameter *member-kb*
  '((member ?x (cons ?x ?l1))
    (<- (member ?x (cons ?l3 (cons ?l1 ?l2))) (member ?x (cons ?l1 ?l2)))))