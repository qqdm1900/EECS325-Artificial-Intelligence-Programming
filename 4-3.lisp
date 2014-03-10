(defstruct 3tree data left middle right)


(defun 3tree-clone (3tr)
  (when 3tr
    (make-3tree :data (3tree-data 3tr)
                :left (3tree-clone (3tree-left 3tr))
                :middle (3tree-clone (3tree-middle 3tr))
                :right (3tree-clone (3tree-right 3tr)))))



(defun 3tree-member (obj 3tr)
  (when 3tr
    (or
     (eql obj (3tree-data 3tr))
     (3tree-member obj (3tree-left 3tr))
     (3tree-member obj (3tree-middle 3tr))
     (3tree-member obj (3tree-right 3tr)))))