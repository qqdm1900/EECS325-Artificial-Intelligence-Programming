#|
Intersect segments uses the beginning and end of each
vector to determine parallelism (via cross product), disjoint
segments (via cross product), or intersecting segments.
|#
(defun intersect-segments (x1 y1 x2 y2 x3 y3 x4 y4)
  (let* ((base-point (sub-vectors (list x1 y1) (list x3 y3)))
         (endpoint1 (sub-vectors (list x1 y1) (list x2 y2)))
         (endpoint2 (sub-vectors (list x3 y3) (list x4 y4)))
         (bottom-cross (cross-point endpoint1 endpoint2)))
    (cond ((= bottom-cross 0)
           (parallel-or-disjoint x1 y1 x2 y2 x3 y3 x4 y4))
          (t (intersecting x1 x2 x3 x4 y1 base-point endpoint1 endpoint2)))))

#|
See if two line segments are parallel or disjoint by
using the cross product formula between the two segment
vectors. The appropriate overlap is returned depending
on parallelism of lines or points, and nil is returned
if the lines are disjoint.
|#
(defun parallel-or-disjoint (x1 y1 x2 y2 x3 y3 x4 y4)
  (let ((result (multiple-value-call #'parallel-span (sort-points x1 y1 x2 y2 x3 y3 x4 y4))))
    (cond ((null result) nil)
          ((eql (slope x1 y1 x2 y2) 'point)
           (if (eql (slope x3 y3 x4 y4) 'point)
               (values-list result)
             (values (car result) (cadr result) (car result) (cadr result))))
          (t (values-list result)))))

#|
See if two line segments intersect at a single point, after
parallel and disjoint segments have been ruled out. The y-values
are not needed, since the check only performs on the x-values. Y1
is needed to calculate the scalar multiple of the vector, so the
intersection point can be found.
|#
(defun intersecting (x1 x2 x3 x4 y1 base-point endpoint1 endpoint2)
  (let ((result (add-vectors
                 (scalar-multiply (/ (cross-point base-point endpoint2)
                                     (cross-point endpoint1 endpoint2))
                                  endpoint1) (list x1 y1))))
    (if (valid-point (car result) x1 x2 x3 x4)
        (values-list result)
      nil)))

#|
Check to see if a given point lies within the original segments.
This is necessary to resolve conflicts with the cross product
detecting intersections beyond the domain of the input segments.
|#
(defun valid-point (x x1 x2 x3 x4)
  (not (or (and (< x x1) (< x x2))
           (and (> x x1) (> x x2))
           (and (< x x3) (< x x4))
           (and (> x x3) (> x x4)))))

#|
Sort-points rotates the input points around so that only
a fraction of the number of rotations need to be checked
by parallel span. Without sorting, there are double the
options per span, resulting in 2*2^3 =  16 unique rotations,
rather than the smaller (and more easily checked) 2 rotations.
|#
(defun sort-points (x1 y1 x2 y2 x3 y3 x4 y4)
  (cond ((and (<= x1 x2) (<= x3 x4))
         (values x1 y1 x2 y2 x3 y3 x4 y4))
        ((<= x1 x2)
         (values x1 y1 x2 y2 x4 y4 x3 y3))
        ((<= x2 x4)
         (values x2 y2 x1 y1 x3 y3 x4 y4))
        (t (values x2 y2 x1 y1 x4 y4 x3 y3))))

#|
Parallel span checks the sorted points in two spans
and determines if the spans are parallel with one another.
Because of the sorting, this can only happen if there is
and intersect between two of the points on the line, which
is covered by just two cases.
|#
(defun parallel-span (x1 y1 x2 y2 x3 y3 x4 y4)
  (cond ((or (and (>= x2 x3) (<= x2 x4))
             (and (<= x2 x3) (>= x2 x4)))
         (list x2 y2 x3 y3))
        (t nil)))

#|
Get the cross product of two vectors. Using a cross product,
the intersection of two lines in 2 space can be found, solving
this problem. If the cross product is 0, the lines are either
parallel, or disjoint.
|#
(defun cross-point (l1 l2)
  (- (* (car l1) (cadr l2)) (* (car l2) (cadr l1))))

#|
Multiply a vector by a scalar.
|#
(defun scalar-multiply (x l)
  (list (* (car l) x) (* (cadr l) x)))

#|
Add one vector to another.
|#
(defun add-vectors (v1 v2)
  (list (+ (car v1) (car v2)) (+ (cadr v1) (cadr v2))))

#|
Subtract one vector from another.
|#
(defun sub-vectors (v1 v2)
  (list (- (car v2) (car v1)) (- (cadr v2) (cadr v1))))

#|
Find the slope of a line between two points. If the
line is a single point, return point. If the slope is
undefined, return undefined. Otherwise, return a number.
|#
(defun slope (x1 y1 x2 y2)
  (cond ((and (eql x1 x2)
              (eql y1 y2))
         'point)
        ((eql x2 x1)
         'undefined)
        ((eql y2 y1)
         0)
        (t (/ (- y2 y1) (- x2 x1)))))