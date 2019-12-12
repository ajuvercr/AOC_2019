#!/usr/bin/sbcl --script

(defun range (max &key (min 0) (step 1))
   (loop for n from min below max by step
      collect n))

(defun r-nth (l i)
    (if (= i 0)
        (list (first l) (rest l))
        (let ((nl (r-nth (rest l) (- i 1))))
            (list (first nl) (cons (first l) (second nl))))))

(defun r-all (l)
    (loop for x in (range (list-length l))
        collect (r-nth l x)))

(defun get-input ()
    '(
        ((-6 12 9 -1) (0 0 0 0))
        ((2 -14 5 -4) (0 0 0 0))
        ((-9 -4 -6 9) (0 0 0 0))
    ))

(defun get-test-input ()
    '(
        ((-1 2 4 3) (0 0 0 0))
        ((0 -10 -8 5) (0 0 0 0))
        ((2 -7 8 -1) (0 0 0 0))
    ))

(defun delta (p1 p2)
    (if (= p1 p2)
        0
        (if (< p1 p2)
            1
            -1)))

(defun get-deltas (input)
    (destructuring-bind (self others) input
        (reduce #'+ (mapcar #'(lambda (x) (delta self x)) others))))

(defun dir-step (input)
    (destructuring-bind (positions velocities) input
        (let ((new-velocities (apply #'mapcar #'+ (list velocities (mapcar #'get-deltas (r-all positions))))))
            (list (apply #'mapcar #'+ (list positions new-velocities)) new-velocities))))

(defun do-while (at test input)
    (let ((new-input (dir-step input)))
        (if (equal new-input test)
            (values (+ at 1) new-input)
            (do-while (+ at 1) test new-input))))

(defun find-match (current)
    (do-while 0 current current))

(format t "~A~%" (apply #'lcm (mapcar #'find-match (get-input))))
