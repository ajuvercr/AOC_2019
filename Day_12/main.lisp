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

(defun print-input (ls)
    (format t "Location ~A~ASpeed ~A~%" (first (first ls)) #\tab (second (first ls)))
    (if (second ls)
        (cons (first ls) (print-input (rest ls)))
        ls))

(defun get-input ()
    '(
        ((-6 2 -9) (0 0 0 ))
        ((12 -14 -4) (0 0 0))
        ((9 5 -6) (0 0 0))
        ((-1 -4 9) (0 0 0))))

(defun get-test-input ()
    '(
        ((-8 -10 0) (0 0 0))
        ((5 5 10) (0 0 0))
        ((2 -7 3) (0 0 0))
        ((9 -8 -3) (0 0 0))
    ))

(defun get-other-test-input ()
    '(
        ((-1 0 2) (0 0 0))
        ((2 -10 -7) (0 0 0))
        ((4 -8 8) (0 0 0))
        ((3 5 -1) (0 0 0))
    ))

(defun delta (p1 p2)
    (if (= p1 p2)
        0
        (if (< p1 p2)
            1
            -1)))

(defun do-gravity (moon1 moon2)
    (list (first moon1) (apply #'mapcar #'+ (list (second moon1) (apply #'mapcar #'delta (list (first moon1) (first moon2)))))))

(defun do-gravities (input)
    (mapcar #'(lambda (x) (reduce #'do-gravity (second x) :initial-value (first x))) (r-all input)))

(defun do-velocities (input)
    (mapcar #'(lambda (x) (list (apply #'mapcar #'+ x) (second x))) input))

(defun do-step (input)
    (do-velocities (do-gravities input)))

(defun calc-energy (input)
    (if input
        (+ (apply #'* (mapcar #'(lambda (x) (reduce #'+ (mapcar #'abs x))) (first input))) (calc-energy (rest input)))
        0))

(defun do-n-times (times at f input)
    (if (< at times)
        (do-n-times times (+ at 1) f (funcall f input))
        input))

(defun real-member (x ls)
    (if ls
        (if (equal x (first ls))
            T
            (real-member x (rest ls)))
        nil))

(defun try-match (seen current at)
    (if (= (second (multiple-value-list (floor at 1000))) 0)
        (format t "At ~A~%" at)
        ())
    (let ((new-input (do-step current)))
        (if (real-member new-input seen)
            at
            (try-match (cons new-input seen) new-input (+ at 1)))))

(format t "Energy: ~A~%" (calc-energy (do-n-times 1000 0 #'do-step (get-other-test-input))))
(format t "After ~A~% tries" (try-match nil (get-test-input) 0))
