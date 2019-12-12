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
        ((-2 12 9 -1) (0 0 0 0))
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

(defun real-member (x ls)
    (if ls
        (if (equal x (first ls))
            T
            (real-member x (rest ls)))
        nil))

(defparameter *equals* nil)
(defun print-it (new at)
    (setf *equals* (cons at *equals*))
    new)

(defun try-match (seen current at show)
    (if (< 0 show)
        (let ((new-input (dir-step current)))
            (if (real-member new-input seen)
                (try-match nil (print-it new-input at) (+ at 1) (- show 1))
                (try-match (cons new-input seen) new-input (+ at 1) show))))
        ())

(setf *equals* nil)
(try-match nil (nth 0 (get-input)) 0 2)
(format t "~A~%" *equals*)
;; (format t "Offset ~A~AFactor: ~A~%" (second *equals*) #\tab (- (first *equals*) (second *equals*)))

(setf *equals* nil)
(try-match nil (nth 1 (get-input)) 0 2)
(format t "~A~%" *equals*)
;; (format t "Offset ~A~AFactor: ~A~%" (second *equals*) #\tab (- (first *equals*) (second *equals*)))

(setf *equals* nil)
(try-match nil (nth 2 (get-input)) 0 2)
(format t "~A~%" *equals*)

;; (format t "Offset ~A~AFactor: ~A~%" (second *equals*) #\tab (- (first *equals*) (second *equals*)))

;; n * 19 = 10 + m * 29
;; 29 = 19 (2) - 9
;; 19 = 9 (2) + 1
;; 9 = 2 (4) + 1
;; 2 = 1 (4) - 2
;; (lcm 19 29) = 551

;; 29 + 551


;; n1, n2, n3 natural numbers
;; first: 18 + n1 * 19 = x  <=> x - 18 / 19 = n1
;; secon: 28 + n2 * 29 = x
;; third: 44 + n3 * 45 = 28 + 522 * n4
;; 16 + n3 * 45 = 522 * n4


;; find x

;; 18 + n1 * 19 = 28 + n2 * 29
;; n1 = (10 + n2 * 29) / 19
;; n2 = 18

;; n4 = (16 + n3 * 45) / 522

;; n3 = (28 - 44 + n4 * 522) / 45


;; m = 522 n = 45 k = 16

;;
