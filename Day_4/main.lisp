#!/usr/bin/sbcl --script

(require "uiop")

(defun 9-from (num)
    (if (< num 10)
        (cons num (9-from (+ num 1)))
        nil))

(defun to-num-rev (l)
    (if l
        (+ (first l) (* 10 (to-num-rev (rest l))))
        0))

(defun to-num (l)
    (let ((x (to-num-rev l)))
        (values x)))

(defun sum (l)
    (reduce #'+ l))

(defun has-double (num)
    (if (second num)
        (values (or (= (first num) (second num)) (has-double (rest num))))
        nil))

(defun max-count (num current)
    (if (second num)
        (if (= (first num) (second num))
            (max-count (rest num) (+ 1 current))
            (if (= current 2)
                (values T)
                (values (max-count (rest num) 1))))
        (values (= current 2))))

(defun is-good (num)
    (max-count num 1))

(defun cc (num)
    (if (= (list-length num) 21)
        (if (is-good num) (values 1) (values 0))
        (sum (loop for i in (9-from (first num)) collect (cc (cons i num))))))

(print (cc (list 0)))
