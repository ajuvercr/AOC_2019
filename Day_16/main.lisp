#!/usr/bin/sbcl --script

(require "uiop")

(defun chunks-sub (ls num)
    (if (> num 0)
        (let ((r (chunks-sub (rest ls) (- num 1))))
            (cons (cons (first ls) (first r)) (rest r)))
        (list nil ls)))

(defun chunks (ls num)
    (if ls
        (let ((sub (chunks-sub ls num)))
            (cons (first sub) (chunks (second sub) num)))
        nil))

(defun range (max &key (min 0) (step 1))
   (loop for n from min below max by step
      collect n))

(defun get-last-digit (x)
    (multiple-value-bind (_ x) (floor (abs x) 10) x))

(defun enumerate (function list)
  (let ((idx 0))
    (loop for elt in list
      collect (funcall function elt idx)
      do (incf idx))))

(defun filter-map (f ls)
    (if ls
        (let ((out (funcall f (first ls))))
            (if out
                (cons out (filter-map f (rest ls)))
                (filter-map f (rest ls))))
        nil))

(defun to-int-list (input)
    (filter-map
        #'(lambda (x) (position x "0123456789"))
        (coerce input 'list)))

(defun get-input ()
    (to-int-list (uiop:read-file-string #p"input2.txt")))

(defun get-patter (index layer)
    (nth (mod (floor (1+ index) (1+ layer)) 4) '(0 1 0 -1)))

(defun do-layer (input layer)
    (get-last-digit (reduce #'+ (enumerate #'(lambda (x i) (* x (get-patter i layer))) input))))

(defun do-phase (input)
    (mapcar #'(lambda (i) (do-layer input i)) (range (list-length input))))

(defun print-for (x i)
    (format t "~A: ~A~%" i x)
    x)

(defun reduce-n-times (f input i)
    (if (> i 0)
        (reduce-n-times f (funcall f input) (1- i))
        input))

(defun get-with-skips (ls input offset)
    (if ls
        (let ((new-offset (+ (first ls) offset)))
            (cons (nth new-offset input) (get-with-skips (rest ls) input (1+ new-offset))))
        nil))

(defun concat (l1 l2)
    (reduce #'cons
                     l1
                     :initial-value l2
                     :from-end t))
(defun repeat (ls n)
    (reduce #'(lambda (x i) (concat x ls)) (range n) :initial-value nil))

(defun get-first (n ls)
    (if (> n 0)
        (cons (first ls) (get-first (1- n) (rest ls)))
        nil))

;; (format t "~A~%" (get-first 5 (repeat '(1 2 3) 3)))

;; (format t "~A~%" (get-input))
;; (format t "~A~%" (get-first 8 (reduce-n-times #'do-phase (get-input) 100)))
(defparameter *times* 8)
(format t  "~A~%" (chunks (repeat (get-input) *times*) (list-length (get-input))))
(format t "~A~%" (chunks (do-phase (repeat (get-input) *times*)) (list-length (get-input))))
(format t "~A~%" (list-length (get-input)))
;; (format t "~A~%" (get-with-skips (to-int-list "70000000") (to-int-list "98765432109876543210") 0))
