#!/usr/bin/sbcl --script

(require "uiop")

(defparameter *width* 25)
(defparameter *height* 6)

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

(defun filter-map (f ls)
    (if ls
        (let ((x (funcall f (first ls))))
            (if x
                (cons x (filter-map f (rest ls)))
                (filter-map f (rest ls))))
        nil))

(defun to-int (x)
    (position x "0123456789"))

(defun max-by (o1 o2)
    (if (> (first o1) (first o2))
        o1
        o2))

(defun app (f x)
    (list (funcall f x) x))

(defun list-max-by (f ls)
    (second (reduce #'max-by (mapcar #'(lambda (x) (app f x)) ls))))

(defun get-input ()
    (filter-map
        #'to-int
        (coerce (uiop:read-file-string #p"input.txt") 'list)))

(defun get-pixel (ls)
    (if ls
        (if (= 2 (first ls))
            (get-pixel (rest ls))
            (first ls))
        2))

(defun transpose (m)
  (apply #'mapcar #'list m))

(defun good-char (x)
    (if (= x 0)
        " "
        "█"))

(defun print-row (ls)
    (format t "~A~%" (reduce #'(lambda (x y) (concatenate 'string x y)) (mapcar #'good-char ls))))

(let ((layer (list-max-by
    #'(lambda (x) (* (count 0 x) -1))
    (chunks (get-input) (* *width* *height*)))))
(format t "Check: ~A~%" (* (count 1 layer) (count 2 layer))))

(mapcar #'print-row (chunks (mapcar #'get-pixel (transpose (chunks (get-input) (* *width* *height*)))) *width*))
