#!/usr/bin/sbcl --script

(require "uiop")

(defparameter *current* nil)

(defun split (d n)
    (if n
        (if (eq (first n) d)
            (cons nil (split d (rest n)))
            (let ((s (split d (rest n))))
                (if (first s)
                    (cons (cons (first n) (first s)) (rest s))
                    (cons (list (first n)) (rest s)))))
        nil))

(defun dist (o1 o2)
    (+ (abs (- (first o1) (first o2))) (abs (- (second o1) (second o2)))))

(defun sorted-insert (x ls)
    (if ls
        (let ((d (dist x *current*)))
            (if (< d (dist (first ls) *current*))
                (cons x ls)
                (cons (first ls) (sorted-insert x (rest ls)))))
        (list x)))

(defun add-uniq (ls el)
    (if ls
        (let ((angle (first el)) (loc (second el)) (cur (first ls)))
            (if (< angle (first cur))
                (cons (list angle (list loc)) ls)
                (if (equal angle (first cur))
                    (cons (list angle (sorted-insert loc (second cur))) (rest ls))
                    (cons cur (add-uniq (rest ls) el)))))
        (list (list (first el) (list (second el))))))

(defun enumerate (function list)
  (let ((idx 0))
    (loop for elt in list
      collect (funcall function elt idx)
      do (incf idx))))

(defun filter (f ls)
    (if ls
        (if (funcall f (first ls))
            (cons (first ls) (filter f (rest ls)))
            (filter f (rest ls)))
        nil))

(defun filter-map (f ls)
    (if ls
        (let ((x (funcall f (first ls))))
            (if x
                (cons x (filter-map f (rest ls)))
                (filter-map f (rest ls))))
        nil))

(defun max-by (o1 o2)
    (if (> (first o1) (first o2))
        o1
        o2))

(defun to-angle (s)
    (atan (first s) (* -1 (second s))))

(defun shift (l)
    (if (second l)
        (cons (second l) (shift (cons (first l) (rest (rest l)))))
        l))

(defun shift-while (f ls)
    (if (funcall f (first ls))
        (shift-while f (shift ls))
        ls))

(defun translate (s1 s2)
    (list (- (first s1) (first s2)) (- (second s1) (second s2))))

(defun handle (origin ls)
    (setf *current* origin)
    (reduce #'add-uniq (mapcar #'(lambda (x) (list (to-angle (translate x origin)) x)) (filter #'(lambda (x) (not (equal x origin))) ls)) :initial-value nil))

(defun app (f x)
    (list (funcall f x) x))

(defun list-max-by (f ls)
    (second (reduce #'max-by (mapcar #'(lambda (x) (app f x)) ls))))

(defun get-input-helper (depth input)
    (if input
        (let ((row (first input)))
            (if row
                (cons (list (first (first row)) depth) (get-input-helper depth (cons (rest row) (rest input))))
                (get-input-helper (+ depth 1) (rest input))))
        nil))

(defun get-input ()
    (get-input-helper 0
        (mapcar #'(lambda (row) (filter #'(lambda (x) x) (enumerate #'(lambda (x idx) (if (char= x #\#) (list idx) nil)) row)))
            (split #\newline (coerce (uiop:read-file-string #p"input.txt") 'list)))))

(defun print-row (ls)
    (format t "~A~%" (reduce #'(lambda (x y) (concatenate 'string x y)) (mapcar #'good-char ls))))

(defun get-it ()
    (second (let ((input (get-input)))
            (reduce #'max-by (mapcar #'(lambda (x) (app #'(lambda (y) (list-length (first y))) x)) (mapcar #'(lambda (origin) (list (handle origin input) origin)) input))))))

(defun get-for-2 ()
    (shift-while #'(lambda (x) (< (first x) 0)) (handle (second (get-it)) (get-input))))

(defun laser-it (c x)
    (destructuring-bind (angle astroids) (first x)
        (format t "~A: ~A~%" c (first astroids))
        (if (second astroids)
            (shift (cons (list angle (rest astroids)) (rest x)))
            (rest x))))

(defun exec (c at f input)
    (if (>= c at)
        (exec c (+ at 1) f (funcall f at input))
        (values)))

(exec 200 1 #'laser-it (get-for-2))
