#!/usr/bin/sbcl --script

(require "uiop")

(defun range (max &key (min 0) (step 1))
   (loop for n from min below max by step
      collect n))

(defun command-args ()
  (or
   #+CLISP *args*
   #+SBCL *posix-argv*
   #+LISPWORKS system:*line-arguments-list*
   #+CMU extensions:*command-line-words*
   nil))

(defun unwrap-or (x def)
    (if x x def))

(defun enumerate (function list)
  (let ((idx 0))
    (loop for elt in list
      collect (funcall function elt idx)
      do (incf idx))))

(defun enumerated (list)
    (enumerate #'(lambda (x i) (list x i)) list))

(defun split (d n)
    (if n
        (if (eq (first n) d)
            (cons nil (split d (rest n)))
            (let ((s (split d (rest n))))
                (if (first s)
                    (cons (cons (first n) (first s)) (rest s))
                    (cons (list (first n)) (rest s)))))
        nil))

(defun flatten (x)
    (reduce #'append x))

(defun real-member (x ls)
    (if ls
        (if (equal (first ls) x)
            T
            (real-member x (rest ls)))
        nil))

(defun do-dir (loc dir)
    (mapcar #'+ loc dir))

(defun get-input ()
    (mapcar #'(lambda (row) (mapcar #'is-bug row)) (split #\newline (coerce (uiop:read-file-string (unwrap-or (second (command-args)) "input.txt")) 'list))))

(defun is-bug (x)
    (if (equal x #\#)
        1
        0))

(defun get-at (loc state)
    (if (or (< (apply #'min loc) 0) (> (apply #'max loc) 4))
        0
        (nth (first loc) (nth (second loc) state))))

(defun get-bug (loc state)
    (if (let ((c (reduce #'+ (mapcar #'(lambda (dir) (get-at (do-dir dir loc) state)) '((0 1) (1 0) (0 -1) (-1 0))))))
            (if (= (get-at loc state) 1)
                (= c 1)                 ;; Should the bug die?
                (or (= c 1) (= c 2)))) 1 0)) ;; Should a bug spawn?

(defun do-step (state &key (inner (empty)) (outer (empty)))
    (mapcar #'(lambda (y) (mapcar #'(lambda (x) (get-bug (list x y) state)) (range 5))) (range 5)))

(defun get-score (state)
    (reduce #'+ (enumerate #'(lambda (x i) (if (= x 1) (expt 2 i) 0)) (flatten state))))

(defun day-1 (state seen)
    (if (real-member state seen)
        (get-score state)
        (day-1 (do-step state) (cons state seen))))

(defun mlast (ls)
    (first (reverse ls)))

;; (top right bottom left)
(defun to-inner (state)
    (list
        (reduce #'+ (first state))
        (reduce #'+ (mapcar #'mlast state))
        (reduce #'+ (mlast state))
        (reduce #'+ (mapcar #'first state))))

;; (top right bottom left)
(defun to-outer (state)
    (list
        (get-at '(2 1) state)
        (get-at '(3 2) state)
        (get-at '(2 3) state)
        (get-at '(1 2) state)))

(defun empty () '(0 0 0 0))

(format t "Answer 1: ~A~%" (day-1 (get-input) nil))

(format t "~A~%" (to-outer (get-input)))
