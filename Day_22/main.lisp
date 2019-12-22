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

(defun split (d n)
    (if n
        (if (eq (first n) d)
            (cons nil (split d (rest n)))
            (let ((s (split d (rest n))))
                (if (first s)
                    (cons (cons (first n) (first s)) (rest s))
                    (cons (list (first n)) (rest s)))))
        nil))

(defun parse-line (line)
    (mapcar #'(lambda (x) (parse-integer (coerce x 'string))) (split #\space line)))

(defun get-input ()
    (mapcar #'parse-line
        (split #\newline (coerce (uiop:read-file-string (unwrap-or (second (command-args)) "input.txt")) 'list))))

(defun shift (list)
  "Move the first element to the end of the list."
  (append (rest list) (list (first list))))

(defparameter S 10007)

(defun deal-into-new-stack (i n)
    (mod (- (- S i) 1) S))

(defun dins (s n)
    (reverse s))

(defun deal-into-new-stack-rev (i n)
    (1- (- S i)))

(defun cut-N-cards (i n)
    (mod (+ i n) S))

(defun cut (a n)
    (if (= n 0)
        a
        (if (< n 0)
            (cut a (+ S n))
            (cut (shift a) (1- n)))))

(defun cut-N-cards-rev (i n)
    (- i n))

(defun deal-with-increment (i n)
    (mod (* n (- S i)) S))

(defun deal (a n)
    (mapcar #'(lambda (x) (nth x a)) (mapcar #'(lambda (x) (deal-with-increment x n)) (range S))))

(defun deal-with-increment-rev (i n)
    (* i n))

(defparameter *->* (list #'deal-into-new-stack #'cut-N-cards #'deal-with-increment))
(defparameter *<-* (list #'deal-into-new-stack-rev #'cut-N-cards-rev #'deal-with-increment-rev))

(defparameter *>* (list #'dins #'cut #'deal))

(defun one-round (init)
    (reduce #'(lambda (acc inp) (apply (nth (first inp) *<-*) (list acc (second inp)))) (get-input) :initial-value init))

(defun do-while (pred f p x n sum)
    (format t "S: ~A~%" sum)
    (if (funcall pred x)
        (do-while pred f x (funcall f x) (1+ n) (+ sum (- p x)))
        n))

(format t "Answer 1: ~A~%" (mod (one-round 2019) S))

(setf S 119315717514047)
(format t "Answer 2: ~A~%" (mod (* 101741582076661 (one-round 2020)) S))
