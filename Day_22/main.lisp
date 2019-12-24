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
(defun deal-into-new-stack-rev (i n)
    (1- (- S i)))

(defun cut-N-cards (i n)
    (mod (+ i n) S))
(defun cut-N-cards-rev (i n)
    (- i n))

(defun deal-with-increment (i n)
    (mod (* n (- S i)) S))
(defun deal-with-increment-rev (i n)
    (* i n))

(defparameter *->* (list #'deal-into-new-stack #'cut-N-cards #'deal-with-increment))
(defparameter *<-* (list #'deal-into-new-stack-rev #'cut-N-cards-rev #'deal-with-increment-rev))


(defun one-round (init)
    (mod (reduce #'(lambda (acc inp) (apply (nth (first inp) *<-*) (list acc (second inp)))) (get-input) :initial-value init) S))

(defun do-while (pred f p x n sum)
    (format t "S: ~A~%" sum)
    (if (funcall pred x)
        (do-while pred f x (funcall f x) (1+ n) (+ sum (- p x)))
        n))

(defun do-n-times (f x n)
    (if (= (mod n 1000) 0)
        (format t "~A ~A~%" n x)
        nil)
    (if (> n 0)
        (do-n-times f (funcall f x) (1- n))
        x))

(format t "Answer 1: ~A~%" (one-round 2019))



(setf S 119315717514047)
(format t "~A~%" (one-round 18066076525641))
(format t "~A~%" (do-n-times #'one-round 18066076525641 241))
;; (format t "Answer 1: ~A~%" (mod (one-round (mod (one-round 2019) S)) S))

;; (do-n-times #'one-round 2019 100)


;; (format t "Answer 2: ~A~%" (mod (* 101741582076661 (- 2020 (one-round 2020))) S))
