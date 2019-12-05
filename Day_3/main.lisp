#!/usr/bin/sbcl --script

(require "uiop")

(defun manhat-dist (x y)
    (+ (abs (- (first x) (first y))) (abs (- (second x) (second y)))))

(defun minimum (l)
    (values (reduce #'min l)))

(defun concat (l1 l2)
    (values (reduce #'cons
                     l1
                     :initial-value l2
                     :from-end t)))

(defun flat1 (l amount)
    (if l
        (if (> amount 0)
            (values (concat (flat1 (first l) (- amount 1)) (flat1 (rest l) amount)))
            (values l))
        nil))

(defun flatten (l)
  (cond ((null l) nil)
        ((atom l) (list l))
        (t (loop for a in l appending (flatten a))))
)

(defun split (d n)
    (if n
        (if (eq (first n) d)
            (cons nil (split d (rest n)))
            (let ((s (split d (rest n))))
                (if (first s)
                    (cons (cons (first n) (first s)) (rest s))
                    (cons (list (first n)) (rest s)))))
        nil))

(defun sort-it (x y)
    (if (= (first x) (first y))
        (values (< (second x) (second y)))
        (values (< (first x) (second y)))))

(defun cross (o1 o2)
    (destructuring-bind (d1 x11 y11 _ x12 y12) (flatten o1)
        (destructuring-bind (d2 x21 y21 _ x22 y22) (flatten o2)
            (if (eq (= x11 x12) (= x21 x22))
                (values nil)
                (if (= x11 x12)
                    (if (and (and (<= (min x21 x22) x11) (<= x11 (max x21 x22))) (and (<= (min y11 y12) y21) (<= y21 (max y11 y12))))
                        (values (+ (+ d1 (abs (- y11 y21))) (+ d2 (abs (- x11 x21)))))
                        (values nil))
                    (if (and (and (<= (min y21 y22) y11) (<= y11 (max y21 y22))) (and (<= (min x11 x12) x21) (<= x21 (max x11 x12))))
                        (values (+ (+ d1 (abs (- x11 x21))) (+ d2 (abs (- y21 y11)))))
                        (values nil)))))))

(defun up (input amount)
    (values (list (+ (first input) amount) (second input))))

(defun down (input amount)
    (values (list (- (first input) amount) (second input))))

(defun right (input amount)
    (values (list (first input) (+ (second input) amount))))

(defun left (input amount)
    (values (list (first input) (- (second input) amount))))

(defun parse (x)
    (let ((amount (parse-integer (coerce (rest x) 'string))))
        (if (eq (first x) #\R)
            (values (list #'right amount))
            (if (eq (first x) #\L)
                (values (list #'left amount))
                (if (eq (first x) #\U)
                    (values (list #'up amount))
                    (values (list #'down amount)))))))

(defun kinda-reduce (state l)
    (if l
        (let* ((new-state (funcall (first (first l)) (first state) (second (first l))))
               (distance (+ (second state) (manhat-dist new-state (first state)))))
            (cons (cons distance new-state) (kinda-reduce (list new-state distance) (rest l))))
        nil))

(defun pairify (l)
    (if (rest l)
        (values (cons (list (first l) (second l)) (pairify (rest l))))))

(defun smallest-test (head tail)
    (if tail
        (values (cons (cross head (first tail)) (smallest-test head (rest tail))))
        nil))

(defun smaller-test (head tail)
    (if tail
        (values (cons (smallest-test head (pairify (first tail))) (smaller-test head (rest tail))))
        nil))

(defun small-test (head tail)
    (if head
        (values (cons (smaller-test (first head) tail) (small-test (rest head) tail)))
        nil))

(defun big-test (l)
    (if l
        (let ((head (first l))
              (tail (rest l)))
            (values (cons (small-test (pairify (rest head)) tail) (big-test tail))))
        nil))

(defun get-input ()
    (mapcar
        #'(lambda (x) (cons (list 0 0 0) (kinda-reduce (list (list 0 0) 0) (mapcar #'parse (split #\, x)))))
        (split #\newline (coerce (uiop:read-file-string #p"input.txt") 'list))))

(print (minimum (flatten (big-test (get-input)))))
