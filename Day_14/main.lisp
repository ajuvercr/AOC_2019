#!/usr/bin/sbcl --script

(require "uiop")

(defun concat (l1 l2)
    (if l1
        (cons (first l1) (concat (rest l1) l2))
        l2))

(defun flatmap (f l)
    (if l
        (concat (funcall f (first l)) (flatmap f (rest l)))
        nil))

(defun split (d n)
    (if n
        (if (eq (first n) d)
            (cons nil (split d (rest n)))
            (let ((s (split d (rest n))))
                (if (first s)
                    (cons (cons (first n) (first s)) (rest s))
                    (cons (list (first n)) (rest s)))))
        nil))

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

(defun any (f ls)
    (if ls
        (if (funcall f (first ls))
            (first ls)
            (any f (rest ls)))
        nil))

(defun partition (f ls)
    (if ls
        (destructuring-bind (f-true f-false) (partition f (rest ls))
            (if (funcall f (first ls))
                (list (cons (first ls) f-true) f-false)
                (list f-true (cons (first ls) f-false))))
        (list nil nil)))

(defun get-first (f ls)
    (if (funcall f (first ls))
        (list (first ls) (rest ls))
        (let ((x (get-first f (rest ls))))
            (list (first x) (cons (first ls) (second x))))))

(defun maybe-update (with x)
    (if (equal (second with) (second x))
        (list (first x) (second x) (+ (first with) (nth 2 x)))
        x))

(defun update-state (state x)
    (mapcar #'(lambda (row) (list (first row) (maybe-update x (second row)))) state))

(defun get-times (x)
    (ceiling (nth 2 x) (first x)))

(defun handle-used (state to-handle)
    (let ((x (mapcar #'(lambda (x) (list (* (first x) (get-times (second to-handle))) (second x))) (first to-handle))))
        (reduce #'update-state x :initial-value state)))

(defun is-used-as-input (x ls)
    (not (any #'(lambda (y) (equal x y)) (mapcar #'second (flatmap #'first ls)))))

(defun set-wanted (x)
    (if (equal (second x) "FUEL")
        (list (first x) (second x) 1)
        (list (first x) (second x) 0)))

(defun parse-line (line)
    (let (
        (parsed (mapcar #'(lambda (x) (list (parse-integer (coerce (first x) 'string)) (coerce (second x) 'string)))
            (chunks (split #\space line) 2))))
        (list (rest (reverse parsed)) (set-wanted (first (reverse parsed))))))

(defun get-input ()
    (mapcar #'parse-line
        (split #\newline
            (coerce (uiop:read-file-string #p"input2.txt") 'list))))

(defun do-step (state)
    (destructuring-bind (f-true f-false) (partition #'(lambda (x) (is-used-as-input (second (second x)) state)) state)
        (if f-false
            (do-step (reduce #'handle-used f-true :initial-value f-false))
            f-true)))

(defun last-step (state)
    (reduce #'+ (mapcar #'(lambda (x) (* (first (first (first x))) (get-times (second x)))) state)))

(format t "~A~%" (do-step (get-input)))
(format t "~A~%" (last-step (do-step (get-input))))
