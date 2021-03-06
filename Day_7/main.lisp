#!/usr/bin/sbcl --script

(require "uiop")

;;;;;;;;;;;;;;;;;;;; Helper functions ;;;;;;;;;;;;;;;;;;;;

(defun get-user-input ()
    (read nil 'eof nil))

(defun range (max &key (min 0) (step 1))
   (loop for n from min below max by step
      collect n))

(defun concat (l1 l2)
    (if l1
        (cons (first l1) (concat (rest l1) l2))
        l2))

(defun flatmap (f l)
    (if l
        (concat (funcall f (first l)) (flatmap f (rest l)))
        nil))

(defun r-nth (l i)
    (if (= i 0)
        (list (first l) (rest l))
        (let ((nl (r-nth (rest l) (- i 1))))
            (list (first nl) (cons (first l) (second nl))))))

(defun r-all (l)
    (loop for x in (range (list-length l))
        collect (r-nth l x)))

(defun shift (l)
    (if (second l)
        (cons (second l) (shift (cons (first l) (rest (rest l)))))
        l))

(defun update (idx value l)
    (if (= idx 0)
        (cons value (rest l))
        (cons (first l) (update (1- idx) value (rest l)))))

(defun get-it (input index position)
    (if position
        (nth index input)
        (nth (nth index input) input)))

(defun split (d n)
    (if n
        (if (eq (first n) d)
            (cons nil (split d (rest n)))
            (let ((s (split d (rest n))))
                (if (first s)
                    (cons (cons (first n) (first s)) (rest s))
                    (cons (list (first n)) (rest s)))))
        nil))

(defun combinations-sub (option other-options)
    (if other-options
        (mapcar #'(lambda (x) (cons option x)) (combinations other-options))
        (list (list option))))

(defun combinations (options)
    (if options
        (flatmap #'(lambda (x) (combinations-sub (first x) (second x))) (r-all options))
        nil))


;;;;;;;;;;;;;;;;;;;; Operation handlers ;;;;;;;;;;;;;;;;;;;;

(defun my-add (states modes)
    (destructuring-bind (input index channel) (first states)
        (let* (
            (arg1 (get-it input (+ index 1) (nth 0 modes)))
            (arg2 (get-it input (+ index 2) (nth 1 modes)))
            (dist (nth (+ index 3) input))
            (resl (+ arg1 arg2)))
        (cons (list (update dist resl input) (+ 4 index) channel) (rest states)))))

(defun my-times (states modes)
    (destructuring-bind (input index channel) (first states)
        (let* (
            (arg1 (get-it input (+ index 1) (nth 0 modes)))
            (arg2 (get-it input (+ index 2) (nth 1 modes)))
            (dist (nth (+ index 3) input))
            (resl (* arg1 arg2)))
    (cons (list (update dist resl input) (+ 4 index) channel) (rest states)))))

(defun my-save (states modes)
    (destructuring-bind (input index channel) (first states)
        (let* (
                (dist (nth (+ index 1) input))
                (resl (first channel)))
            (cons (list (update dist resl input) (+ 2 index) (rest channel)) (rest states)))))

(defun my-write (states modes)
    (destructuring-bind (input1 index1 channel1) (first states)
        (destructuring-bind (input2 index2 channel2) (second states)
            (let ((arg1 (get-it input1 (+ index1 1) (nth 0 modes))))
                (shift (cons (list input1 (+ 2 index1) channel1) (cons (list input2 index2 (shift (cons arg1 channel2))) (rest (rest states)))))))))

(defun my-jump-if-true (states modes)
    (destructuring-bind (input index channel) (first states)
        (let* (
            (arg1 (get-it input (+ index 1) (nth 0 modes)))
            (arg2 (get-it input (+ index 2) (nth 1 modes))))
        (cons (if (/= 0 arg1) (list input arg2 channel) (list input (+ 3 index) channel)) (rest states)))))

(defun my-jump-if-false (states modes)
    (destructuring-bind (input index channel) (first states)
        (let* (
            (arg1 (get-it input (+ index 1) (nth 0 modes)))
            (arg2 (get-it input (+ index 2) (nth 1 modes))))
        (cons (if (= 0 arg1) (list input arg2 channel) (list input (+ 3 index) channel)) (rest states)))))

(defun my-lt (states modes)
    (destructuring-bind (input index channel) (first states)
        (let* (
            (arg1 (get-it input (+ index 1) (nth 0 modes)))
            (arg2 (get-it input (+ index 2) (nth 1 modes)))
            (dist (nth (+ index 3) input))
            (resl (if (< arg1 arg2) 1 0)))
    (cons (list (update dist resl input) (+ 4 index) channel) (rest states)))))

(defun my-eq (states modes)
    (destructuring-bind (input index channel) (first states)
        (let* (
            (arg1 (get-it input (+ index 1) (nth 0 modes)))
            (arg2 (get-it input (+ index 2) (nth 1 modes)))
            (dist (nth (+ index 3) input))
            (resl (if (= arg1 arg2) 1 0)))
    (cons (list (update dist resl input) (+ 4 index) channel) (rest states)))))


;; Get the right operation handler
(defparameter *operations* (make-hash-table))
(setf (gethash 1 *operations*) #'my-add)
(setf (gethash 2 *operations*) #'my-times)
(setf (gethash 3 *operations*) #'my-save)
(setf (gethash 4 *operations*) #'my-write)
(setf (gethash 5 *operations*) #'my-jump-if-true)
(setf (gethash 6 *operations*) #'my-jump-if-false)
(setf (gethash 7 *operations*) #'my-lt)
(setf (gethash 8 *operations*) #'my-eq)

(defun getop (code)
    (gethash code *operations*))


(defun get-input ()
    (mapcar #'parse-integer
        (mapcar
            #'(lambda (x) (coerce x 'string))
            (split #\, (coerce (uiop:read-file-string #p"input.txt") 'list)))))

(defun parse-modes (code)
    (if (< code 10)
        (values (list (/= 0 code)))
        (multiple-value-bind (del opt) (floor code 10)
            (values (cons (/= 0 opt) (parse-modes del))))))

(defun parse-code (code)
    (multiple-value-bind (del opt) (floor code 100)
        (list opt (parse-modes del))))


;; Step the states once
(defun action (states)
    (let* (
        (state (first states))
        (index (second state))
        (code (parse-code (nth index (first state)))))
    (funcall (getop (first code)) states (second code))))

;; Step states while opt code is not 99
(defun do-run (states)
    (if (= (nth (second (first states)) (first (first states))) 99)
        states
        (do-run (action states))))

;; Step all thrusters and 'input' channels
(defun do-program (thrusters)
    (first (nth 2 (first
        (do-run (cons (list (get-input) 0 (list (first thrusters) 0)) (mapcar #'(lambda (x) (list (get-input) 0 (list x))) (rest thrusters)))))
    )))

(format t "~A~%" (reduce #'max (mapcar #'do-program (combinations (list 5 6 7 8 9)))))
