#!/usr/bin/sbcl --script

(require "uiop")

(defun update (idx value l)
    (if (= idx 0)
        (cons value (rest l))
        (cons (first l) (update (1- idx) value (rest l)))))

(defun get-it (input index position)
    (if position
        (nth index input)
        (nth (nth index input) input)))

(defun my-add (input index modes)
    (let* (
        (arg1 (get-it input (+ index 1) (nth 0 modes)))
        (arg2 (get-it input (+ index 2) (nth 1 modes)))
        (dist (nth (+ index 3) input))
        (resl (+ arg1 arg2)))
    (list (update dist resl input) (+ 4 index))))

(defun my-times (input index modes)
    (let* (
        (arg1 (get-it input (+ index 1) (nth 0 modes)))
        (arg2 (get-it input (+ index 2) (nth 1 modes)))
        (dist (nth (+ index 3) input))
        (resl (* arg1 arg2)))
    (list (update dist resl input) (+ 4 index))))

(defun get-user-input ()
    (read nil 'eof nil))

(defparameter *thrusters* (list))
(defun save-thruster (num)
    (setf *thrusters* (cons (first *thrusters*) (cons num (rest *thrusters*)))))

(defun get-thruster ()
    (let ((out (first *thrusters*)))
        (progn
            (setf *thrusters* (rest *thrusters*))
            (values out))))

(defun my-save (input index modes)
    (let* (
        (dist (nth (+ index 1) input))
        (resl (get-thruster)))
    (list (update dist resl input) (+ 2 index))))

(defun my-write (input index modes)
    (let ((arg1 (get-it input (+ index 1) (nth 0 modes))))
        (save-thruster arg1 ))
    (list input (+ 2 index)))

(defun jump-if-true (input index modes)
    (let* (
        (arg1 (get-it input (+ index 1) (nth 0 modes)))
        (arg2 (get-it input (+ index 2) (nth 1 modes))))
        (if (/= 0 arg1) (list input arg2) (list input (+ 3 index)))))

(defun jump-if-false (input index modes)
    (let* (
        (arg1 (get-it input (+ index 1) (nth 0 modes)))
        (arg2 (get-it input (+ index 2) (nth 1 modes))))
        (if (= 0 arg1) (list input arg2) (list input (+ 3 index)))))

(defun my-lt (input index modes)
    (let* (
        (arg1 (get-it input (+ index 1) (nth 0 modes)))
        (arg2 (get-it input (+ index 2) (nth 1 modes)))
        (dist (nth (+ index 3) input))
        (resl (if (< arg1 arg2) 1 0)))
    (list (update dist resl input) (+ 4 index))))


(defun my-eq (input index modes)
    (let* (
        (arg1 (get-it input (+ index 1) (nth 0 modes)))
        (arg2 (get-it input (+ index 2) (nth 1 modes)))
        (dist (nth (+ index 3) input))
        (resl (if (= arg1 arg2) 1 0)))
    (list (update dist resl input) (+ 4 index))))


(defparameter *operations* (make-hash-table))
(setf (gethash 1 *operations*) #'my-add)
(setf (gethash 2 *operations*) #'my-times)
(setf (gethash 3 *operations*) #'my-save)
(setf (gethash 4 *operations*) #'my-write)
(setf (gethash 5 *operations*) #'jump-if-true)
(setf (gethash 6 *operations*) #'jump-if-false)
(setf (gethash 7 *operations*) #'my-lt)
(setf (gethash 8 *operations*) #'my-eq)

(defun split (d n)
    (if n
        (if (eq (first n) d)
            (cons nil (split d (rest n)))
            (let ((s (split d (rest n))))
                (if (first s)
                    (cons (cons (first n) (first s)) (rest s))
                    (cons (list (first n)) (rest s)))))
        nil))

(defun get-input ()
    (mapcar #'parse-integer
        (mapcar
            #'(lambda (x) (coerce x 'string))
            (split #\, (coerce (uiop:read-file-string #p"input.txt") 'list)))))

(defun heads (l)
    (reverse (rest (reverse l))))

(defun verb-noun (id)
    (let* ((noun (floor id 100))
           (verb (- id (* noun 100))))
        (values verb noun)))

(defun parse-modes (code)
    (if (< code 10)
        (values (list (/= 0 code)))
        (multiple-value-bind (del opt) (floor code 10)
            (values (cons (/= 0 opt) (parse-modes del))))))

(defun parse-code (code)
    (multiple-value-bind (del opt) (floor code 100)
        (list opt (parse-modes del))))

(defun getop (code)
    (gethash code *operations*))

(defun action (states)
    (let* (
        (state (first states))
        (index (second state))
        (code (parse-code (nth index (first state)))))
    (funcall (getop (first code)) (first state) index (second code))))

(defun do-step (state)
    (if (= (nth (second state) (first state)) 99)
        (first state)
        (do-step (action (list state)))))

(defun do-program (input)
    (let ((state (list input 0)))
        (do-step state)))

(defun r-program ()
    (do-program (get-input))
    (if (first *thrusters*)
        (r-program)
        (second *thrusters*)))

(defun r (thrusters)
    (setf *thrusters* thrusters)
    (save-thruster 0)
    (r-program))

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

(defun flat1 (l)
    (if l
        (reduce #'concat l)
        nil))

(defun flat (l amount)
    (if l
        (if (> amount 0)
            (values (concat (flat (first l) (- amount 1)) (flat (rest l) amount)))
            (values l))
        nil))

(defun r-nth (l i)
    (if (= i 0)
        (list (first l) (rest l))
        (let ((nl (r-nth (rest l) (- i 1))))
            (list (first nl) (cons (first l) (second nl))))))

(defun r-all (l)
    (loop for x in (range (list-length l))
        collect (r-nth l x)))

(defun build-sub (option other-options)
    (if other-options
        (mapcar #'(lambda (x) (cons option x)) (build-list other-options))
        (list (list option))))

(defun build-list (options)
    (if options
        (flatmap #'(lambda (x) (build-sub (first x) (second x))) (r-all options))
        nil))

(setf *thrusters* (list 4 0 3 2 1 0))
(print (reduce #'max (mapcar #'r (build-list (list 1 2 3 4 0)))))
