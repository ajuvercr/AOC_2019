#!/usr/bin/sbcl --script

(require "uiop")

(defun range (max &key (min 0) (step 1))
   (loop for n from min below max by step
      collect n))

;;;;;;;;;;;;;;;;;;;; Helper functions ;;;;;;;;;;;;;;;;;;;;

(defun enumerate (function list)
  (let ((idx 0))
    (loop for elt in list
      collect (funcall function elt idx)
      do (incf idx))))

(defun enumerated (list)
    (enumerate #'(lambda (x i) (list x i)) list))

(defun get-user-input ()
    (read nil 'eof nil))

(defun concat (l1 l2)
    (if l1
        (cons (first l1) (concat (rest l1) l2))
        l2))

(defun flatmap (f l)
    (if l
        (concat (funcall f (first l)) (flatmap f (rest l)))
        nil))

(defun flatten (l) (flatmap #'(lambda (x) x) l))

(defun filter (f ls)
    (if ls
        (if (funcall f (first ls))
            (cons (first ls) (filter f (rest ls)))
            (filter f (rest ls)))
        nil))

(defun filter-map (f ls)
    (if ls
        (let ((out (funcall f (first ls))))
            (if out
                (cons out (filter-map f (rest ls)))
                (filter-map f (rest ls))))
        nil))

(defun shift (l)
    (if (second l)
        (cons (second l) (shift (cons (first l) (rest (rest l)))))
        l))

(defun update (idx value l)
    (if (= idx 0)
        (cons value (rest l))
        (cons (first l) (update (1- idx) value (rest l)))))

(defun split (d n)
    (if n
        (if (eq (first n) d)
            (cons nil (split d (rest n)))
            (let ((s (split d (rest n))))
                (if (first s)
                    (cons (cons (first n) (first s)) (rest s))
                    (cons (list (first n)) (rest s)))))
        nil))

(defun get-first (f ls)
    (if ls
        (if (funcall f (first ls))
            (first ls)
            (get-first f (rest ls)))
        nil))

(defun print-it (x)
    (if (> x 255)
        (format t "~%~A~%" x)
        (format t "~A" (code-char x))
    ) x)

;; (defun print-it (x) x)

;;;;;;;;;;;;;;;;;;;;   ROBOT function   ;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;; Index mode handler ;;;;;;;;;;;;;;;;;;;;

(defun my-position (input index base)
    (nth index input))

(defun my-immediate (input index base)
    index)

(defun my-relative (input index base)
    (+ base (nth index input)))

(defparameter *modes* (make-hash-table))
(setf (gethash nil *modes*) #'my-position)
(setf (gethash 0 *modes*) #'my-position)
(setf (gethash 1 *modes*) #'my-immediate)
(setf (gethash 2 *modes*) #'my-relative)

(defun get-it (mode input index base)
    (let ((out (nth (funcall (gethash mode *modes*) input index base) input)))
        (if out
            out
            0)))

;;;;;;;;;;;;;;;;;;;; Operation handlers ;;;;;;;;;;;;;;;;;;;;

(defmacro gen (name count fn)
    `(defun ,name (state modes)
        (destructuring-bind (input index base channel) state
            (let (
                (args (list ,@(mapcar #'(lambda (x) `(get-it (nth ,x modes) input (+ index ,(+ x 1)) base)) (range (- count 2)))))
                (dist (funcall (gethash (nth ,(- count 2) modes) *modes*) input (+ index ,(- count 1)) base)))
            (list (update dist (apply ,fn args) input) (+ ,count index) base channel)))))

(gen my-add 4 #'+)
(gen my-times 4 #'*)
(gen my-lt 4 #'(lambda (x y) (if (< x y) 1 0)))
(gen my-eq 4 #'(lambda (x y) (if (= x y) 1 0)))

;; Get movement command and update channel
(defun my-save (state modes)
    (destructuring-bind (input index base (ip output)) state
        ;; (format t "Saving ~A ~A~%" output (code-char (first output)))
        (let* (
                (dist (funcall (gethash (nth 0 modes) *modes*) input (+ index 1) base)))
            (list (update dist (first output) input) (+ 2 index) base (list ip (rest output))))))

(defun my-write (state modes)
    (destructuring-bind (input index base (ip output)) state
        (let ((arg1 (get-it (nth 0 modes) input (+ index 1) base)))
            (list input (+ 2 index) base (list (cons (print-it arg1) ip) output)))))

(defun my-jump-if-true (state modes)
    (destructuring-bind (input index base channel) state
        (let* (
            (arg1 (get-it (nth 0 modes) input (+ index 1) base))
            (arg2 (get-it (nth 1 modes) input (+ index 2) base)))
        (if (/= 0 arg1) (list input arg2 base channel) (list input (+ 3 index) base channel)))))

(defun my-jump-if-false (state modes)
    (destructuring-bind (input index base channel) state
        (let* (
            (arg1 (get-it (nth 0 modes) input (+ index 1) base))
            (arg2 (get-it (nth 1 modes) input (+ index 2) base)))
        (if (= 0 arg1) (list input arg2 base channel) (list input (+ 3 index) base channel)))))

(defun my-set-base (state modes)
    (destructuring-bind (input index base channel) state
        (let ((new-base (get-it (nth 0 modes) input (+ index 1) base)))
            (list input (+ 2 index) (+ new-base base) channel))))

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
(setf (gethash 9 *operations*) #'my-set-base)

(defun getop (code)
    (gethash code *operations*))

(defun get-input ()
    (mapcar #'parse-integer
        (mapcar
            #'(lambda (x) (coerce x 'string))
            (split #\, (coerce (uiop:read-file-string #p"input.txt") 'list)))))

(defun parse-modes (code)
    (if (< code 10)
        (values (list code 0 0 0))
        (multiple-value-bind (del opt) (floor code 10)
            (values (cons opt (parse-modes del))))))

(defun parse-code (code)
    (multiple-value-bind (del opt) (floor code 100)
        (list opt (parse-modes del))))

;; Step the states once
(defun action (state)
    (let* (
        (index (second state))
        (code (parse-code (nth index (first state)))))
    (funcall (getop (first code)) state (second code))))

;; Step states while opt code is not 99
(defun do-run (state)
    (if (= (nth (second state) (first state)) 99)
        (nth 3 state)
        (do-run (action state))))

;; Step all thrusters and 'input' channels
(defun do-program (input channel)
    (do-run (list input 0 0 (list nil channel))))

(defun print-state (state)
    (format t "~A~%" (coerce (mapcar #'(lambda (x) (if (= x 0) #\space (if (= x 1) #\# #\O))) (first state)) 'string))
    (if (second state)
        (print-state (rest state))
        nil))

(defun print-first-last (state)
    (format t "~A~A~A~%" (second (get-first #'(lambda (x) (= (first x) 1)) (enumerated (first state)))) #\tab (second (get-first #'(lambda (x) (= (first x) 1)) (reverse (enumerated (first state))))))
    (if (second state)
        (print-first-last (rest state)) nil))

(defparameter *input* (get-input))
(defun get-t (x y) (first (first (do-program *input* (list y x)))))

(defparameter *state* (mapcar #'(lambda (x) (mapcar #'(lambda (y) (get-t x y)) (range 50))) (range 50)))
(format t "Answer 1: ~A~%" (reduce #'+ (flatten *state*)))

(defun day-19-sub (x y)
    (if (= (get-t x y) 1)
        x
        (day-19-sub (1+ x) y)))

(defun day-19 (x y delta)
    (let ((x (day-19-sub x y)))
        (if (= (get-t (+ x delta) (- y delta)) 1)
            (list x (- y delta))
            (day-19 x (1+ y) delta))))

(let ((p2 (day-19 0 4 99)))
    (format t "Answer 2: ~A~%" (+ (* 10000 (second p2)) (first p2))))
