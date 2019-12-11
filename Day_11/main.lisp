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

;;;;;;;;;;;;;;;;;;;; Robot things ;;;;;;;;;;;;;;;;;;;;

(defparameter *state* nil)
(defparameter *position* '(0 0))
(defparameter *direction* '(0 -1))
(defparameter *should-paint* T)
(defparameter *total-pain* 0)

(defun uniq-add (pos colour ls)
    (if ls
        (if (equal (first (first ls)) pos)
            (cons (list pos colour) (rest ls))
            (cons (first ls) (uniq-add pos colour (rest ls))))
        (list (list pos colour))))

(defun get-colour (pos ls)
    (if ls
        (if (equal (first (first ls)) pos)
            (second (first ls))
            (get-colour pos (rest ls)))
        0))

(defun turn (right)
    (format t "Turning ~A~%" right)
    (destructuring-bind (x y) *direction*
        (setf *direction* (if (= x 0)
            (list (* y (if right -1 1)) 0)
            (list 0 (* x (if right 1 -1))))))
    (setf *position* (list (+ (first *position*) (first *direction*)) (+ (second *position*) (second *direction*))))
    (get-colour *position* *state*))

(defun paint (colour)
    (format t "Painting ~A~%" colour)
    (setf *total-pain* (1+ *total-pain*))
    (setf *state* (uniq-add *position* colour *state*)))

(defun robot (input)
    (if (setf *should-paint* (not *should-paint*))
        (turn (= 1 input))
        (paint input)))

(defun print-pos (pos)
    (format t "~A" (if (= 1(get-colour pos *state*)) "█" " ")))

(defun draw-state ()
    (let (
        (minx (reduce #'min (mapcar #'(lambda (x) (first (first x))) *state*)))
        (maxx (reduce #'max (mapcar #'(lambda (x) (first (first x))) *state*)))
        (miny (reduce #'min (mapcar #'(lambda (x) (second (first x))) *state*)))
        (maxy (reduce #'max (mapcar #'(lambda (x) (second (first x))) *state*))))
            (loop for y in (range (+ maxy 3) :min (- miny 2) ) do (loop for x in (range (+ maxx 3) :min (- minx 2)) do (print-pos (list x y))) (format t "~%"))))

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

;; TODO: handle writing to relative mode
(defun my-add (states modes)
    (destructuring-bind (input index base channel) (first states)
        (let* (
            (arg1 (get-it (nth 0 modes) input (+ index 1) base))
            (arg2 (get-it (nth 1 modes) input (+ index 2) base))
            (dist (funcall (gethash (nth 2 modes) *modes*) input (+ index 3) base))
            (resl (+ arg1 arg2)))
        (cons (list (update dist resl input) (+ 4 index) base channel) (rest states)))))

(defun my-times (states modes)
    (destructuring-bind (input index base channel) (first states)
        (let* (
            (arg1 (get-it (nth 0 modes) input (+ index 1) base))
            (arg2 (get-it (nth 1 modes) input (+ index 2) base))
            (dist (funcall (gethash (nth 2 modes) *modes*) input (+ index 3) base))
            (resl (* arg1 arg2)))
    (cons (list (update dist resl input) (+ 4 index) base channel) (rest states)))))

(defun my-save (states modes)
    (format t "Saving?~%")
    (destructuring-bind (input index base channel) (first states)
        (let* (
                (dist (funcall (gethash (nth 0 modes) *modes*) input (+ index 1) base))
                (resl (first channel)))
            (cons (list (update dist resl input) (+ 2 index) base (rest channel)) (rest states)))))

(defun my-write (states modes)
    (destructuring-bind (input index base channel) (first states)
        (let ((arg1 (get-it (nth 0 modes) input (+ index 1) base)))
            (cons (list input (+ 2 index) base (list (robot arg1))) (rest states)))))

(defun my-jump-if-true (states modes)
    (destructuring-bind (input index base channel) (first states)
        (let* (
            (arg1 (get-it (nth 0 modes) input (+ index 1) base))
            (arg2 (get-it (nth 1 modes) input (+ index 2) base)))
        (cons (if (/= 0 arg1) (list input arg2 base channel) (list input (+ 3 index) base channel)) (rest states)))))

(defun my-jump-if-false (states modes)
    (destructuring-bind (input index base channel) (first states)
        (let* (
            (arg1 (get-it (nth 0 modes) input (+ index 1) base))
            (arg2 (get-it (nth 1 modes) input (+ index 2) base)))
        (cons (if (= 0 arg1) (list input arg2 base channel) (list input (+ 3 index) base channel)) (rest states)))))

(defun my-lt (states modes)
    (destructuring-bind (input index base channel) (first states)
        (let* (
            (arg1 (get-it (nth 0 modes) input (+ index 1) base))
            (arg2 (get-it (nth 1 modes) input (+ index 2) base))
            (dist (funcall (gethash (nth 2 modes) *modes*) input (+ index 3) base))
            (resl (if (< arg1 arg2) 1 0)))
    (cons (list (update dist resl input) (+ 4 index) base channel) (rest states)))))

(defun my-eq (states modes)
    (destructuring-bind (input index base channel) (first states)
        (let* (
            (arg1 (get-it (nth 0 modes) input (+ index 1) base))
            (arg2 (get-it (nth 1 modes) input (+ index 2) base))
            (dist (funcall (gethash (nth 2 modes) *modes*) input (+ index 3) base))
            (resl (if (= arg1 arg2) 1 0)))
    (cons (list (update dist resl input) (+ 4 index) base channel) (rest states)))))

(defun my-set-base (states modes)
    (destructuring-bind (input index base channel) (first states)
        (let ((new-base (get-it (nth 0 modes) input (+ index 1) base)))
            (cons (list input (+ 2 index) (+ new-base base) channel) (rest states)))))


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
(defun do-program ()
    (do-run (list (list (get-input) 0 0 (list 2)))))

(do-program)
;; (robot 1)
;; (robot 0)

;; (robot 0)
;; (robot 0)

;; (robot 1)
;; (robot 0)

;; (robot 1)
;; (robot 0)

;; (robot 0)
;; (robot 1)

;; (robot 1)
;; (robot 0)

;; (robot 1)
;; (robot 0)

(defun get-uniq (ls)
    (if ls
        (if (not (member (first ls) (rest ls)))
            (cons (first ls) (get-uniq (rest ls)))
            (get-uniq (rest ls)))
        nil))

(format t "Pos ~A Direction ~A State ~A~%" *position* *direction* *state*)
(format t "Count ~A~%" (list-length (get-uniq (mapcar #'(lambda (x) (+ (first (first x)) (* (second (first x)) 100))) *state*))))
(format t "Total pain ~A~%" *total-pain*)

(draw-state)
