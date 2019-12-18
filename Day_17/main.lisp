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

(defun ffirst (x) (first (first x)))

(defun cons-back (x ls) (reverse (cons x (reverse ls))))

(defun check-continue-current (current fs ls)
    (if (equal (first ls) (nth current (ffirst fs))) 
        (list 
            (list (1+ current) fs (rest ls))
            (list 0 (shift fs) (rest ls))
            (list 0 (shift (shift fs)) (rest ls))
        )
        nil))

(defun check-add-builder (current fs ls)
    (if (and (second (first fs)) (= (list-length (ffirst fs)) current))
        (list 
            (list (1+ current) (cons (list (cons-back (first ls) (ffirst fs)) T) (rest fs)) (rest ls))
            (list 0 (shift (cons (list (cons-back (first ls) (ffirst fs)) nil) (rest fs))) (rest ls))
            (list 0 (shift (shift (cons (list (cons-back (first ls) (ffirst fs)) nil) (rest fs)))) (rest ls)))
        nil))

(defun flatten (x)
    (flatmap #'(lambda (x) x) x))

(defun calc-robot-input (current fs ls)   ;; fs: (a b c) ((() bool) )
    (if (> (list-length (ffirst fs)) 4)  ;; pruning
        nil
        (flatmap #'(lambda (x) (apply x (list current fs ls))) (list #'check-continue-current #'check-add-builder))))

(defun call-n-times (f input n)
    (if (> n 0)
        (call-n-times f (funcall f input) (1- n))
        input))

(defun get-fs (input)
    (mapcar #'first (second (first (call-n-times #'(lambda (x) (flatmap #'(lambda (x) (apply #'calc-robot-input x)) x)) (list (list 0 '((nil T) (nil T) (nil T)) input)) (list-length input))))))

(defun fs-to-big (current fs ls)
    (if ls
        (if current
            (if (equal (first current) (first ls))
                (fs-to-big (rest current) fs (rest ls))
                nil)
            (first (filter-map #'(lambda (x) (let ((r (fs-to-big (first x) fs ls))) (if r (cons (nth (second x) '(#\A #\B #\C)) r) nil))) (enumerated fs))))
        (list T)))

(defun get-it-all (input)
    (let ((fs (get-fs input)))
        (list (reverse (rest (reverse (fs-to-big nil fs input)))) fs)))

(defun print-state (state)
    (format t "~A~%" (coerce (first state) 'string))
    (if (second state)
        (print-state (rest state))
        nil))

(defun get-at (state coor)
    (if (< (apply #'min coor) 0)
        nil
        (nth (first coor) (nth (second coor) state))))

(defun zip (&rest rest)
    (apply #'mapcar #'list rest))

(defun all (ls)
    (if ls
        (if (first ls)
            (all (rest ls))
            nil)
        T))

;; chars: '(top left self right down) chars
(defun is-crossing (chars x y)
    (if (all (mapcar #'(lambda (x) (equal x #\#)) chars))
        (* x y)
        0))

;; Count the crossings on this depth
;; ( ((char)) int) -> int
(defun count-it (state depth)
    (reduce #'+ (enumerate #'(lambda (x i) (is-crossing x (1+ i) depth)) 
        (zip (rest (first state)) (second state) (rest (second state)) (rest (rest (second state))) (rest (nth 2 state))))))

(defun count-state (state depth)
    (if state
        (cons (count-it state depth) (count-state (rest state) (1+ depth)))
        nil))

(defun get-start (state depth)
    (let ((f (get-first #'(lambda (x) (equal (first x) #\^)) (enumerated (first state)))))
        (if f
            (list (second f) depth)
            (get-start (rest state) (1+ depth)))))

;; 0: up
;; 1: right
;; 2: down
;; 3: left
(defun get-dir (dir)
    (nth (mod dir 4) '((0 -1) (1 0) (0 1) (-1 0))))

(defun apply-dir (pos dir)
    (mapcar #'+ pos dir))

(defun move-along (pos dir state count)
    (let ((new-pos (apply-dir pos dir)))
        (if (equal (get-at state new-pos) #\#)
            (move-along new-pos dir state (1+ count))
            (list count pos))))

;; robot: (dir pos) state: state
;; -> ('L'|'R', new-dir, count, pos)
(defun get-move (robot state)
    (destructuring-bind (dir pos) robot
        (let ((new-pos (move-along pos (get-dir (+ dir 1)) state 0)))
            (if (equal pos (second new-pos))
                (cons #\L (cons (+ dir 3) (move-along pos (get-dir (+ dir 3)) state 0)))
                (cons #\R (cons (+ dir 1) new-pos))
            ))))

(defun get-moves (dir pos state)
    (destructuring-bind (l d c p) (get-move (list dir pos) state)
        (if (equal p pos)
            nil
            (cons (list l c) (get-moves d p state)))))

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
    (do-run (list input 0 0 channel)))

(defun get-options (pos ls)
    (mapcar #'(lambda (x) (list 1 x))
        (filter #'(lambda (x) (not (equal (second x) "#"))) (mapcar #'(lambda (x) (get-state-loc (apply-dir (first pos) x) ls)) '(0 1 2 3)))))

(defun join-with (ls d)
    (if ls
        (concat (coerce (first ls) 'list) (cons d (join-with (rest ls) d)))
        nil))

(defun get-robot-input ()
    (mapcar #'char-code (join-with '("A,B,A,C,B,C,B,C,A,C" "R,12,L,6,R,12" "L,8,L,6,L,10" "R,12,L,10,L,6,R,10" "n") #\newline)))

(defparameter *state* (reverse (mapcar #'reverse (split #\newline (mapcar #'code-char (first (do-program (get-input) '(() ()))))))))
;; (format t "Anwser 1: ~A~%" (reduce #'+ (count-state *state* 1)))
;; (format t "~%Anwser 2: ~A~%" (first (first (do-program (cons 2 (rest (get-input))) (list () (get-robot-input))))))
(format t "fs: ~A~%" (join-with (mapcar #'list (first (get-it-all (get-moves 0 (get-start *state* 0) *state*)))) #\,))

;; ((12 R) (6 L) (12 R) (8 L) (6 L) (10 L) (12 R) (6 L) (12 R) (12 R) (10 L) (6 L) (10 R) (8 L) (6 L) (10 L) (12 R) (10 L) (6 L) (10 R) (8 L) (6 L) (10 L) (12 R) (10 L) (6 L) (10 R) (12 R) (6 L) (12 R) (12 R) (10 L) (6 L) (10 R))

;; A: (12 R) (6 L) (12 R)        B
;; B: (8 L) (6 L) (10 L)         C
;; C: (12 R) (10 L) (6 L) (10 R) A
;; A,B,A,C,B,C,B,C,A,C

;; B,C,B,A,C,A,C,A,B,A