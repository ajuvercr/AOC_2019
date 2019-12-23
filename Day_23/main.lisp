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

(defun all (p ls)
    (if ls
        (and (funcall p (first ls)) (all p (rest ls)))
        T))

(defparameter *channels* (mapcar #'(lambda (x) (list x)) (range 50)))
(defparameter *nat-pack* nil)
(defparameter *last-y* 0)

(defun set-nat (pack)
    (if *nat-pack* 
        (setf *nat-pack* pack)
        (format t "First pack ~A~%" (setf *nat-pack* pack))))

(defun do-nat-thing ()
    (do-post (cons 0 *nat-pack*))
    (format t "Nat thing ~A~%" *nat-pack*)
    (if (= *last-y* (second *nat-pack*))
        (format t "First second y ~A~%" *last-y*) 
        nil)
    (setf *last-y* (second *nat-pack*)))

(defun get-empty-thing ()
    (if (and *nat-pack* (all #'(lambda (x) (not x)) *channels*)) (do-nat-thing) nil)
    -1)

(defun do-post (thing)
    (if (= (first thing) 255) 
        (set-nat (rest thing))
        (setf (nth (first thing) *channels*) (append (nth (first thing) *channels*) (rest thing))))
    nil)

(defun post (thing)
    (if (= (list-length thing) 3)
        (do-post (reverse thing))
        thing))

(defun get-thing (id)
    (if (nth id *channels*)
        (destructuring-bind (f &rest rest) (nth id *channels*)
            (setf (nth id *channels*) rest)
            f)
        (get-empty-thing)))

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
    (destructuring-bind (input index base (id buf)) state
        (let ((dist (funcall (gethash (nth 0 modes) *modes*) input (+ index 1) base)))
            (list (update dist (get-thing id) input) (+ 2 index) base (list id buf)))))

(defun my-write (state modes)
    (destructuring-bind (input index base (id buf)) state
        (let ((arg1 (get-it (nth 0 modes) input (+ index 1) base)))
            (list input (+ 2 index) base (list id (post (cons arg1 buf)))))))

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
(defun do-run (states)
    (do-run (shift (cons (action (first states)) (rest states)))))

(defparameter *input* (get-input))

;; Step all thrusters and 'input' channels
(defun build-state (id)
    (list *input* 0 0 (list id nil)))

(do-run (mapcar #'build-state (range 50)))
