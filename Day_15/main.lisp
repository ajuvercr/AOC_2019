#!/usr/bin/sbcl --script

(require "uiop")

(defun sorted-add (item ls f)
    (if ls
        (if (funcall f item (first ls))
            (cons (first ls) (sorted-add item (rest ls) f))
            (cons item ls))
        (list item)))

(defun real-member (x ls)
    (if ls
        (if (equal x (first ls))
            ls
            (real-member x (rest ls)))
        nil))

(defun dijkstra-sib (state endf getf seen lw)
    (if (real-member (first (second (first state))) seen)
        (dijkstra-sib (rest state) endf getf seen lw)
        (if state
            (destructuring-bind ((weight path) &rest rest) state
                (if (funcall endf (first path))
                    (reverse path)
                    (let ((new-ones (mapcar #'(lambda (x) (list (+ weight (first x)) (cons (second x) path))) (funcall getf (first path)))))
                        (dijkstra-sib (reduce #'(lambda (ls x) (sorted-add x ls #'(lambda (x y) (< (first x) (first y))))) new-ones :initial-value rest) endf getf
                            (cons (first path) seen) (max weight lw)))))
            lw)))


;; Start: T is start position
;; Endf is function (T) -> bool to see if T is end
;; getf is function (T) -> (w, T)[] T to list of weights and other T
;;
;; dijkstra -> (weight, T)[]
(defun dijkstra (start endf getf)
    (dijkstra-sib (list (list 0 (list start))) endf getf nil 0))

(defun range (max &key (min 0) (step 1))
   (loop for n from min below max by step
      collect n))

;;;;;;;;;;;;;;;;;;;; Helper functions ;;;;;;;;;;;;;;;;;;;;

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
    (format t "~A~%" x)
    x)

;;;;;;;;;;;;;;;;;;;;   ROBOT function   ;;;;;;;;;;;;;;;;;;;;

;; Program state: ((location, from-dir, dir-todo)[] (location, string)[])
;; (first (first (first state))) == current location

(defparameter *running* nil)

(defun get-state-loc (loc ls)
    (get-first #'(lambda (x) (equal (first x) loc)) ls))

(defun get-oposite-dir (dir)
    (if (< dir 2)
        (abs (- dir 1))
        (+ (get-oposite-dir (- dir 2)) 2)))

(defun apply-dir2 (x dir)
    (+ x (- (* dir 2) 1)))

(defun apply-dir (loc dir)
    (if (< dir 2)
        (list (first loc) (apply-dir2 (second loc) dir))
        (list (apply-dir2 (first loc) (- dir 2)) (second loc))))

;; (location, string)[]
(defun print-pos (loc ls)
    (let ((fir (get-state-loc loc ls)))
        (if fir
            (format t "~A" (second fir))
            (format t " "))))

(defun draw-state (ls)
    (let (
        (minx (reduce #'min (mapcar #'(lambda (x) (first (first x))) ls)))
        (maxx (reduce #'max (mapcar #'(lambda (x) (first (first x))) ls)))
        (miny (reduce #'min (mapcar #'(lambda (x) (second (first x))) ls)))
        (maxy (reduce #'max (mapcar #'(lambda (x) (second (first x))) ls))))
            (loop for y in (range (+ maxy 3) :min (- miny 2) ) do (loop for x in (range (+ maxx 3) :min (- minx 2)) do (print-pos (list x y) ls)) (format t "~%"))))


(defun get-movement-command (state)
    (destructuring-bind (((loc from-dir dir-todo) &rest rest) ls) state
        (if (or from-dir dir-todo)
            (if dir-todo                    ;; Can i go somewhere?
                (if (get-state-loc (apply-dir loc (first dir-todo)) ls)
                    (get-movement-command (list (cons (list loc from-dir (rest dir-todo)) rest) ls))        ;; location is already visited, try another one
                    (list (first dir-todo) (list (cons (list loc from-dir dir-todo) rest) ls)))      ;; this is a good dir, go go go
                (list from-dir state))    ;; Return
            (setf *running* (list 0 ls)))))


(defun handle-moved (status state)
    (destructuring-bind (((loc from-dir dir-todo) &rest rest) ls) state
        ;; (draw-state (cons (list loc "D") ls))
        (if dir-todo
            (let ((new-loc (apply-dir loc (first dir-todo))))
                (if (= status 0)
                    (list (cons (list loc from-dir (rest dir-todo)) rest) (cons (list new-loc "#") ls))  ;; Wall
                    (list (cons (list new-loc (get-oposite-dir (first dir-todo)) '(0 1 2 3))
                        (cons (list loc from-dir (rest dir-todo)) rest)) (cons (list new-loc (if (= status 2) "X" ".")) ls)))) ;; No Wall
            (list rest ls))))

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
    (destructuring-bind (input index base channel) state
        (let* (
                (dist (funcall (gethash (nth 0 modes) *modes*) input (+ index 1) base))
                (mov (get-movement-command channel)))
            (list (update dist (1+ (first mov)) input) (+ 2 index) base (second mov)))))

(defun my-write (state modes)
    (destructuring-bind (input index base channel) state
        (let ((arg1 (get-it (nth 0 modes) input (+ index 1) base)))
            (list input (+ 2 index) base (handle-moved arg1 channel)))))

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
    (if *running*
        (second *running*)
        (do-run (action state))))

;; Step all thrusters and 'input' channels
(defun do-program (input)
    (do-run (list input 0 0 '((((0 0) nil (0 1 2 3))) (((0 0) "O"))))))


(defun get-options (pos ls)
    (mapcar #'(lambda (x) (list 1 x))
        (filter #'(lambda (x) (not (equal (second x) "#"))) (mapcar #'(lambda (x) (get-state-loc (apply-dir (first pos) x) ls)) '(0 1 2 3)))))

(defun is-finished (x)
    (equal (second x) "X"))

(defparameter *state* (do-program (get-input)))
(defparameter *path* (dijkstra '((0 0) ".") #'is-finished #'(lambda (x) (get-options x *state*))))
(draw-state (concat (mapcar #'(lambda (x) (list (first x) "█")) *path*) *state*))
(format t "Steps: ~A~%" (1- (list-length *path*)))
(format t "Mins ~A~%" (dijkstra (get-first #'is-finished *state*) #'(lambda (x) nil) #'(lambda (x) (get-options x *state*))))
