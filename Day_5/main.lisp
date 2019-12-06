#!/usr/bin/sbcl --script

(require "uiop")

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
        (setf (nth dist input) resl))
    (list input (+ 4 index)))

(defun my-times (input index modes)
    (let* (
        (arg1 (get-it input (+ index 1) (nth 0 modes)))
        (arg2 (get-it input (+ index 2) (nth 1 modes)))
        (dist (nth (+ index 3) input))
        (resl (* arg1 arg2)))
        (setf (nth dist input) resl))
    (list input (+ 4 index)))

(defun get-user-input ()
    (read nil 'eof nil))

(defun my-save (input index modes)
    (let* (
        (dist (nth (+ index 1) input))
        (resl (get-user-input)))
        (setf (nth dist input) resl))
    (list input (+ 2 index)))

(defun my-write (input index modes)
    (let ((arg1 (get-it input (+ index 1) (nth 0 modes))))
        (format t "Output: ~A~%" arg1 ))
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
        (setf (nth dist input) resl))
    (list input (+ 4 index)))

(defun my-eq (input index modes)
    (let* (
        (arg1 (get-it input (+ index 1) (nth 0 modes)))
        (arg2 (get-it input (+ index 2) (nth 1 modes)))
        (dist (nth (+ index 3) input))
        (resl (if (= arg1 arg2) 1 0)))
        (setf (nth dist input) resl))
    (list input (+ 4 index)))

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

(defun action (input index)
    (let ((code (parse-code (nth index input))))
        (values (funcall (getop (first code)) input index (second code) ))))

(defun do-program (input)
    (let ((state (list input 0)))
        (loop while (/= (nth (second state) (first state)) 99)
        do
            (setf state (action (first state) (second state)))))
    (values input))

(do-program (get-input))
