#!/usr/bin/sbcl --script

(require "uiop")


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

(defun action (input index)
    (let* ((op (nth index input))
          (arg1 (nth (nth (+ index 1) input) input))
          (arg2 (nth (nth (+ index 2) input) input))
          (dist (nth (+ index 3) input))
          (resl (if (= op 1)
              (+ arg1 arg2)
              (* arg1 arg2))))
        (setf (nth dist input) resl))
    (values input))

(defun do-program (input noun verb)
    (setf (nth 1 input) noun)
    (setf (nth 2 input) verb)
    (let ((index 0))
        (loop while (/= (nth index input) 99)
        do  (setf input (action input index))
            (setf index (if
                (< (+ index 4) (list-length input))
                    (+ index 4)
                    0))))
    (values input))

(defun is-good (input id)
    (multiple-value-bind (verb noun) (verb-noun id) (do-program input noun verb))
    (values (/= (first input) 19690720)))

(let ((id 0))
      (loop while (is-good (get-input) id)
      do (setf id (+ id 1)))
      (format t "Result ~A~%" id))
