#!/usr/bin/sbcl --script

(require "uiop")

(defun sum (l)
    (reduce #'+ l))

(defun split (d n)
    (if n
        (if (eq (first n) d)
            (cons nil (split d (rest n)))
            (let ((s (split d (rest n))))
                (if (first s)
                    (cons (cons (first n) (first s)) (rest s))
                    (cons (list (first n)) (rest s)))))
        nil))

(defun set-input ()
        (mapcar
            #'(lambda (x) (set-orbits (mapcar #'(lambda (x) (coerce x 'string)) (split #\, x ))))
            (split #\newline (coerce (uiop:read-file-string #p"input.txt") 'list))))

(defparameter *orbits* (list))
(defparameter *orbit-count* (make-hash-table))

(defun orbits-sub (spacething l)
    (if l
        (if (equal spacething (second (first l)))
            (values (first (first l)))
            (values (orbits-sub spacething (rest l))))
        nil))

(defun orbits (spacething)
    (values (orbits-sub spacething *orbits*)))

(defun set-orbits (orbiter)
    (setf *orbits* (cons orbiter *orbits*))
    (values (second orbiter)))

(defun set-oc (spacething count)
    (setf (gethash spacething *orbit-count*) count)
    (values count))

(defun get-orbits-sub (spacething)
    (let ((parent (orbits spacething)))
        (if parent
            (values (set-oc spacething (+ (get-orbits parent) 1)))
            (values (set-oc spacething 0)))))

(defun get-orbits (spacething)
    (let ((oc (gethash spacething *orbit-count*)))
        (if oc
            (values oc)
            (values (get-orbits-sub spacething)))))

(defun build-path (spacething)
    (let ((parent (orbits spacething)))
        (if parent
            (cons parent (build-path parent))
            parent)))

(defun find-mutual (o1 o2)
    (format t "tyring ~A~%" (first o1))
    (let ((mem (member (first o1) o2)))
        (if mem
            (first o1)
            (find-mutual (rest o1) o2))))

;; (set-input)
;; (format t "~A~%" (orbits "L"))

(format t "Hops ~A~%" (sum (mapcar #'get-orbits (set-input))))
(format t "~A~%" (build-path "YOU"))
(format t "~A~%" (build-path "SAN"))
(let ((mut (find-mutual (build-path "YOU") (build-path "SAN"))))
    (format t "Count ~A~%" (+ (list-length (member mut (reverse (build-path "YOU")))) (list-length (member mut (reverse (build-path "SAN")))))))
