#!/usr/bin/sbcl --script

(require "uiop")

(defun get-first (f ls)
    (if ls
        (if (funcall f (first ls))
            (first ls)
            (get-first f (rest ls)))
        nil))

(defun range (max &key (min 0) (step 1))
   (loop for n from min below max by step
      collect n))

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

(defun flat-map (f l)
    (if l
        (concat (funcall f (first l)) (flat-map f (rest l)))
        nil))

(defun filter-map (f ls)
    (if ls
        (let ((out (funcall f (first ls))))
            (if out
                (cons out (filter-map f (rest ls)))
                (filter-map f (rest ls))))
        nil))

(defun filter (f ls)
    (if ls
        (if (funcall f (first ls))
            (cons (first ls) (filter f (rest ls)))
            (filter f (rest ls)))
        nil))

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

(defun dijkstra-sib (state endf getf seen prune)
    (if (funcall prune (first (second (first state))) seen)
        (dijkstra-sib (rest state) endf getf seen prune)
        (if state
            (destructuring-bind ((weight path) &rest rest) state
                (if (funcall endf (first path))
                    (list weight (reverse path))
                    (let ((new-ones (filter #'(lambda (x) (not (real-member x seen))) (mapcar #'(lambda (x) (list (+ weight (first x)) (cons (second x) path))) (funcall getf (first path))))))
                        (dijkstra-sib (reduce #'(lambda (ls x) (sorted-add x ls #'(lambda (x y) (>= (first x) (first y))))) new-ones :initial-value rest) endf getf
                            (cons (first path) seen) prune))))
            nil)))

;; Start: T is start position
;; Endf is function (T) -> bool to see if T is end
;; getf is function (T) -> (w, T)[] T to list of weights and other T
;;
;; dijkstra -> (weight, T)[]
(defun dijkstra (start endf getf &key (prune #'real-member))
    (dijkstra-sib (list (list 0 (list start))) endf getf nil prune))

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
    (split #\newline (coerce (uiop:read-file-string #p"input.txt") 'list)))


;;;;;;;;;;;;;;;;;;;;; Transform input to graph ;;;;;;;;;;;;;;;;;;;;;

;; 0 up
;; 1 down
;; 2 left
;; 3 right
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


(defparameter *state* (get-input))


(defun get-at (loc)
    (nth (first loc) (nth (second loc) *state*)))

(defun is-open (x) (not (equal x #\#)))
(defun is-important (x) (and (is-open x) (not (equal x #\.))))

(defun get-options (dist seen loc)
    (filter-map
        #'(lambda (dir) (let ((new-loc (apply-dir loc dir))) (if (and (is-open (get-at new-loc)) (not (real-member new-loc seen))) (list (1+ dist) (cons new-loc seen) new-loc) nil)))
        (range 4)))

;; ls: ((dist prev_dir loc))
(defun transform-input (ls)
    (if ls
        (if (is-important (get-at (nth 2 (first ls))))
            (cons (list (first (first ls)) (get-at (nth 2 (first ls)))) (transform-input (rest ls)))
            (transform-input (concat
                (apply #'get-options (first ls)) (rest ls))))
        nil))

(defun get-min-for (ch ls)
    (list (reduce #'min (mapcar #'first (filter #'(lambda (x) (equal (second x) ch)) ls)) :initial-value 9999) ch))

(defun get-all-min (os ls)
    (filter #'(lambda (x) (/= 9999 (first x))) (mapcar #'(lambda (x) (get-min-for (first x) ls)) os)))

(defun start-transform (os loc)
    (get-all-min os (transform-input (get-options 0 nil loc))))

(defun get-all-importants ()
    (flat-map #'(lambda (y)
        (filter-map #'(lambda (x) (if (is-important (first x)) (list (first x) (list (second x) (second y))) nil))
            (enumerated (first y))))
        (enumerated *state*)))

(defun get-graph ()
    (let ((os (get-all-importants)))
    (mapcar #'(lambda (x) (list (first x) (start-transform os (second x)))) os)))


;; (format t "~A~%" (get-options *state* 0 5 '(8 4)))
;; (format t "~A~%" (mapcar #'(lambda (x) (list (first x) (get-at (second x) *state*))) (start-transform '(8 4) *state*)))
;; (format t "~A~%" (get-all-importants *state*))
(defparameter *graph*  (get-graph))
(defparameter *keys* (filter #'lower-case-p (mapcar #'first (get-all-importants))))
(format t "~A~%~A~%" *graph* *keys*)

;; Start: T is start position
;; Endf is function (T) -> bool to see if T is end
;; getf is function (T) -> (w, T)[] T to list of weights and other T

;; T: (char (keys))

(defun add-key (key keys)
    (if (equal key #\@)
        keys
        (if keys
            (if (equal (first keys) key)
                keys
                (cons (first keys) (add-key key (rest keys))))
            (list key))))

(defun endf (item)
    (= (list-length *keys*) (list-length (second item))))

(defun to-T (item keys)
    (destructuring-bind (weight item) item
        (if (upper-case-p item)
            (if (member (char-downcase item) keys)
                (list weight (list item keys))
                nil)
            (list weight (list item (add-key item keys))))))

(defun is-subset (one other)
    (if one
        (if (real-member (first one) other)
            (is-subset (rest one) other)
            nil)
        T))

(defun prune (one other)
    (if (equal (first one) (first other))
        (is-subset (second one) (second other))
        nil))

(defun get-f (item)
    (filter-map #'(lambda (x) (to-T x (second item))) (second (get-first #'(lambda (x) (equal (first x) (first item))) *graph*))))

;; (format t "~A~%" (get-f '(#\A (#\a))))

;; (format t "~A~%" *graph*)
(format t "~A~%" (dijkstra (list #\@ nil) #'endf #'get-f :prune #'(lambda (x xs) (get-first #'(lambda (y) (prune x y)) xs))))
