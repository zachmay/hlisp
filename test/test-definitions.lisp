(defun consSelf (L) (cons L L))

(defun test1  (A) (cond ((eq A 1) "yes") (T "no")))

(defun lastElt (L)
    (cond
        ((null L) nil)
        ((null (cdr L)) (car L))
        (T (lastElt (cdr L)))))

(defun incrementAll (L)
    (cond
        ((null L) nil) ; L is empty
        (T (cons (+ 1 (car L)) (incrementAll (cdr L))))))

(defun incrementAll2 (L A)
    (cond
        ((null L) nil) ; L is empty
        (T (cons (+ A (car L)) (incrementAll2 (cdr L) A)))
    ))

(defun append (L1 L2)
    (cond
        ((null L1) L2)
        (T (cons (car L1) (append (cdr L1) L2)))))

(defun ReverseIt (L)
    (cond
        ((null L) nil)
        (T (append (ReverseIt (cdr L)) (cons (car L) nil)))
    ))

(defun ReverseAll (L)
    (cond
        ((atom L) L)
        (T (append (ReverseAll (cdr L))
            (cons (ReverseAll (car L)) nil)))))

(defun mapcar (F Z)
    (cond
        ((null Z) nil)
        (t (cons (F (car Z)) (mapcar F (cdr Z))))))

(defun square (x) (* x x))

