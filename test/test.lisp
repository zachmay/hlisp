nil
1
(cons nil nil)
(cons 1 nil)
(cons 'a nil)
(cons (quote a) nil)
(quote (a b))
'(a b)
(cons (quote A) (quote B)) 
(cons (quote C) (quote (A B))) 
(cons 1 (cons 2 (cons 3 nil))) 
(consSelf 1) 
(consSelf nil) 
(consSelf (quote (A B))) 
(cons (quote A) (cons (quote (B)) (cons (quote C) nil))) (
(cons (quote A) (cons (cons (quote B) nil) (cons (quote c) nil)))
(cons 'a (cons '(B) (cons 'C nil)))
(cons 'a (cons (cons 'B nil) (cons 'c nil)))
(test1 1) 
(test1 '1) 
(test1 'a) 
(test1 2) 
(test1 (cons 1 nil))
(car (quote (A B))) 
(cdr (quote (A B))) 
(car (quote (1))) 
(cdr (quote (1))) 
(car (cons 'A  'B)) 
(cdr (cons 'A  'B))
(lastElt nil) 
(lastElt (quote (1))) 
(lastElt (quote (1 2 3))) 
(lastElt (quote (1 (2 2) 3))) 
(lastElt (quote (1 2 (3 3)))) 
(incrementAll nil) 
(incrementAll '(1)) 
(incrementAll '(1 3 5)) 
(incrementAll2 nil 5) 
(incrementAll2 '(1) 5) 
(incrementAll2 '(1 3 5) 5) 
(ReverseIt (quote (a b))) 
(ReverseIt (quote (a b (c d)))) 
(ReverseIt (quote (a))) 
(ReverseIt ()) 
(ReverseAll (quote (a b))) 
(ReverseAll (quote (a b ((c d) e) f))) 
(ReverseAll (quote (a))) 
(ReverseAll ()) 
(ReverseAll (quote a))
(append (quote (a b c)) nil) 
(append nil (quote (a b c))) 
(append (quote (a b c)) (quote (d e f))) 
(mapcar (lambda (x) (* x x)) (quote (1 2 3 4))) 
(mapcar square '(1 2 3 4))
(mapcar (lambda (x) (+ 1 x)) (quote (1 2 3 4))) 
(+ 1 2 3)
(- 1 2 3)
(* 1 2 3)
(/ 1 2 3)
(+ 1)
(- 1)
(* 1)
(/ 1)
(setq pi 3.14159265358979)
pi
(defun circle-area (r) (* r r pi))
(circle-area 1)
(circle-area 10)
(* (setq a 5) a)
a
(cons 'a a)
(equal (cons 'a a) (cons a 'a))
(equal (+ 1 3) (+ 3 1))
(setq s "string")
(setq l '(a b c))
(mapcar numberp (list 'a a 5 's s "string" nil () t 'l l '(1 2 3)))
(mapcar stringp (list 'a a 5 's s "string" nil () t 'l l '(1 2 3)))
(mapcar listp   (list 'a a 5 's s "string" nil () t 'l l '(1 2 3)))
(mapcar null    (list 'a a 5 's s "string" nil () t 'l l '(1 2 3)))
(mapcar atom    (list 'a a 5 's s "string" nil () t 'l l '(1 2 3)))
(my-eval '(cons 1 2) ())
(my-eval '(+ 1 2 3) ())
(my-eval '((lambda (x y) (+ x y)) a b) '((a 10) (b 1)))