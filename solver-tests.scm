; Template for SAT Solver Test Cases 

(record not [arg])   
(record or  [args])
(record and [args])


; The first example is to test the edge case for no solution
(val f1 (make-and (list2
                    (make-or (list1 'x))
                    (make-or (list1 (make-not 'x))))))
(val s1 'no-solution)

; The following test is in conjunctive normal form
(val f2 (make-and (list2 
                    (make-or (list3 'x 'y 'z)) 
                    (make-or (list3 'x (make-not 'y) (make-not 'z))))))
(val s2 '((x #t)))

; The following test is in disjunctive normal form
(val f3 (make-or (list3
                   (make-and (list3 'x (make-not 'y) 'z))
                   (make-and (list3 (make-not 'x) (make-not 'y) 'z))
                   (make-and (list3 'x 'y (make-not 'z))))))
(val s3 '((x #t) (y #f) (z #t)))

    (check-expect (find-formula-true-asst 
                    f3 (lambda () 'fail) (lambda (cur res) cur)) s3)
    
    (check-expect (find-formula-true-asst
                    f2 (lambda () 'fail) (lambda (cur res) cur)) s2)

    (check-expect (find-formula-true-asst
                    f1 (lambda () 'no-solution) (lambda (cur res) cur)) s1)

