
    (check-assert (function? find-formula-true-asst))    ; correct name
    (check-error (find-formula-true-asst))                ; not 0 arguments
    (check-error (find-formula-true-asst 'x))             ; not 1 argument
    (check-error (find-formula-true-asst 'x (lambda () 'fail)))   ; not 2 args
    (check-error
       (find-formula-true-asst 'x 
                               (lambda () 'fail) 
                               (lambda (c r) 'succeed) 'z)) ; not 4 args

    (check-error (find-formula-true-asst 'x 
                                         (lambda () 'fail)
                                         (lambda () 'succeed)))
        ; success continuation expects 2 arguments, not 0
    (check-error (find-formula-true-asst 'x 
                                         (lambda () 'fail) 
                                         (lambda (_) 'succeed)))
        ; success continuation expects 2 arguments, not 1
    (check-error (find-formula-true-asst
                       (make-and (list2 'x (make-not 'x)))
                       (lambda (_) 'fail)
                       (lambda (_) 'succeed)))
        ; failure continuation expects 0 arguments, not 1


    (check-expect   ; x can be solved
       (find-formula-true-asst 'x
                               (lambda () 'fail)
                               (lambda (cur resume) 'succeed))
       'succeed)

    (check-expect   ; x is solved by '((x #t))
       (find-formula-true-asst 'x
                               (lambda () 'fail)
                               (lambda (cur resume) (find 'x cur)))
       #t)

    (check-expect   ; (make-not 'x) can be solved
       (find-formula-true-asst (make-not 'x)
                               (lambda () 'fail)
                               (lambda (cur resume) 'succeed))
       'succeed)

    (check-expect   ; (make-not 'x) is solved by '((x #f))
       (find-formula-true-asst (make-not 'x)
                               (lambda () 'fail)
                               (lambda (cur resume) (find 'x cur)))
       #f)

    (check-expect   ; (make-and (list2 'x (make-not 'x))) cannot be solved
       (find-formula-true-asst (make-and (list2 'x (make-not 'x)))
                               (lambda () 'fail)
                               (lambda (cur resume) 'succeed))
       'fail)

    (check-expect (find-formula-true-asst
                    f3 (lambda (_) 'fail) (lambda (cur res) cur)) s3)

