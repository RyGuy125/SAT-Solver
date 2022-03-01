;;;;;;;;;;;;;;;;;;;;; COMP 105 CONTINUATIONS ASSIGNMENT;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Exercise L 
;;;

;; The following function is used for testing purposes
(define value? (_) #t) ;; tells if the argument is a value


;; (list-of? A? v) returns #t if `v` is a list of values where each
;; value satisfies A?, otherwise returns #f

;; Effectively, this program ensures LIST(A) actually contains
;; elements only values in `A`.

;; laws:
;;   (list-of? A? '()) == #t
;;   (list-of? A? a) == #f, 
;;   (list-of? A? (cons y z) == #f, where z is not a list
;;   (list-of? A? (cons a as) == 
;;      == (list-of? A? as), when (A? a)
;;      == #f, when (not (A? a))
(define list-of? (A? x)
    (if (null? x)
        #t
        (if (not (pair? x))
            #f
            (if (A? (car x))
                (list-of? A? (cdr x))
                #f))))

        (check-assert      (list-of? symbol? '(green turf scrapes)))
        (check-assert (not (list-of? symbol? (cons 'Comp 105))))
        
        (check-assert (not (list-of? value? list-of?)))
        
        (check-assert (not (list-of? boolean? '(#t #f #t 4))))
        (check-assert      (list-of? boolean? '(#t #f #t #f #f)))
        
        (check-assert (not (list-of? function?
                                     (cons null? (cons symbol? '(3))))))
        (check-assert      (list-of? function?
                                     (cons null? (cons symbol? 
                                                       (cons number? '())))))
        
        (check-assert (not (list-of? number? '(4 5 52 34 null?))))
        (check-assert      (list-of? number? '(-2 34 23 -28323)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Exercise F
;;;

(record not [arg])
(record or [args])
(record and [args])

;; (formula? v) takes an arbitrary uScheme value and returns #t if the 
;; value represents a boolean formula made from records, #f otherwise

;; laws:
;;   (formula? v) == #t, when (symbol? v)
;;   (formula? v) == (list-of? formula? v), when (pair? v) 
;;   (formula? (make-not f)) == (formula? f)
;;   (formula? (make-or fs)) == (formula? fs)
;;   (formula? (make-and fs) == (formula? fs)
;;   (formula? v) == #f, when `v` has no forms of the previous laws
(define formula? (v)
    (cond
        [(symbol? v) #t]
        [(pair? v) (list-of? formula? v)]
        [(not? v) (formula? (not-arg v))]
        [(or? v)  (formula? (or-args v))]
        [(and? v) (formula? (and-args v))]
        [#t #f]))

        (check-assert      (formula? 'y))
        (check-assert (not (formula? 4)))
        (check-assert (not (formula? null?)))
        (check-assert      (formula? (make-not (make-or (list2 'x 'y)))))
        (check-assert      (formula? (make-and 
                                       (list3 (make-or (list3 'x 'y 'z))
                                              (make-and 
                                                (list2 (make-not 'x) 'x))
                                              (make-or (list3 'x 
                                                         (make-not 'y)
                                                         (make-not 'z)))))))
        (check-assert (not (formula? (make-and
                                       (list2 (make-or 
                                                (list2 (make-not null?) 'y))
                                              'm)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Exercise E
;;;

;; (eval-formula form env) takes a formula `form` and environment (association
;; list) `env` and returns #t if the key-symbols pairs in `env` map 
;; boolean values to each symbol in `f` such that `f` is satisfied by `env`.
;; #f otherwise. (eval-formula f env) is called only when every symbol in `f`
;; is bound in `env`. 

;; laws:
;;  (eval-formula f env) == (find f env), when (symbol? f)
;;  (eval-formula (make-not  f) env) == (not (eval-formula f env))
;;  (eval-formula (make-or  fs) env) == (exists? 
;;                                        (lambda (f) (eval-formula f env)) fs)
;;  (eval-formula (make-and fs) env) == (all? 
;;                                        (lambda (f) (eval-formula f env)) fs)
;;                                       

(define eval-formula (form env)
    (cond
        [(symbol? form) (find form env)]
        [(not? form)    (not (eval-formula (not-arg form) env))]
        [(or? form)     (exists? (lambda (f) (eval-formula f env))
                                 (or-args form))]
        [(and? form)    (all? (lambda (f) (eval-formula f env))
                              (and-args form))]
        [#t #f]))


        (check-assert (not (eval-formula (make-or '()) '((x #f) (y #f)))))
        (check-assert      (eval-formula (make-and '()) '((x #t))))
        (check-assert      (eval-formula (make-not '()) '((x #f))))
        (check-assert (not (eval-formula '() '((x #f) (y #t)))))
        (check-assert      (eval-formula (make-or  (list1 'x)) '((x #t))))
        (check-assert      (eval-formula (make-and (list1 'x)) '((x #t))))
        (check-assert      (eval-formula (make-not (list1 'x)) '((x #f))))
        (check-assert (not (eval-formula (make-and (list2
                                                     (make-not 'x)
                                                     'x))
                                         '((x #t)))))
        (check-assert      (eval-formula (make-or (list2 'x 'y)) 
                                         '((x #t) (y #f))))
        (check-assert (not (eval-formula (make-and 
                                           (list3 
                                             (make-or (list3 'x 'y 'z))
                                             (make-and (list2 
                                                         (make-not 'x)
                                                         'x))
                                             (make-or (list3
                                                        'x
                                                        (make-not 'y) 
                                                        (make-not 'z)))))
                                         '((x #t) (y #f) (z #t)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Exercise T
;;;


;; please consult solver-tests.scm for code


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Exercise S
;;;

;; (find-formula-true-asst form fail succ) uses continuations to search for
;; a mapping of variables to Booleans makes the evaluation of the formula 
;; `form` to evaluate to #t. The other parameters act as part of the 
;; continuation, with `fail` being a 0-arg failure continuation and `succ`
;; being a 2-arg success continuation that takes the partial solution and 
;; resume continuation.


(define find-formula-true-asst (form fail succ)
  (letrec 
 
;; laws:
;;   (find-formula-asst x             bool cur fail succeed) == 
;;     == (find-formula-symbol x bool cur fail succeed), where (symbol? x)
;;   (find-formula-asst (make-not f)  bool cur fail succeed) ==
;;     == (find-formula-asst f (not bool) cur fail succeed)
;;   (find-formula-asst (make-or  fs) #t   cur fail succeed) ==
;;     == (find-any-asst fs #t cur fail succeed)
;;   (find-formula-asst (make-or  fs) #f   cur fail succeed) ==
;;     == (find-all-asst fs #f cur fail succeed)
;;   (find-formula-asst (make-and fs) #t   cur fail succeed) ==
;;     == (find-all-asst fs #t cur fail succeed)
;;   (find-formula-asst (make-and fs) #f   cur fail succeed) ==
;;     == (find-any-asst fs #f cur fail succeed)
    ([find-formula-asst 
       (lambda (form bool cur fail succ)
         (cond
           [(symbol? form) (find-formula-symbol form bool cur fail succ)]
           [(not? form)    (find-formula-asst (not-arg form)
                                              (not bool) cur fail succ)]
           [(or? form)     (if bool
                             (find-any-asst (or-args form) bool cur fail succ)
                             (find-all-asst (or-args form) bool cur fail 
                                            succ))]
           [(and? form)    (if bool
                             (find-all-asst (and-args form) bool cur fail 
                                            succ)
                             (find-any-asst (and-args form) bool cur fail 
                                            succ))]
           [#t (error 'not-a-formula)]))]

;; laws:
;;   (find-all-asst '()               bool cur fail succeed) == 
;;     == (succeed cur fail)
;;   (find-all-asst (cons f fs)       bool cur fail succeed) ==
;;     == (find-formula-asst f bool cur fail
;;           (lambda (c res) (find-all-asst fs bool c res succeed)))
     [find-all-asst
       (lambda (form bool cur fail succ)
         (if (null? form)
           (succ cur fail)
           (find-formula-asst (car form) bool cur fail
                              (lambda (c res)  ; curr and resume continuation
                                (find-all-asst (cdr form) bool 
                                               c res succ)))))]

;;   (find-any-asst '()               bool cur fail succeed) == (fail)
;;   (find-any-asst (cons f fs)       bool cur fail succeed) == 
;;     == (find-formula-asst f bool cur
;;           (lambda () (find-any-asst fs bool cur fail succeed))
;;           succeed)
     [find-any-asst
       (lambda (form bool cur fail succ)
         (if (null? form)
           (fail)
           (find-formula-asst (car form) bool cur
                              (lambda () (find-any-asst (cdr form) bool cur
                                                        fail succ))
                              succ)))]

;;   (find-formula-symbol x bool cur fail succeed) ==
;;     == (succ (bind x bool cur) fail), where x is not bound in cur
;;   (find-formula-symbol x bool cur fail succeed) ==
;;     == (succ cur fail), where x is bool in cur
;;   (find-formula-symbol x bool cur fail succeed) ==
;;     == (fail), where x is (not bool) in cur
     [find-formula-symbol
       (lambda (x bool cur fail succ)
         (if (null? (find x cur))
           (succ (bind x bool cur) fail)
           (if (= (find x cur) bool)
             (succ cur fail)
             (fail))))])
    
    (find-formula-asst form #t '() fail succ)))


