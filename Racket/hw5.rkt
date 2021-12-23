;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

(define (racketlist->mupllist list)
  (if (null? list)
      (aunit)
      (apair (car list) (racketlist->mupllist (cdr list)))))

(define (mupllist->racketlist list)
  (if (aunit? list)
      null
      (cons (apair-e1 list) (mupllist->racketlist (apair-e2 list)))))

;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(or (int? e) (aunit? e) (closure? e)) e]
        [(fun? e) (closure env e)]
        [(ifgreater? e)
         (let ([e1 (eval-under-env (ifgreater-e1 e) env)]
               [e2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? e1) (int? e2))
              (if (> (int-num e1) (int-num e2))
                  (eval-under-env (ifgreater-e3 e) env)
                  (eval-under-env (ifgreater-e4 e) env))
              (error "ifgreater needs e1 and e2 to be integers")))]
        [(mlet? e)
         (let ([value (eval-under-env (mlet-e e) env)])
           (eval-under-env (mlet-body e) (cons (cons (mlet-var e) value) env)))]
        [(call? e)
         (let ([funexp (eval-under-env (call-funexp e) env)]
               [actual (eval-under-env (call-actual e) env)])
           (if (closure? funexp)
               (let* ([fun (closure-fun funexp)]
                      [enviroment (if (not (fun-nameopt fun))
                                     (append (list (cons (fun-formal fun) actual))(closure-env funexp))
                                     (append (list (cons (fun-formal fun) actual) (cons (fun-nameopt fun) funexp))(closure-env funexp)))])
                 (eval-under-env (fun-body fun) enviroment))
               (error "call expected to have closure")))]
        [(apair? e)
         (let ([e1 (eval-under-env (apair-e1 e) env)]
               [e2 (eval-under-env (apair-e2 e) env)])
           (apair e1 e2))]
        [(fst? e)
         (let ([value (eval-under-env (fst-e e) env)])
           (if(apair? value)
           (apair-e1 value)
           (error "fst expected to have pair")))]
         [(snd? e)
         (let ([value (eval-under-env (snd-e e) env)])
           (if(apair? value)
           (apair-e2 value)
           (error "snd? expected to have pair")))]
         [(isaunit? e)
          (let ([value (eval-under-env (isaunit-e e) env)])
            (if (aunit? value)
                (int 1)
                (int 0)))]
        
        
        ;; CHANGE add more cases here
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3)
  (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2)
  (if (null? lstlst)
      e2
      (mlet (caar lstlst) (cdar lstlst) (mlet* (cdr lstlst) e2))))

(define (ifeq e1 e2 e3 e4)
  (mlet* (list (cons "e1" e1) (cons "e2" e2))
         (ifgreater (var "e1") (var "e2") e4
                    (ifgreater (add (var "e1") (int 1)) (var "e2") e3 e4))))

;; Problem 4

(define mupl-map
  (fun #f "f"
  (fun "fun" "ls"
       (ifgreater (isaunit (var "ls")) (int 0)
                  (aunit)
                  (apair (call (var "f") (fst (var "ls"))) (call (var "fun")(snd (var "ls"))))))))

(define mupl-mapAddN 
 (mlet "map" mupl-map
       (fun #f "n"
            (fun #f "list"
                 (call (call mupl-map (fun #f "x"
                                     (add (var "x") (var "n"))))
                       (var "list")))))) 

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
