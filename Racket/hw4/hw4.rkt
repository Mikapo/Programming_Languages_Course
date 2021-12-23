
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below
(define (sequence low high strive)
  (letrec ([helper (lambda(low high strive acc)
                      (if (> low high)
                          acc
                          (helper (+ low strive) high strive (append acc (list low)) )))])
           (helper low high strive null)))

(define (string-append-map string-list suffix)
  (map (lambda(string)(string-append string suffix)) string-list ))

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) ((list-nth-mod (list 1 2) 2)(error "list-nth-mod: empty list"))]
        [#t (car (list-tail xs (remainder n (length xs))))]))

(define (stream-for-n-steps s n)
  (let* ([stream-pair (s)])
    (if (= n 0)
        (list)
        (append (list (car stream-pair)) (stream-for-n-steps (cdr stream-pair) (- n 1))))))