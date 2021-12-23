
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

(define (funny-number-stream)
  (letrec ([stream (lambda (x)(cons (if (= (remainder (+ x 1) 5) 0)
                                        (* (+ x 1) (- 0 1))
                                         (+ x 1))
                                    (lambda () (stream (+ x 1)))))])
    (stream 0)))

(define (dan-then-dog)
  (letrec ([stream (lambda (x) (if (= x 0)
                                    (cons "dan.jpg" (lambda () (stream 1)))
                                    (cons "dog.jpg" (lambda () (stream 0)))))])
           (stream 0)))

(define (stream-add-zero s)
  (letrec ([stream (lambda (x)
                     (let* ([stream-pair (x)])
                       (cons (cons 0 (car stream-pair)) (lambda () (stream (cdr stream-pair))) )))])
    (lambda () (stream s))))

(define (cycle-lists xs ys)
  (letrec ([stream (lambda (xs ys) (cons (cons (car xs) (car ys))
                                         (lambda () (stream (append (cdr xs) (list (car xs))) (append (cdr ys) (list (car ys)))))))])
    (lambda () (stream xs ys))))

(define (vector-assoc v vec)
  (letrec ([helper (lambda (v vec lenght i)
                     (let* ([current-value (if (= i lenght) #f (vector-ref vec i))])
                       (cond
                         [(= i lenght) #f]
                         [(not (pair? current-value)) (helper v vec lenght (+ i 1))]
                         [(= (car current-value) v) current-value]
                         [#t (helper v vec lenght (+ i 1))])))])
    (helper v vec (vector-length vec) 0)))

(define (cached-assoc xs n)
  (let* ([cache (make-vector n)]
         [cache-index 0])
    (lambda (v)
      (let* ([cached-value (assoc cache v)])
        (if cached-value
            cached-value
            (let* ([found-value (assoc xs v)])
              (if found-value
                  (begin
                    (vector-set! cache cache-index found-value)
                    (if (= cache-index (- n 1))
                        (set! cache-index 0)
                        (set! cache-index (+ cache-index 1)))
                    found-value)
                  #f)))))))

