#lang racket

;Frederic Marchand
;ID#100817579
;Comp3007A3

;Question 3

;define the empty stream
(define the-empty-stream '())

;procedure delay 
(define-syntax delay 
  (syntax-rules ()
    ((_ exp ...)
     (lambda () exp ...))))

;;procedure force
(define (force exp) (exp))

;;stream-cons
(define-syntax stream-cons
  (syntax-rules ()
    ((stream-cons x y) (cons x (delay y)))))

;;stream-car is essentially just calling car on a stream
(define (stream-car str) (car str))

;;stream-cdr forces the cdr of the stream
(define (stream-cdr str) (force (cdr str)))

;checks if a stream is null
(define (stream-null? str) (null? str))

;;returns the nth element in stream str
(define (stream-ref str n) 
  (if (= n 0) (stream-car str) 
      (stream-ref (stream-cdr str) (- n 1))))

;;filters out all elements of stream str that dont pass the predicate test
(define (stream-filter pred str) 
  (cond ((stream-null? str) the-empty-stream)
        ((pred (stream-car str))
         (stream-cons (stream-car str) (stream-filter pred (stream-cdr str))))
        (else (stream-filter pred (stream-cdr str)))))

;;this implementation is not required for the assignment but it is useful 
;;map procedure as in the course notes
(define (stream-map proc str)
  (if (stream-null? str) the-empty-stream
      (stream-cons (proc (stream-car str)) (stream-map proc (stream-cdr str)))))
        
;;performs procedure proc on each element of stream str
(define (stream-for-each proc str) 
  (if (stream-null? str) 'done
      (begin (proc (stream-car str))
             (stream-for-each proc (stream-cdr str)))))

;;creates a stream containing the first n elements from str
(define (first n str) 
  (if (= n 0) the-empty-stream
      (stream-cons (stream-car str) (first (- n 1) (stream-cdr str)))))

;;convert a list to a stream
(define (list->stream lis) 
  (define (helper lst str)
    (if (null? lst) str
        (helper (cdr lst) (stream-cons (car lst) str))))
    (helper (reverse lis) the-empty-stream))

;;convert a finite stream to a list
;the stream must be finite otherwise the procedure will go on forever
(define (stream->list str)
  (if (stream-null? str) '()
      (cons (stream-car str)
            (stream->list (stream-cdr str)))))

;testing
;delay (* 15 4)
(display "an-expression defined as (define an-expression (delay (* 15 4))) -> ") 
(define an-expression (delay (* 15 4)))
an-expression
(newline)

;compute (* 15 4) -> 60
(display "Call: (force an-expression) -> ")
(force an-expression)
(display "Expected Output: 60")(newline)(newline)


;testing stream-cons
(display "defining someth as (define someth (stream-cons 15 (* 5 7))) -> ")
(define someth (stream-cons 15 (* 5 7)))
someth
(newline)

;testing stream-car
(display "(stream-car someth) -> ")
(stream-car someth)
(display "Expected Output: 15")(newline)(newline)

;testing stream-cdr
(display "(stream-cdr someth) -> ")
(stream-cdr someth)
(display "Expected Output: 35")(newline)(newline)

;converts the list to a stream
(display "(list->stream '(10 8 6 4)) -> ")
(list->stream '(10 8 6 4))
(newline)

;stream->list, stream-for-each, first stream-ref and stream-filter tested below 


;returns whether n is a prime number or not
(define (prime? n)
  ;returns the square a number x
  (define (square x) (* x x))
  
  ;returns the smallest divisor of n
  (define (smallest-divisor n)
    (find-divisor n 2))
  
  ;finds divisors of n
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))
  
  ;checks if a divided by b has a remainder
  (define (divides? a b)
    (= (remainder b a) 0))
  
  (= n (smallest-divisor n)))

;test prime?
(display "(prime? 15) -> ")
(prime? 15)
(display "Expected Output: #f")(newline)(newline)

(display "(prime? 17) -> ")
(prime? 17)
(display "Expected Output: #t")(newline)(newline)


;random number functions as in the text book
(define (rand-update x) (+ 1 (random 100)))
(define (random-init) (rand-update 0))
(define rand
  (let ((x (random-init)))
    (lambda ()
      (set! x (rand-update x))
      x)))


;creates a stream of infinite integers starting from n that skip by 2 each time
(define (integers-starting-from-skip-two n)
   (stream-cons n (integers-starting-from-skip-two (+ n 2))))

;creates a stream of infinite integers starting from n that skip by 1 each time
(define (integers-starting-from n)
   (stream-cons n (integers-starting-from (+ n 1))))


;;the following tests ensure that the procedures first and stream->list and stream-filter function properly

;;(1) infinite stream of ones
(define infinite-ones (stream-cons 1 infinite-ones))
(display "(1) infinite ones")(newline)
(stream->list (first 25 infinite-ones))


;;(2)stream of infinite even integers
(define infinite-even-integers (integers-starting-from-skip-two 0))
(display "(2) infinite even integers")(newline)
(stream->list (first 25 infinite-even-integers))


;;(3)infinite stream of random numbers between 1 and 100
(define infinite-random-numbers 
  (stream-cons (random-init) 
               (stream-map rand-update infinite-random-numbers)))
(display "(3) infinite random numbers")(newline)
(stream->list (first 25 infinite-random-numbers))


;;(4)infinite list of all prime numbers
(define infinite-prime-numbers (stream-filter prime? (integers-starting-from 1)))
(display "(4) infinite prime numbers")(newline)
(stream->list (first 25 infinite-prime-numbers))

(newline)(newline)

;testing stream->list and first
;converts a stream to a list
(display "Call: (stream->list (first 25 infinite-random-numbers)) -> \n")
(stream->list (first 25 infinite-random-numbers))
(display "Expected Output: a list containing 25 random numbers")(newline)(newline)

;converts a stream to as list
(display "Call: (first 25 (stream-filter prime? infinite-random-numbers) -> \n")
(stream->list (first 25 (stream-filter prime? infinite-random-numbers)))
(display "Expected Output: a list containing 25 random numbers that are prime numbers only")(newline)(newline)


;testing stream-for-each
(define (display-stream s)
  (stream-for-each 
   (lambda (x) 
     (newline)
     (display x)) s))

(display "Call: (display-stream (first 10 infinite-even-integers)) -> ")
(display-stream (first 10 infinite-even-integers))
(display "Expected Output: the first 10 even integers printed on a new line each time")
(newline)(newline)

;testing stream-ref
(display "Call: (stream-ref infinite-even-integers 10000) -> \n")
(stream-ref infinite-even-integers 10000)
(display "Expected Output: 20000 because the 10000th even integer is 10000*2=20000")(newline)(newline)











