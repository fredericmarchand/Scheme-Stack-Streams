#lang racket

;Frederic Marchand
;ID#100817579
;Comp3007A3

;Question2

;;make-stack creates a stack
(define (make-stack . arg) 
  
  (define create-stack
    (lambda (stack)
      (lambda (command . args)
        (case command
          ((empty?) (null? stack));;returns true if the stack is empty and false if not
          ((top) (if (null? stack) 'empty (car stack)));returns the element at the top of the stack
          ((push!) (set! stack (cons (car args) stack));adds an element to the top of the stack and prints it
                   (car stack))
          ((pop!) (if (not (null? stack))(let ((value (car stack)));removes the top element in the stack and returns it, if the stack is empty an "empty" message is returned
                    (set! stack (cdr stack))
                    value) 'empty))
          ((print) stack)))));prints the stack
  
  (create-stack '()))

;testing
;;create 2 stacks
(define stack1 (make-stack))
(define stack2 (make-stack))
(display "Call: stack1 -> ")stack1
(display "Call: stack2 -> ")stack2
(newline)(newline)

;printing new empty stack
(display "Call: (stack1 'print) -> ")(stack1 'print)
(display "Should output: '()")(newline)(newline)

;checking top of empty stack
(display "Call: (stack1 'top) -> ")(stack1 'top)
(display "Should output: 'empty")(newline)(newline)

;printing new empty stack
(display "Call: (stack2 'print) -> ")(stack2 'print)
(display "Should output: '()")(newline)(newline)

;checking top of empty stack
(display "Call: (stack2 'top) -> ")(stack2 'top)
(display "Should output: 'empty")(newline)(newline)

;poping from empty stack
(display "Call: (stack2 'pop!) -> ")(stack2 'pop!)
(display "Should output: 'empty")(newline)(newline)

;pushing 5 into stack
(display "Call: (stack1 'push! 5) -> ")(stack1 'push! 5)
(display "Should output: 5")(newline)(newline)

;printing stack
(display "Call: (stack1 'print) -> ")(stack1 'print)
(display "Should output: '(5)")(newline)(newline)

;pushing into 1 stack what is popped from another
(display "Call: (stack2 'push! (stack1 'pop!)) -> ")(stack2 'push! (stack1 'pop!))
(display "Should output: 5")(newline)(newline)

;printing stack1
(display "Call: (stack1 'print) -> ")(stack1 'print)
(display "Should output: '()")(newline)(newline)

;printing stack2
(display "Call: (stack2 'print) -> ")(stack2 'print)
(display "Should output: '(5)")(newline)(newline)

;pushing 5 into stack1
(display "Call: (stack1 'push! 5) -> ")(stack1 'push! 5)
(display "Should output: 5")(newline)(newline)

;pushing 6 into stack1
(display "Call: (stack1 'push! 6) -> ")(stack1 'push! 6)
(display "Should output: 6")(newline)(newline)

;pushing 7 into stack1
(display "Call: (stack1 'push! 7) -> ")(stack1 'push! 7)
(display "Should output: 7")(newline)(newline)

;pushing 8 into stack1
(display "Call: (stack1 'push! 8) -> ")(stack1 'push! 8)
(display "Should output: 8")(newline)(newline)

;pushing 34 into stack1
(display "Call: (stack1 'push! 34) -> ")(stack1 'push! 34)
(display "Should output: 34")(newline)(newline)

;printing stack1
(display "Call: (stack1 'print) -> ")(stack1 'print)
(display "Should output: '(34 8 7 6 5)")(newline)(newline)



;reverse list procedure takes a list and returns it reversed
;it uses two stacks to reverse it
(define (reverse-list lst)
  
  ;first stack
  (define stack (make-stack))
  ;second stack
  (define other-stack (make-stack))
  
  ;;pushes all elements of a list into a stack
  (define (push-all lst stk)
    (cond ((not(null? lst))
          (stk 'push! (car lst))
          (push-all (cdr lst) stk))))
  
  ;;pops all the elements off a stack and pushes them into another stack
  (define (push-all2 stk1 stk2)
    (cond ((not (stk1 'empty?))
          (stk2 'push! (stk1 'pop!))
          (push-all2 stk1 stk2))))
  
  ;;displatch procedure for reversing the list by using stack1 and stack2
  (define (activate)
    (push-all lst stack)
    (push-all2 stack other-stack)
    (push-all2 other-stack stack)
    (stack 'print))
  
  (activate))

;;test cases
(display "Testing list reversal") (newline)(newline)

;reverse a list with 5 elements
(display "Call: (reverse-list '(6 5 4 3 2)) -> ")(reverse-list '(6 5 4 3 2))
(display "Should output: '(2 3 4 5 6)")(newline)(newline)

;reverse a list with 6 elements
(display "Call: (reverse-list '(6 5 4 3 2 1)) -> ")(reverse-list '(6 5 4 3 2 1))
(display "Should output: '(1 2 3 4 5 6)")(newline)(newline)

;reverse a list with 1 element
(display "Call: (reverse-list '(2)) -> ")(reverse-list '(2))
(display "Should output: '(2)")(newline)(newline)

;reverse a list with 8 elements
(display "Call: (reverse-list '(1 6 8 7 21 9 5 3)) -> ")(reverse-list '(1 6 8 7 21 9 5 3))
(display "Should output: '(3 5 9 21 7 8 6 1)")(newline)(newline)

;reverse an empty list
(display "Call: (reverse-list '()) -> ")(reverse-list '())
(display "Should output: '()")(newline)(newline)
