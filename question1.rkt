#lang racket

;Frederic Marchand
;ID#100817579
;Comp3007A3

;Question1

;;quicksort sorts a list of numbers in expected case O(nlogn), worst case O(n^2)
(define (quicksort lst)
  
  ;returns the pivot if the list is not null, the pivot is the first element in the list
  (define (get-pivot lst)
    (cond ((null? lst) '())
          ((null? (cdr lst)) '())
          (else (car lst))))
  
  ;;helper function partitions the list into the elements greater than the pivot and the elements less than the pivot
  (define (quick-help pivot lst lst-smaller lst-greater)
    (cond ((null? lst) (list lst-smaller lst-greater))
          ((< (car lst) pivot) (quick-help pivot (cdr lst) (cons (car lst) lst-smaller) lst-greater))
          ((>= (car lst) pivot) (quick-help pivot (cdr lst) lst-smaller (cons (car lst) lst-greater)))))
  
  ;;the sorting gets the pivot from the list and if its not null it pa
  (define (sort lst)
    (let ((pivot (get-pivot lst)))
      (if (null? pivot) lst 
          (let ((partitions (quick-help pivot lst '() '())))
            (append (quicksort (car partitions)) 
                    (quicksort (cadr partitions)))))))
  
  (sort lst))


;test cases
;already ordered
(display "Call: (quicksort '(1 2 3 4 5 6)) -> Output: ")
(quicksort '(1 2 3 4 5 6)) (display "Should output: '(1 2 3 4 5 6)")(newline)(newline)

;multiple items not ordered
(display "Call: (quicksort '(11 8 14 7 2 20 16)) -> Output: ")
(quicksort '(11 8 14 7 2 20 16)) (display "Should output: '(2 7 8 11 14 16 20)")(newline)(newline)

;one item
(display "Call: (quicksort '(11)) -> Output: ")
(quicksort '(11))(display "Should output: '(11)")(newline)(newline)

;two items not ordered
(display "Call: (quicksort '(11 8)) -> Output: ")
(quicksort '(11 8))(display "Should output: '(8 11)")(newline)(newline)

;two items already ordered
(display "Call: (quicksort '(8 9)) -> Output: ")
(quicksort '(8 9))(display "Should output: '(8 9)")(newline)(newline)

;empty list
(display "Call: (quicksort '()) -> Output: ")
(quicksort '())(display "Should output: '()")(newline)(newline)

;unordered list with many items
(display "Call: (quicksort '(11 8 14 7 2 20 16 55 80 1)) -> Output: ")
(quicksort '(11 8 14 7 2 20 16 55 80 1))(display "Should output: '(1 2 7 8 11 14 16 20 55 80)")(newline)(newline)


