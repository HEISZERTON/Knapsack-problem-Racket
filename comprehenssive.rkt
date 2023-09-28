
;Emmitt Heisz 30016658
#lang racket

;solves problem by checking all posible solutions
(define (knapsack maxwt itemL)
  (if (null? itemL)'(0)
      (if ( < maxwt (list-ref (car itemL) 2)) (knapsack maxwt (cdr itemL))
          (returnlarger maxwt itemL)
      )
   )
  
)

;append 2 lists
(define (append-list L1 L2)(if (null? L1)L2(cons (car L1) (append-list (cdr L1) L2))))

;return larger bag
(define (max k1 k2)
  (if ( > (car k1) (car k2)) k1 k2))

;add item to bag
(define (add k i)
  (define v (+ (list-ref i 1) (car k)))
  (define l (append-list (cdr k) (list-ref i 0) ))
  (cons v (list l))
  )

;check bag solution with and without first item and return the larger of the 2
(define (returnlarger maxwt itemL)
  (define item (car itemL))
  (define k1(knapsack maxwt (cdr itemL)))
  (define k2(knapsack (- maxwt (list-ref item 2)) (cdr itemL)))
  (max k1 (add k2 item))
  )


;solves the knapsackproblem given 
(define (solveknapsack name)
  (define i (open-input-file name))
  (define n (read i))
  (define itemslist (getitems n i))
  (knapsack (read i) itemslist)
)

;gets a list of item from the file
(define (getitems n i)
  (define item (list (read i)(read i)(read i)))
  (if (> n 1)
  (append-list (list item) (getitems (- n 1) i))
  (list item)
  )
)
