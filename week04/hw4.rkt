
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

;1 Write a function sequence that takes 3 arguments low, high, and stride, all assumed to be numbers. Further assume stride is positive. sequence produces a list of numbers from low to high (including low and possibly high) separated by stride and in sorted order

(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))
  
;2 Write a function string-append-map that takes a list of strings xs and a string suffix and returns a list of strings. Each element of the output should be the corresponding element of the input appended with suffix (with no extra space between the element and suffix). You must use Racket-library functions map and string-append. Sample solution: 2 lines.

(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

;3 Write a function list-nth-mod that takes a list xs and a number n. If the number is negative, terminate the computation with (error "list-nth-mod: negative number"). Else if the list is empty, terminate the computation with (error "list-nth-mod: empty list"). Else return the ith element of the list where we count from zero and i is the remainder produced when dividing n by the list’s length. Library functions length, remainder, car, and list-tail are all useful – see the Racket documentation. Sample solution is 6 lines.

(define (list-nth-mod xs n)
  (if (< n 0)
    (error "list-nth-mod: negative number")
     (if (null? xs)
       (error "list-nth-mod: empty list")
       (car (list-tail xs (remainder n (length xs)))))))

;4 Write a function stream-for-n-steps that takes a stream s and a number n. It returns a list holding the first n values produced by s in order. Assume n is non-negative. Sample solution: 5 lines. Note: You can test your streams with this function instead of the graphics code.

(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (let ([pr (s)])
      (cons (car pr) (stream-for-n-steps (cdr pr) (- n 1))))))

;5 Write a stream funny-number-stream that is like the stream of natural numbers (i.e., 1, 2, 3, ...) except numbers divisble by 5 are negated (i.e., 1, 2, 3, 4, -5, 6, 7, 8, 9, -10, 11, ...). Remember a stream is a thunk that when called produces a pair. Here the car of the pair will be a number and the cdr will be another stream.

(define funny-number-stream
  (letrec ([f (lambda (x) (cons (if (= (remainder x 5) 0) (* -1 x) x) (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))

;6 Write a stream dan-then-dog, where the elements of the stream alternate between the strings "dan.jpg" and "dog.jpg" (starting with "dan.jpg"). More specifically, dan-then-dog should be a thunk that when called produces a pair of "dan.jpg" and a thunk that when called produces a pair of "dog.jpg" and a thunk that when called... etc. Sample solution: 4 lines.

(define dan-then-dog 
  (lambda () (cons "dan.jpg" (lambda () (cons "dog.jpg" dan-then-dog)))))

;7 Write a function stream-add-zero that takes a stream s and returns another stream. If s would produce v for its ith element, then (stream-add-zero s) would produce the pair (0 . v) for its ith element. Sample solution: 4 lines. Hint: Use a thunk that when called uses s and recursion. Note: One of the provided tests in the file using graphics uses (stream-add-zero dan-then-dog) with place-repeatedly.

;(check-equal? (stream-for-n-steps (stream-add-zero ones) 1) (list (cons 0 1)) "stream-add-zero test")

(define (stream-add-zero s)
    (let ([pr (s)])
      (lambda () (cons (cons 0 (car pr)) (stream-add-zero (cdr pr)))))) 

;8  Write a function cycle-lists that takes two lists xs and ys and returns a stream. The lists may or may not be the same length, but assume they are both non-empty. The elements produced by the stream are pairs where the first part is from xs and the second part is from ys. The stream cycles forever through the lists. For example, if xs is ’(1 2 3) and ys is ’("a" "b"), then the stream would produce, (1 . "a"), (2 . "b"), (3 . "a"), (1 . "b"), (2 . "a"), (3 . "b"), (1 . "a"), (2 . "b"), etc. Sample solution is 6 lines and is more complicated than the previous stream problems. Hints: Use one of the functions you wrote earlier. Use a recursive helper function that takes a number n and calls itself with (+ n 1) inside a thunk.

(define (cycle-lists xs ys)
  (define (cycle-lists-helper xxs yys)
    (lambda () (cons (cons (car xxs) (car yys)) (cycle-lists-helper (if (null? (cdr xxs)) xs (cdr xxs)) (if (null? (cdr yys)) ys (cdr yys))))))
  (cycle-lists-helper xs ys))
              
;9 Write a function vector-assoc that takes a value v and a vector vec. It should behave like Racket’s assoc library function except (1) it processes a vector (Racket’s name for an array) instead of a list, (2) it allows vector elements not to be pairs in which case it skips them, and (3) it always takes exactly two arguments. Process the vector elements in order starting from 0. You must use library functions vector-length, vector-ref, and equal?. Return #f if no vector element is a pair with a car field equal to v, else return the first pair with an equal car field. Sample solution is 9 lines, using one local recursive helper function.

(define (vector-assoc v vec)
  (define (vector-iterate i)
    (if (>= i (vector-length vec)) #f
    (let ([vr (vector-ref vec i)])
    (if (pair? vr) 
        (if (equal? (car vr) v) vr (vector-iterate (+ i 1)))
        (vector-iterate (+ 1 i))))))
  (vector-iterate 0))
              
;10  Write a function cached-assoc that takes a list xs and a number n and returns a function that takes one argument v and returns the same thing that (assoc v xs) would return. However, you should use an n-element cache of recent results to possibly make this function faster than just calling assoc (if xs is long and a few elements are returned often). The cache must be a Racket vector of length n that is created by the call to cached-assoc (use Racket library function vector or make-vector) and used-and-possibly-mutated each time the function returned by cached-assoc is called. Assume n is positive. 

;(assoc v lst) takes a list of pairs and locates the first element of lst whose car is equal to v according to is-equal?.  If such an element exists, the pair (i.e., an element of lst) is returned.  Otherwise, the result is #f

(define (cached-assoc xs n)
  (let ([the-cache (make-vector n #f)]
        [cache-index 0])
    (lambda (v) 
      (let ([ans (vector-assoc v the-cache)])
        (if ans
            ans
            (let ([new-ans (assoc v xs)])
              (if new-ans
                  (begin
                    (vector-set! the-cache cache-index new-ans)
                    (set! cache-index (if (= (vector-length the-cache) (+ 1 cache-index)) 0 (+ 1 cache-index)))
                    new-ans)
                  #f)))))))
              
              
              
              
              
              
              
              
              
              