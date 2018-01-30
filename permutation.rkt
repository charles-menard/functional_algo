;;return the seq with only the items that satisfy the predicate
(define (filter pred seq)
  (cond ((null? seq) nil)
        ((pred (car seq))
         (cons (car seq)
               (filter pred (cdr seq))))
        (else (filter pred (cdr seq)))))

(define (remove item seq)
  (filter (lambda (x) (not (= x item))) seq))
;; apply op to the sequence beginning with initial and going right
(define (accumulate op initial seq)
  (if (null? seq)
      initial
      (op (car seq)
          (accumulate op initial (cdr seq)))))
;; append seq2 to seq1
(define (append seq1 seq2)
  (if (null? seq1)
      seq2
    (cons (car seq1) (append (cdr seq1) seq2))))
    
;; apply procedure proc to each list within the list seq and flatten it in one list

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

;;returns permutations of the elements of the list seq

(define (permutations seq)
  (if (null? seq)
      (list nil)
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x seq))))
               seq)))

(define (forEach proc seq)
    (if (null? seq)
        nil
        (begin (proc (car seq))
               (forEach proc (cdr seq)))))
;; return the a function that returns the list of natural from 1 to n
(define iota
  (lambda (y)
    (let loop ((n 1))
      (if (<= n y)
          (cons n (loop (+ n 1)))
          '()))))

(for-each (lambda (x) (display x) (newline)) (permutations (iota 3)))
