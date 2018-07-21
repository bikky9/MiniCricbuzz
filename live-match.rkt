#lang racket
(require net/url)
(require (planet neil/html-parsing:3:0))
(define (helper s l c)
  (cond ((member s (flatten (car l))))


        (else (helper s (cdr l) (+ 1 c)))))
(define (getpos s l m)
  (cond ((not (list? (list-ref l (helper s l 0)))) (reverse (cons (helper s l 0) m)))
        (else (getpos s (list-ref l (helper s l 0)) (cons (helper s l 0) m)))))
(define (get l m)
  (cond ((equal? l #f) #f)
        ((null? (cdr m)) (if (< (car m) (length l)) (list-ref l (car m)) #f))
        (else (get (if (< (car m) (length l)) (list-ref l (car m)) #f) (cdr m)))))
(define (pr l)
  (reverse (cons (+ 1 (car (reverse l))) (cdr (reverse l)))))
(define (prm l)
  (reverse (cons (- (car (reverse l)) 1) (cdr (reverse l)))))

(define (show k l)
(define (showh k l m)
  (cond ((not (get k (pr l))) m)
         ((not (list? (get k l))) m)
        ((or (equal? (car (get k l)) 'b) (equal? (car (get k l)) '&)) (showh k (pr (pr l)) (string-append m
                                                                                                          (cadr (get k l)) (get k (pr l)))))
        (else (get k l))))
  (showh k l (get k (prm l))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(define k (html->xexp (port->string (get-pure-port
;     (string->url "")))))
;(define k
 ; (html->xexp (get-pure-port (string->url "http://www.cricbuzz.com/live-cricket-scores/20085/srh-vs-rheadbhfhucb"))))
(define 
(if (equal? (get k '(4 3 5 3 9 4 2 3 2 3 3 3 2)) #f) (begin (display (get k '(4 3 5 3 9 4 2 3 2 3 3 2))) (get k '(4 3 5 3 9 4 2 3 2 3 5 3 2))) (get k '(4 3 5 3 9 4 2 3 2 3 3 3 2)))
(if (equal? " " (get k '(4 3 5 3 9 4 7 3 3 2))) (get k '(4 3 5 3 9 4 7 3 3 3 2)) (car (cdr (get k '(4 3 5 3 9 4 7 3 3 2)))))
(if (equal? " " (get k '(4 3 5 3 9 4 7 3 3 2))) (show k '(4 3 5 3 9 4 7 3 5 3)) "")
(if (equal? " " (get k '(4 3 5 3 9 4 7 5 3 2))) (get k '(4 3 5 3 9 4 7 5 3 3 2)) (if (list? (get k '(4 3 5 3 9 4 7 5 3 2))) (car (cdr (get k '(4 3 5 3 9 4 7 5 3 2)))) (show k '(4 3 5 3 9 4 7 5 3 3))))
(if (equal? " " (get k '(4 3 5 3 9 4 7 5 3 2))) (show k '(4 3 5 3 9 4 7 5 5 3)) "")