#lang racket/gui
(require net/url)
(require (planet neil/html-parsing:3:0))
(require table-panel)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;definitions of helper functions;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (hello l) (list (list->string (takewhile (lambda (x) (not (equal? #\space x))) l)) (list->string (takewhile (lambda (y) (not (equal? #\space y)))  (cddddr (dropwhile (lambda (x) (not (equal? #\space x))) l))))))
(define (scan a) (cond [(equal? "CSK" a) (read-bitmap (get-pure-port (string->url "file:///home/nikhil/CSK2.png")) #:backing-scale 3)]
                       [(equal? "MI" a) (read-bitmap (get-pure-port (string->url "file:///home/nikhil/MI2.png"))  #:backing-scale 1)]
                       [(equal? "RR" a) (read-bitmap (get-pure-port (string->url "file:///home/nikhil/RR2.png"))  #:backing-scale 5)]
                       [(equal? "DD" a) (read-bitmap (get-pure-port (string->url "file:///home/nikhil/DD2.png"))  #:backing-scale 0.8)]
                       [(equal? "KXIP" a) (read-bitmap (get-pure-port (string->url "file:///home/nikhil/KXIP2.png"))  #:backing-scale 8)]
                       [(equal? "SRH" a) (read-bitmap (get-pure-port (string->url "file:///home/nikhil/SRH2.png"))  #:backing-scale 1)]
                       [(equal? "KKR" a) (read-bitmap (get-pure-port (string->url "file:///home/nikhil/KKR2.png"))  #:backing-scale 5)]
                       [(equal? "RCB" a) (read-bitmap (get-pure-port (string->url "file:///home/nikhil/RCB2.png")) #:backing-scale 4)]))
(define-syntax while
  (syntax-rules (:)
    ((while : pred? : stmt ...)
     (do () ((not pred?))
       stmt ...))))
(define (correct-substring s) (if (equal? (string-ref s 16) #\2) (string-append "http://www.cricbuzz.com/live-cricket-scorecard/" (substring s 16 30))
                                                                 (string-append "http://www.cricbuzz.com/live-cricket-scorecard/" (substring s 21 35))))
(define (helper s l c)
  (cond ((member s (flatten (car l))) c)
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
    (cond  ;((not (string? (get k (pr l)))) (string-append m (cadr (get k l)))) 
      ((not (list? (get k (pr l)))) m)
          ((equal? (car (get k l)) 'b) (showh k (pr (pr l)) (string-append m
                                                                 (cadr (get k l)) (get k (pr l)))))
          (else (get k l))))
  (showh k l (get k (prm l))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Past-Match GUI;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define k (html->xexp (get-pure-port (string->url "http://www.cricbuzz.com/cricket-series/2676/indian-premier-league-2018/matches"))))
(define x 8)
(define y 1)
(while : (not (equal? (get k (list 4 3 5 3 10 2 x 4 2 4 2)) #f)) :
       (set! y (+ y 1))
       (set! x (+ x 1)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define new-font (make-object font%  10 'default 'normal 'bold #f 'default #f 'aligned))
(define new-font1 (make-object font%  13 'default 'normal 'bold #f 'default #f 'aligned))
(define frame (new frame% (label "Mini-Cricbuzz")
                   ))
(define live-match (new group-box-panel% [parent frame] [label "Live-Match"] [border 10]))
(define k1 (html->xexp (get-pure-port (string->url "http://www.cricbuzz.com"))))
(new message% [label (get k1 '(4 3 5 3 5 2 3 2 2 2 2 1 1 1))] [parent live-match])
(new button% [label "Go Live"] [parent live-match]
     [callback (lambda (button event) (live-match-final (string-append "http://www.cricbuzz.com" (get k1 '(4 3 5 3 5 2 3 2 2 2 2 1 2 1)))))])
(new button% [label "Statistics"] [parent live-match]
     [callback (lambda (button event) (Tour))])
(define frame-panel (new vertical-panel% [parent frame]
                         [alignment '(left top)]
                         [style (list 'auto-vscroll)]
                         [min-height 525]
                         [border 10]))
(define Main-logo (read-bitmap (get-pure-port (string->url "file:///home/nikhil/Pictures/Screenshot%20from%202018-04-19%2004-13-16.png"))))
;(new message% [label Main-logo] [parent frame-panel])
(define number-of-matches (- y 1))
(define table (new table-panel% [parent frame-panel]
                   [alignment '(left top)]
                   [dimensions (list (+ (quotient number-of-matches 2) 1) 2)]
                   [column-stretchability #f]
                   [row-stretchability #f]
                   [spacing 5]))
(set! x 8)
(while : (not (equal? (get k (list 4 3 5 3 10 2 x 4 2 4 2)) #f)) :
       (let* ([cell (new group-box-panel% [parent table]
                         [label (string-append "Match No." (number->string (- x 7)))]
                         [font new-font1])]
              [y x])
         (new message% [parent cell] [label (get k (list 4 3 5 3 10 2 x 4 2 2 2 1))])
         ;(new message% [parent cell] [label "VS"])
         ;(new message% [parent cell] [label (get k (list 4 3 5 3 10 2 3 5 (- (* 2 x) 13) 2))])
         (new message% [parent cell] [label (get k (list 4 3 5 3 10 2 x 4 2 4 2))])
         (new button% [parent cell] [label "Scorecard"]
              [callback (lambda (button event) (past-match (correct-substring (get k (list 4 3 5 3 10 2 y 4 2 2 1 2 1))) #t))]))
       (set! x (+ x 1)))
(send frame show #t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (skip l k) (cond [(= k 0) l]
                         [else (skip (cdr l) (- k 1))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define tournment%
  (class object%
    (super-new)
    (define scrap (make-object player% 0 0 0 0 0 0 0 0 0 0 0 0 100))
    (define l '())
    (define big-l '())
    (define orange-cap scrap)
    (define purple-cap scrap)
    (define most-sixes scrap)
    (define most-fours scrap)
    (define best-SR scrap)
    (define best-ER scrap)
    (define/public (get-orange-cap) orange-cap)
    (define/public (get-purple-cap) purple-cap)
    (define/public (get-most-sixes) most-sixes)
    (define/public (get-most-fours) most-fours)
    (define/public (get-best-SR) best-SR)
    (define/public (get-best-ER) best-ER)
    
    (define/public (f l2)
      (begin
        (define (f2 l1)
      (cond [(not (null? l1))
             (let* ((x (car l1))
                    (z (findf (lambda (y) (string-equal? (get-field  name y) (get-field name x))) big-l)))
               (if (equal? z #f) (begin (set! l (cons x l)) (f2 (cdr l1)))
                   (begin
                     (set! big-l (remove z big-l))
                          (modify z x)
                     (set! l (cons z l))
                     
                     (f2 (cdr l1)))))]))
        (f2 l2)
      (modify-oc l)
      (modify-pc l)
      (modify-ms l)
      (modify-mf l)
      (modify-bSR l)
      (modify-bER l)
      (set! big-l (append l big-l))
      (set! l '())))

    (define (modify-oc l)
      (cond [(not (null? l))
             (let* ((x (car l))
                    (y (get-field runs orange-cap))
                    (xr (get-field runs x)))
             (cond [(< y xr) (begin (set! orange-cap x) (modify-oc (cdr l)))]
                   [(= y xr) (begin (cond [(> (get-field SR x) (get-field SR orange-cap))
                                                (set! orange-cap x)])
                                    (modify-oc (cdr l)))]
                   [else (modify-oc (cdr l))]))]))

   (define (modify-pc l)
      (cond [(not (null? l))
             (let* ((x (car l))
                    (scrapr (get-field wickets purple-cap))
                    (xr (get-field wickets x)))
             (cond [(< scrapr xr) (begin (set! purple-cap x) (modify-pc (cdr l)))]
                   [(= scrapr xr) (begin (cond [(< (get-field ER x) (get-field ER purple-cap))
                                                (set! purple-cap x)])
                                         (modify-pc (cdr l)))]
                   [else (modify-pc (cdr l))]))]))

    (define (modify-ms l)
      (cond [(not (null? l))
             (let* ((x (car l))
                    (scrapr (get-field sixes most-sixes))
                    (xr (get-field sixes x)))
             (cond [(< scrapr xr) (begin (set! most-sixes x) (modify-ms (cdr l)))]
                   [(= scrapr xr) (begin (cond [(> (get-field runs x) (get-field runs most-sixes))
                                                (set! most-sixes x)])
                                         (modify-ms (cdr l)))]
                   [else (modify-ms (cdr l))]))]))

    (define (modify-mf l)
      (cond [(not (null? l))
             (let* ((x (car l))
                    (scrapr (get-field fours most-fours))
                    (xr (get-field fours x)))
             (cond [(< scrapr xr) (begin (set! most-fours x) (modify-mf (cdr l)))]
                   [(= scrapr xr) (begin (cond [(> (get-field runs x) (get-field runs most-fours))
                                                (set! most-fours x)])
                                         (modify-mf (cdr l)))]
                   [else (modify-mf (cdr l))]))]))

    (define (modify-bSR l)
      (cond [(not (null? l))
             (let* ((x (car l))
                    (scrapr (get-field SR best-SR))
                    (xr (get-field SR x)))
             (cond [(< scrapr xr) (begin (cond [(> (get-field balls x) 20) (set! best-SR x)])
                                                        (modify-bSR (cdr l)))]
                   [(= scrapr xr) (begin (cond [(> (get-field runs x) (get-field runs best-SR))
                                                (cond [(> (get-field balls x) 20) (set! best-SR x)])])
                                         (modify-bSR (cdr l)))]
                   [else (modify-bSR (cdr l))]))]))

    (define (modify-bER l)
      (cond [(not (null? l))
             (let* ((x (car l))
                    (scrapr (get-field ER best-ER))
                    (xr (get-field ER x)))
             (cond [(> scrapr xr) (begin (cond [(> (get-field balls-bowled x) 18) (set! best-ER x)])
                                         (modify-bER (cdr l)))]
                   [(= scrapr xr) (begin (cond [(> (get-field wickets x) (get-field wickets best-ER))
                                                (cond [(> (get-field balls-bowled x) 18) (set! best-ER x)])])
                                         (modify-bER (cdr l)))]
                   [else (modify-bER (cdr l))]))]))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (takewhile p l)
  (cond [(or (null? l) (not (p (car l)))) '()]
        [else (cons (car l) (takewhile p (cdr l)))]))
(define (dropwhile p l)
  (cond [(or (null? l) (not (p (car l)))) l]
        [else (dropwhile p (cdr l))]))
(define (string-equal? a b) (let* ([s1 (string->list a)]
                                   [s2 (string->list b)])
                              (cond [(equal? s1 s2) #t]
                                    [(equal? s1 (cdr (reverse (cdr (reverse s2))))) #t]
                                    [(equal? s2 (cdr (reverse (cdr (reverse s1))))) #t]
                                    [else #f])))
(define (string->integer s) (string->number s))
  ;(define p (/ 1 10))
  ;(define (f x a) (begin (set! p (* 10 p)) (+ a (* x p))))
  ;(let* ([l (string->list s)])
   ; (foldr (lambda (x ans) (f x ans)) 0 (map (lambda (x) (- (char->integer x) 48)) l))))
;(define (string->integer s) (- (char->integer (string->list s))) 48))
(define (modify z x)
      (begin
       (send z set-runs (get-field runs x))
       (send z set-balls (get-field balls x))
       (send z set-sixes (get-field sixes x))
       (send z set-fours (get-field fours x))
       (send z set-balls-bowled (get-field balls-bowled x))
       (send z set-maidens (get-field maidens x))
       (send z set-runs-given (get-field runs-given x))
       (send z set-wickets (get-field wickets x))
       (send z set-SR)
       (send z set-ER)
       (send z set-NB (get-field NB x))))
(define player%
  (class object%
    (super-new)
    (init-field name)
    (init-field runs)
    (init-field balls)
    (init-field fours)
    (init-field sixes)
    (init-field SR)
    (init-field balls-bowled)
    (init-field maidens)
    (init-field runs-given)
    (init-field wickets)
    (init-field NB)
    (init-field WD)
    (init-field ER)
    (define/public (set-runs a) (set! runs (+ runs a)))
    (define/public (set-balls a) (set! balls (+ balls a)))
    (define/public (set-fours a) (set! fours (+ fours a)))
    (define/public (set-sixes a) (set! sixes (+ sixes a)))
    (define/public (set-balls-bowled a) (set! balls-bowled (+ balls-bowled a)))
    (define/public (set-maidens a) (set! maidens (+ maidens a)))
    (define/public (set-runs-given a) (set! runs-given (+ runs-given a)))
    (define/public (set-wickets a) (set! wickets (+ wickets a)))
    (define/public (set-NB a) (set! NB (+ NB a)))
    (define/public (set-WD a) (set! WD (+ WD a)))
    (define/public (set-SR) (set! SR (if (not (equal? balls 0)) (exact->inexact (/ (floor (* (/ runs balls) 10000)) 100)) 0)))
    (define/public (set-ER) (set! ER (if (not (equal? balls-bowled 0)) (exact->inexact (/
                                                                   (floor (* (/ runs-given balls-bowled) 600)) 100)) 0)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (past-match url bool)
  (define x 7)
  (define l '())
  (define list-pavan '())
  (begin (define k (html->xexp (port->string (get-pure-port (string->url url)))))
         (while : (not (equal? (get k (list 4 3 5 3 9 3 3 3 x 3 3 2)) #f)) :
                (set! l (cons (make-object player%
                                (get k (list 4 3 5 3 9 3 3 3 x 3 3 2))
                                (string->integer (get k (list 4 3 5 3 9 3 3 3 x 7 2)))
                                (string->integer (get k (list 4 3 5 3 9 3 3 3 x 9 2)))
                                (string->integer (get k (list 4 3 5 3 9 3 3 3 x 11 2)))
                                (string->integer (get k (list 4 3 5 3 9 3 3 3 x 13 2)))
                                0
                                0
                                0
                                0
                                0
                                0
                                0
                                0) l))
                (begin (set! list-pavan l) (set! x (+ x 2))))
         (define x1 (/ (- x 7) 2))
         ;(display x)
         (define y 3)
         ;(trace y)
         (while : (not (equal? (get k (list 4 3 5 3 9 3 3 3 (+ x 4) 5 y 2)) #f)) :
                (set! l (cons (make-object player%
                                (get k (list 4 3 5 3 9 3 3 3 (+ x 4) 5 y 2))
                                0 0 0 0 0 0 0 0 0 0 0 0) l))
                (begin (set! list-pavan l) (set! y (+ y 2))))
         (define y1 (/ (- y 3) 2))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         (set! x 5)
         (while : (not (equal? (get k (list 4 3 5 3 9 3 3 8 x 3 3 2)) #f)) :
                (let* ((hello (make-object player%
                                (get k (list 4 3 5 3 9 3 3 8 x 3 3 2))
                                0
                                0
                                0
                                0
                                0
                                (- (* 10 (string->integer (get k (list 4 3 5 3 9 3 3 8 x 5 2)))) (* 4 (floor (string->integer (get k (list 4 3 5 3 9 3 3 8 x 5 2)))))) 
                                (string->integer (get k (list 4 3 5 3 9 3 3 8 x 7 2)))
                                (string->integer (get k (list 4 3 5 3 9 3 3 8 x 9 2)))
                                (string->integer (get k (list 4 3 5 3 9 3 3 8 x 11 2)))
                                (string->integer (get k (list 4 3 5 3 9 3 3 8 x 13 2)))
                                (string->integer (get k (list 4 3 5 3 9 3 3 8 x 15 2)))
                                (string->integer (get k (list 4 3 5 3 9 3 3 8 x 17 2)))))
                       (z (findf (lambda (y) (string-equal? (get-field  name y) (get-field name hello))) l)))
                  (if (equal? z #f) (begin (set! l (cons hello l)) (set! list-pavan (cons hello list-pavan)))
                      (begin ;(set! l (remove z l))
                        (set! list-pavan (remove z list-pavan))
                        (modify z hello)
                        (set! list-pavan (cons z list-pavan))
                        (set! l (append (takewhile (lambda (x) (string-equal? (get-field name x) (get-field name z))) l)
                                        (list z)
                                        (dropwhile (lambda (x) (string-equal? (get-field name x) (get-field name z))) l)))))
                (set! x (+ x 2))))
         (define z1 (/ (- x 5) 2))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         (set! x 7)
         (while : (not (equal? (get k (list 4 3 5 3 9 3 4 3 x 3 3 2)) #f)) :
                (let* ((hello (make-object player%
                                (get k (list 4 3 5 3 9 3 4 3 x 3 3 2))
                                (string->integer (get k (list 4 3 5 3 9 3 4 3 x 7 2)))
                                (string->integer (get k (list 4 3 5 3 9 3 4 3 x 9 2)))
                                (string->integer (get k (list 4 3 5 3 9 3 4 3 x 11 2)))
                                (string->integer (get k (list 4 3 5 3 9 3 4 3 x 13 2)))
                                0
                                0
                                0
                                0
                                0
                                0
                                0
                                0))
                       (z (findf (lambda (y) (string-equal? (get-field name y) (get-field name hello))) l)))
                  (if (equal? z #f) (begin (set! l (cons hello l)) (set! list-pavan (cons hello list-pavan)))
                      (begin
                        (set! list-pavan (remove z list-pavan))
                        (modify z hello)
                        (set! list-pavan (cons z list-pavan))
                        (set! l (append (takewhile (lambda (x) (string-equal? (get-field name x) (get-field name z))) l)
                                        (list z)
                                        (dropwhile (lambda (x) (string-equal? (get-field name x) (get-field name z))) l))))))
                (set! x (+ x 2)))
         (define x2 (/ (- x 7) 2))
         ;(display x)
         (set! y 3)
         (while : (not (equal? (get k (list 4 3 5 3 9 3 4 3 (+ x 4) 5 y 2)) #f)) :
                (let* ((x (make-object player%
                            (get k (list 4 3 5 3 9 3 4 3 (+ x 4) 5 y 2))
                            0 0 0 0 0 0 0 0 0 0 0 0))
                       (z (findf (lambda (y) (string-equal? (get-field  name y) (get-field name x))) l)))
                  (if (equal? z #f) (begin (set! l (cons x l)) (set! list-pavan (cons x list-pavan)))
                      (begin (set! list-pavan (remove z list-pavan))
                             (begin (modify z x)
                                    (set! list-pavan (cons z list-pavan))
                                    (set! l (append (takewhile (lambda (x) (string-equal? (get-field name x) (get-field name z))) l)
                                                    (list z)
                                                    (dropwhile (lambda (x) (string-equal? (get-field name x) (get-field name z))) l))))))) 
                (set! y (+ y 2)))
         (define y2 (/ (- y 3) 2))
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;bowling scorecard;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         (set! x 5)
         (while : (not (equal? (get k (list 4 3 5 3 9 3 4 8 x 3 3 2)) #f)) :
                (let* ((x (make-object player%
                            (get k (list 4 3 5 3 9 3 4 8 x 3 3 2))
                            0
                            0
                            0
                            0
                            0
                            (- (* 10 (string->integer (get k (list 4 3 5 3 9 3 4 8 x 5 2)))) (* 4 (floor (string->integer (get k (list 4 3 5 3 9 3 4 8 x 5 2)))))) 
                            (string->integer (get k (list 4 3 5 3 9 3 4 8 x 7 2)))
                            (string->integer (get k (list 4 3 5 3 9 3 4 8 x 9 2)))
                            (string->integer (get k (list 4 3 5 3 9 3 4 8 x 11 2)))
                            (string->integer (get k (list 4 3 5 3 9 3 4 8 x 13 2)))
                            (string->integer (get k (list 4 3 5 3 9 3 4 8 x 15 2)))
                            (string->integer (get k (list 4 3 5 3 9 3 4 8 x 17 2)))))
                       (z (findf (lambda (y) (string-equal? (get-field  name y) (get-field name x))) l)))
                  (if (equal? z #f) (begin (set! l (cons x l)) (set! list-pavan (cons x list-pavan)))
                      (begin (set! list-pavan (remove z list-pavan))
                             (modify z x)
                             (set! list-pavan (cons z list-pavan))
                             (set! l (append (takewhile (lambda (x) (string-equal? (get-field name x) (get-field name z))) l)
                                             (list z)
                                             (dropwhile (lambda (x) (string-equal? (get-field name x) (get-field name z))) l))))))
                (set! x (+ x 2)))
         (define z2 (/ (- x 5) 2))
         (define bin (map (lambda (y) (send y set-SR)) l))
         (define bin2 (map (lambda (y) (send y set-ER)) l))
         ;(define (set-list l) (cond [(null? (cdr l)) (car l)]
          ;                          [else (begin (set! l (cdr l)) (car l))]))
         ;(define k (html->xexp (port->string (get-pure-port (string->url url)))))
         ;(define Extras1 (get k '(4 3 5 3 9 3 3 3 25 5 2)))
         ;(define Extras-types1 (get k '(4 3 5 3 9 3 3 3 25 7 3)))
         ;(define Extras2 (get k '(4 3 5 3 9 3 4 3 25 5 2)))
         ;(define Extras-types2 (get k '(4 3 5 3 9 3 4 3 25 7 3)))
         (define dup (reverse l))
         (define scorecard (new frame% [label (get k '(4 2 22 1 2 1))]
                                [border 10]
                                [spacing 3]))
         (new message% [parent scorecard] [label (get k '(4 3 5 3 9 3 2 2))])
         (define Teams (new horizontal-pane% [parent scorecard] [spacing 10] [border 10]))
         (define total1 (string-append (get k '(4 3 5 3 9 3 3 3 3 5 2)) (get k '(4 3 5 3 9 3 3 3 3 5 4))))
         (define total2 (string-append (get k '(4 3 5 3 9 3 4 3 3 5 2)) (get k '(4 3 5 3 9 3 4 3 3 5 4))))
         (define Innings1 (new group-box-panel% [parent Teams] [label (get k '(4 3 5 3 9 3 3 3 3 3 1))] [border 10] [font new-font1]))
         (define Innings2 (new group-box-panel% [parent Teams] [label (get k '(4 3 5 3 9 3 4 3 3 3 1))] [border 10] [font new-font1]))
         (define Table1 (new table-panel% [parent Innings1]
                             [alignment '(left top)]
                             [dimensions (list (+ x1 2) 6)]
                             [column-stretchability #f]
                             [row-stretchability #f]
                             [spacing 10]))
         (new message% [parent Table1] [label "Batsman"] [font new-font])
         (new message% [parent Table1] [label "Runs"] [font new-font])
         (new message% [parent Table1] [label "Balls"] [font new-font])
         (new message% [parent Table1] [label "4s"] [font new-font])
         (new message% [parent Table1] [label "6s"] [font new-font])
         (new message% [parent Table1] [label "SR"] [font new-font])
         (for ((i (in-range x1)))
           (let* (
                  [temp (car dup)])
             (new message% [parent Table1]
                  [label (get-field name temp)])
             (new message% [parent Table1]
                  [label (number->string (get-field runs temp))])
             (new message% [parent Table1]
                  [label (number->string (get-field balls temp))])
             (new message% [parent Table1]
                  [label (number->string (get-field fours temp))])
             (new message% [parent Table1]
                  [label (number->string (get-field sixes temp))])
             (new message% [parent Table1]
                  [label (number->string (get-field SR temp))])
             (set! dup (cdr dup))))
         ;(define Extras-table (new table-panel% [parent Table1]
                                ;[alignment '(left top)]
                                ;[dimensions (list 1 2)])
         ;(new message% [parent Extras-table] [label "Extras"])
         ;(new message% [parent Extras-table] [label Extras1])
         (define Total (new table-panel% [parent Table1]
                         [alignment '(left top)]
                         [dimensions (list 1 2)]
                         [column-stretchability #f]
                         [row-stretchability #f]))
         (new message% [parent Total] [label "                                                           Total"])
         (new message% [parent Total] [label total1])
         (set! dup (skip dup y1))
         ;(new message% [parent Innings1] [label "Did not Bat"])
         ;(for ((k (in-range y1)))
          ; (let* ([msg (new message% [parent Innings1] [label (get-field name (car dup))])
          ;             (set dup (cdr dup))))
         
         (define Table12 (new table-panel% [parent Innings1]
                             [dimensions (list (+ z1 1) 8)]
                             [alignment '(left bottom)]
                             [column-stretchability #f]
                             [row-stretchability #f]
                             [spacing 10]))
         (new message% [parent Table12] [label "Bowler"] [font new-font])
         (new message% [parent Table12] [label "Overs"] [font new-font])
         (new message% [parent Table12] [label "Maidens"] [font new-font])
         (new message% [parent Table12] [label "Runs"] [font new-font])
         (new message% [parent Table12] [label "Wickets"] [font new-font])
         (new message% [parent Table12] [label "NB"] [font new-font])
         (new message% [parent Table12] [label "Wd"] [font new-font])
         (new message% [parent Table12] [label "ER"] [font new-font])
         (for ((i (in-range z1)))
           (let* (
                  [temp (car dup)])
             (new message% [parent Table12]
                  [label (get-field name temp)])
             (new message% [parent Table12]
                  [label (number->string (exact->inexact (+ (quotient (get-field balls-bowled temp) 6) (/ (remainder (get-field balls-bowled temp) 6) 10))))])
             (new message% [parent Table12]
                  [label (number->string (get-field maidens temp))])
             (new message% [parent Table12]
                  [label (number->string (get-field runs-given temp))])
             (new message% [parent Table12]
                  [label (number->string (get-field wickets temp))])
             (new message% [parent Table12]
                  [label (number->string (get-field NB temp))])
             (new message% [parent Table12]
                  [label (number->string (get-field WD temp))])
             (new message% [parent Table12]
                  [label (number->string (get-field ER temp))])
             (set! dup (cdr dup))))
         ;(define did-not-bat1 (new table-panel% [parent Table1]
          ;                         [alignment '(left top)]
           ;                        [dimensions (list 1 y1)]
            ;                       [stretchable-width #f]
             ;                      [stretchable-height #f]
              ;                     [column-stretchability #f]
               ;                    [row-stretchability #f]))
        ; (new message% [parent did-not-bat1] [label
         (define Table2 (new table-panel% [parent Innings2]
                             [alignment '(left top)]
                             [dimensions (list (+ x2 2) 6)]
                             [column-stretchability #f]
                             [row-stretchability #f]
                             [spacing 10]))
         (new message% [parent Table2] [label "Batsman"] [font new-font])
         (new message% [parent Table2] [label "Runs"] [font new-font])
         (new message% [parent Table2] [label "Balls"] [font new-font])
         (new message% [parent Table2] [label "4s"] [font new-font])
         (new message% [parent Table2] [label "6s"] [font new-font])
         (new message% [parent Table2] [label "SR"] [font new-font])
         (for ((i (in-range x2)))
           (let* (
                  [temp (car dup)])
             (new message% [parent Table2]
                  [label (get-field name temp)])
             (new message% [parent Table2]
                  [label (number->string (get-field runs temp))])
             (new message% [parent Table2]
                  [label (number->string (get-field balls temp))])
             (new message% [parent Table2]
                  [label (number->string (get-field fours temp))])
             (new message% [parent Table2]
                  [label (number->string (get-field sixes temp))])
             (new message% [parent Table2]
                  [label (number->string (get-field SR temp))])
             (set! dup (cdr dup))))
         ;(define Extras-table (new table-panel% [parent Table1]
                                ;[alignment '(left top)]
                                ;[dimensions (list 1 2)])
         ;(new message% [parent Extras-table] [label "Extras"])
         ;(new message% [parent Extras-table] [label Extras1])
         (define Total2 (new table-panel% [parent Table2]
                         [alignment '(left top)]
                         [dimensions (list 1 2)]
                         [column-stretchability #f]
                         [row-stretchability #f]))
         (new message% [parent Total2] [label "                                                           Total"])
         (new message% [parent Total2] [label total2])
         (set! dup (skip dup y2))
         (define Table22 (new table-panel% [parent Innings2]
                             [dimensions (list (+ z2 1) 8)]
                             [alignment '(left bottom)]
                             [column-stretchability #f]
                             [row-stretchability #f]
                             [spacing 10]))
         (new message% [parent Table22] [label "Bowler"] [font new-font])
         (new message% [parent Table22] [label "Overs"] [font new-font])
         (new message% [parent Table22] [label "Maidens"] [font new-font])
         (new message% [parent Table22] [label "Runs"] [font new-font])
         (new message% [parent Table22] [label "Wickets"] [font new-font])
         (new message% [parent Table22] [label "NB"] [font new-font])
         (new message% [parent Table22] [label "Wd"] [font new-font])
         (new message% [parent Table22] [label "ER"] [font new-font])
         (for ((i (in-range z2)))
           (let* ([temp (car dup)])
             (new message% [parent Table22]
                  [label (get-field name temp)])
             (new message% [parent Table22]
                  [label (number->string (exact->inexact (+ (quotient (get-field balls-bowled temp) 6) (/ (remainder (get-field balls-bowled temp) 6) 10))))])
             (new message% [parent Table22]
                  [label (number->string (get-field maidens temp))])
             (new message% [parent Table22]
                  [label (number->string (get-field runs-given temp))])
             (new message% [parent Table22]
                  [label (number->string (get-field wickets temp))])
             (new message% [parent Table22]
                  [label (number->string (get-field NB temp))])
             (new message% [parent Table22]
                  [label (number->string (get-field WD temp))])
             (new message% [parent Table22]
                  [label (number->string (get-field ER temp))])
             (set! dup (cdr dup))))
         (if bool (send scorecard show #t) (send scorecard show #f))
         list-pavan))
(define tournment (make-object tournment%))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Live-Match GUI;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (live-match-final url)
  (define k (html->xexp (get-pure-port (string->url url))))
  (define (teams l) (hello (string->list (get k '(4 3 5 3 5 2 2 3 2)))))
  (define frame (new frame% [label "Mini-Cricbuzz"] [border 10]
                     ))
  (define Main-logo (read-bitmap (get-pure-port (string->url "file:///home/nikhil/Pictures/Screenshot%20from%202018-04-19%2004-13-16.png"))))
  (define LHS (read-bitmap (get-pure-port (string->url "file:///home/nikhil/four1.jpeg")) #:backing-scale 6))
  (define RHS (read-bitmap (get-pure-port (string->url "file:///home/nikhil/four2.jpeg")) #:backing-scale 5))
  (define mainlogo (new horizontal-pane% [parent frame]))
  (new message% [label LHS] [parent mainlogo])
  (new message% [label Main-logo] [parent mainlogo])
  (new message% [label RHS] [parent mainlogo])
  (define body-logo (new horizontal-pane% [parent frame] [alignment '(center center)]))
  (new message% [label (scan (car (teams k)))] [parent body-logo]);[label (scan "RR")][parent body-logo])
  (define nikhil9 (new group-box-panel% [label "Live-Score"] [parent body-logo] [border 10] [spacing 8] [alignment '(center top)]))
  ;(define nikhil9 (new vertical-pane% [parent body-logo] [border 10] [spacing 10] [alignment '(center top)]))
  (define nikhil7 (new horizontal-pane% [parent nikhil9])) 
   (if (equal? (get k '(4 3 5 3 9 4 2 3 2 3 5 3 2)) #f)
       (new message% [label (get k '(4 3 5 3 9 4 2 3 2 3 3 3 2))] [parent nikhil7] [font new-font1])
       (begin (new message% [label (get k '(4 3 5 3 9 4 2 3 2 3 5 3 2))] [parent nikhil7])
              (new message% [label (get k '(4 3 5 3 9 4 2 3 2 3 3 2))] [parent nikhil7] [font new-font1]))) 
   (define ashrut 1)
  (if (get k '(4 3 5 3 9 4 2 3 3 2 4 2 2 2))
      (let* ([bat (new table-panel% [parent nikhil9]
                       [dimensions (list 5 6)]
                       [alignment '(left top)]
                       [spacing 15])])
        (new message% [label "Batsman"] [parent bat] [font new-font])
        (new message% [label "Runs"] [parent bat] [font new-font])
        (new message% [label "Balls"] [parent bat] [font new-font])
        (new message% [label "4s"] [parent bat] [font new-font])
        (new message% [label "6s"] [parent bat] [font new-font])
        (new message% [label "SR"] [parent bat] [font new-font])
        (new message% [label (get k '(4 3 5 3 9 4 2 3 3 2 3 2 2 2))] [parent bat])
        (new message% [label (get k '(4 3 5 3 9 4 2 3 3 2 3 3 2))] [parent bat])
        (new message% [label (get k '(4 3 5 3 9 4 2 3 3 2 3 4 2))] [parent bat])
        (new message% [label (get k '(4 3 5 3 9 4 2 3 3 2 3 5 2))] [parent bat])
        (new message% [label (get k '(4 3 5 3 9 4 2 3 3 2 3 6 2))] [parent bat])
        (new message% [label (get k '(4 3 5 3 9 4 2 3 3 2 3 7 2))] [parent bat])
        (new message% [label (get k '(4 3 5 3 9 4 2 3 3 2 4 2 2 2))] [parent bat])
        (new message% [label (get k '(4 3 5 3 9 4 2 3 3 2 4 3 2))] [parent bat])
        (new message% [label (get k '(4 3 5 3 9 4 2 3 3 2 4 4 2))] [parent bat])
        (new message% [label (get k '(4 3 5 3 9 4 2 3 3 2 4 5 2))] [parent bat])
        (new message% [label (get k '(4 3 5 3 9 4 2 3 3 2 4 6 2))] [parent bat])
        (new message% [label (get k '(4 3 5 3 9 4 2 3 3 2 4 7 2))] [parent bat])
        (new message% [label "Bowler"] [parent bat] [font new-font])
        (new message% [label "Overs"] [parent bat] [font new-font])
        (new message% [label "Maidens"] [parent bat] [font new-font])
        (new message% [label "Runs"] [parent bat] [font new-font])
        (new message% [label "Wickets"] [parent bat] [font new-font])
        (new message% [label "ER"] [parent bat] [font new-font])
        (new message% [label (get k '(4 3 5 3 9 4 2 3 3 3 3 2 2 2))] [parent bat])
        (new message% [label (get k '(4 3 5 3 9 4 2 3 3 3 3 3 2))] [parent bat])
        (new message% [label (get k '(4 3 5 3 9 4 2 3 3 3 3 4 2))] [parent bat])
        (new message% [label (get k '(4 3 5 3 9 4 2 3 3 3 3 5 2))] [parent bat])
        (new message% [label (get k '(4 3 5 3 9 4 2 3 3 3 3 6 2))] [parent bat])
        (new message% [label (get k '(4 3 5 3 9 4 2 3 3 3 3 7 2))] [parent bat])
        (new message% [label (string-append "Time-Line: " (get k '(4 3 5 3 9 4 2 3 5 3 1)))] [parent nikhil9])
        (set! ashrut 2))
      (let* ([bat (new table-panel% [parent nikhil9]
                       [dimensions (list 4 6)]
                       [alignment '(left top)]
                       [spacing 15])])
        (new message% [label "Batsman"] [parent bat] [font new-font])
        (new message% [label "Runs"] [parent bat] [font new-font])
        (new message% [label "Balls"] [parent bat] [font new-font])
        (new message% [label "4s"] [parent bat] [font new-font])
        (new message% [label "6s"] [parent bat] [font new-font])
        (new message% [label "SR"] [parent bat] [font new-font])
        (new message% [label (get k '(4 3 5 3 9 4 2 3 3 2 3 2 2 2))] [parent bat])
        (new message% [label (get k '(4 3 5 3 9 4 2 3 3 2 3 3 2))] [parent bat])
        (new message% [label (get k '(4 3 5 3 9 4 2 3 3 2 3 4 2))] [parent bat])
        (new message% [label (get k '(4 3 5 3 9 4 2 3 3 2 3 5 2))] [parent bat])
        (new message% [label (get k '(4 3 5 3 9 4 2 3 3 2 3 6 2))] [parent bat])
        (new message% [label (get k '(4 3 5 3 9 4 2 3 3 2 3 7 2))] [parent bat])
        (new message% [label "Bowler"] [parent bat] [font new-font])
        (new message% [label "Overs"] [parent bat] [font new-font])
        (new message% [label "Maidens"] [parent bat] [font new-font])
        (new message% [label "Runs"] [parent bat] [font new-font])
        (new message% [label "Wickets"] [parent bat] [font new-font])
        (new message% [label "ER"] [parent bat] [font new-font])
        (new message% [label (get k '(4 3 5 3 9 4 2 3 3 3 3 2 2 2))] [parent bat])
        (new message% [label (get k '(4 3 5 3 9 4 2 3 3 3 3 3 2))] [parent bat])
        (new message% [label (get k '(4 3 5 3 9 4 2 3 3 3 3 4 2))] [parent bat])
        (new message% [label (get k '(4 3 5 3 9 4 2 3 3 3 3 5 2))] [parent bat])
        (new message% [label (get k '(4 3 5 3 9 4 2 3 3 3 3 6 2))] [parent bat])
        (new message% [label (get k '(4 3 5 3 9 4 2 3 3 3 3 7 2))] [parent bat])
        (new message% [label (string-append "Time-Line: " (get k '(4 3 5 3 9 4 2 3 5 3 1)))] [parent nikhil9])
        (set! ashrut 3)))
  (define commentary (new group-box-panel% [label "Commentary"] [parent nikhil9] [min-width 80] [alignment '(center top)]))
  (new message% [label (string-append (if (equal? " " (get k '(4 3 5 3 9 4 7 3 3 2))) (get k '(4 3 5 3 9 4 7 3 3 3 2)) (if (list? (get k '(4 3 5 3 9 4 7 3 3 2))) (car (cdr (get k '(4 3 5 3 9 4 7 3 3 2)))) (get k '(4 3 5 3 9 4 7 3 3 2))))
                                      "    "
                                      (if (equal? " " (get k '(4 3 5 3 9 4 7 3 3 2))) (show k '(4 3 5 3 9 4 7 3 5 3)) ""))]
       [parent commentary])
  (new message% [label (string-append (if (equal? " " (get k '(4 3 5 3 9 4 7 5 3 2))) (get k '(4 3 5 3 9 4 7 5 3 3 2)) (if (list? (get k '(4 3 5 3 9 4 7 5 3 2))) (car (cdr (get k '(4 3 5 3 9 4 7 5 3 2)))) (show k '(4 3 5 3 9 4 7 5 3 3))))
                                      "    "
                                      (if (equal? " " (get k '(4 3 5 3 9 4 7 5 3 2))) (show k '(4 3 5 3 9 4 7 5 5 3)) ""))]
       [parent commentary])
  (new message% [label (scan (cadr (teams k)))] [parent body-logo]);[label (scan "MI")] [parent body-logo])
  (new button% [label "Refresh"] [parent nikhil7] [callback (lambda (button event) (begin (send frame show #f) (live-match-final (string-append "http://www.cricbuzz.com" (get k1 '(4 3 5 3 5 2 3 2 2 2 2 1 2 1))))))]);"http://www.cricbuzz.com/live--cricket-scores/19468/aiufdhiuherdifuh")))]) ;
  (send frame show #t))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (Tour)
  (define statistics (new frame% [label "Tournment Statistics"]))
  (for ((i (in-range number-of-matches)))
    (send tournment f (past-match (correct-substring (get k (list 4 3 5 3 10 2 (+ i 8) 4 2 2 1 2 1))) #f)))
  (new message% [label (string-append "Orange Cap:  " (get-field name (send tournment get-orange-cap)))] [parent statistics])
  (new message% [label (string-append "Purple Cap:  " (get-field name (send tournment get-purple-cap)))] [parent statistics])
  (new message% [label (string-append "Most Sixes:  " (get-field name (send tournment get-most-sixes)))] [parent statistics])
  (new message% [label (string-append "Most Fours:  " (get-field name (send tournment get-most-fours)))] [parent statistics])
  (new message% [label (string-append "Best Batting Strike Rate:  " (get-field name (send tournment get-best-SR)))] [parent statistics])
  (new message% [label (string-append "Best Bowling Economy Rate:  " (get-field name (send tournment get-best-ER)))] [parent statistics])
  (send statistics show #t))