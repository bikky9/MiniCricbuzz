#lang racket/gui
(require (lib "racket/trace"))
(require table-panel)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;pavan;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (skip l k) (cond [(= k 0) l]
                         [else (skip (cdr l) (- k 1))]))
(define-syntax while
  (syntax-rules (:)
    ((while : pred? : stmt ...)
    (do () ((not pred?))
      stmt ...))))
(define tournment%
  (class object%
    (super-new)
    (define scrap (make-object player% 0 0 0 0 0 0 0 0 0 0 0 0 0))
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
    
    (define/public (f l2)
      (begin
        (define (f2 l1)
      (cond [(not (null? l1))
             (let* ((x (car l1))
                    (z (findf (lambda (y) (equal? (get-field  name y) (get-field name x))) big-l)))
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
                    (scrapr (get-field sixes scrap))
                    (xr (get-field sixes x)))
             (cond [(< scrapr xr) (begin (set! most-sixes x) (modify-ms (cdr l)))]
                   [(= scrapr xr) (begin (cond [(> (get-field runs x) (get-field runs scrap))
                                                (set! most-sixes x)])
                                         (modify-ms (cdr l)))]))]))

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
             (cond [(< scrapr xr) (begin (set! best-SR x) (modify-bSR (cdr l)))]
                   [(= scrapr xr) (begin (cond [(> (get-field runs x) (get-field runs best-SR))
                                                (set! best-SR x)])
                                         (modify-bSR (cdr l)))]
                   [else (modify-bSR (cdr l))]))]))

    (define (modify-bER l)
      (cond [(not (null? l))
             (let* ((x (car l))
                    (scrapr (get-field ER best-ER))
                    (xr (get-field ER x)))
             (cond [(< scrapr xr) (begin (set! best-ER x) (modify-bER (cdr l)))]
                   [(= scrapr xr) (begin (cond [(> (get-field wickets x) (get-field wickets best-ER))
                                                (set! best-ER x)])
                                         (modify-bER (cdr l)))]))]
            [else (modify-bER (cdr l))]))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Niranjan;;;;;;;;;;;;;;;;;;;;;;;;;
(require net/url)
(require (planet neil/html-parsing:3:0))
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
    (define/public (set-SR) (set! SR (if (not (equal? runs 0)) (exact->inexact (/ (floor (* (/ runs balls) 10000)) 100)) 0)))
    (define/public (set-ER) (set! ER (if (not (equal? balls-bowled 0)) (exact->inexact (/
                                                                   (floor (* (/ runs-given balls-bowled) 600)) 100)) 0)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define k (html->xexp (port->string (get-pure-port
     (string->url "http://www.cricbuzz.com/live-cricket-scorecard/20065/csk-vs-kkr-5th-match-indian-premier-league-2018")))))
;;;;;;;;;;;;;;;;;;;;;;;;;batting scorecard;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(trace get)
(define (past-match url)
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
         (define scorecard (new frame% [label (get k '(4 2 22 1 2 1))]))
         (new message% [parent scorecard] [label (get k '(4 3 5 3 9 3 2 2))])
         (define Teams (new horizontal-pane% [parent scorecard] [spacing 10] [border 10]))
         (define total1 (string-append (get k '(4 3 5 3 9 3 3 3 3 5 2)) (get k '(4 3 5 3 9 3 3 3 3 5 4))))
         (define total2 (string-append (get k '(4 3 5 3 9 3 4 3 3 5 2)) (get k '(4 3 5 3 9 3 4 3 3 5 4))))
         (define Innings1 (new group-box-panel% [parent Teams] [label (get k '(4 3 5 3 9 3 3 3 3 3 1))] [border 10]))
         (define Innings2 (new group-box-panel% [parent Teams] [label (get k '(4 3 5 3 9 3 4 3 3 3 1))] [border 10]))
         (define Table1 (new table-panel% [parent Innings1]
                             [alignment '(left top)]
                             [dimensions (list (+ x1 2) 6)]
                             [column-stretchability #f]
                             [row-stretchability #f]
                             [spacing 10]))
         (new message% [parent Table1] [label "Batsmen"])
         (new message% [parent Table1] [label "Runs"])
         (new message% [parent Table1] [label "Balls"])
         (new message% [parent Table1] [label "4s"])
         (new message% [parent Table1] [label "6s"])
         (new message% [parent Table1] [label "SR"])
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
         (new message% [parent Table12] [label "Bowler"])
         (new message% [parent Table12] [label "Overs"])
         (new message% [parent Table12] [label "Maidens"])
         (new message% [parent Table12] [label "Runs"])
         (new message% [parent Table12] [label "Wickets"])
         (new message% [parent Table12] [label "NB"])
         (new message% [parent Table12] [label "Wd"])
         (new message% [parent Table12] [label "ER"])
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
         (new message% [parent Table2] [label "Batsmen"])
         (new message% [parent Table2] [label "Runs"])
         (new message% [parent Table2] [label "Balls"])
         (new message% [parent Table2] [label "4s"])
         (new message% [parent Table2] [label "6s"])
         (new message% [parent Table2] [label "SR"])
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
         (new message% [parent Table22] [label "Bowler"])
         (new message% [parent Table22] [label "Overs"])
         (new message% [parent Table22] [label "Maidens"])
         (new message% [parent Table22] [label "Runs"])
         (new message% [parent Table22] [label "Wickets"])
         (new message% [parent Table22] [label "NB"])
         (new message% [parent Table22] [label "Wd"])
         (new message% [parent Table22] [label "ER"])
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
         (send scorecard show #t)
         list-pavan))
(define tournment (make-object tournment%))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;GUI;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(send tournment f (past-match "http://www.cricbuzz.com/live-cricket-scorecard/20064/srh-vs-rr-4th-match-indian-premier-league-2018"))
;(send tournment f (past-match "http://www.cricbuzz.com/live-cricket-scorecard/20082/dd-vs-kxip-22st-match-indian-premier-league-2018"))
;(send tournment f (past-match "http://www.cricbuzz.com/live-cricket-scorecard/20081/dd-vs-kxip-22st-match-indian-premier-league-2018"))
;(send tournment f (past-match "http://www.cricbuzz.com/live-cricket-scorecard/20065/dd-vs-kxip-22st-match-indian-premier-league-2018"))