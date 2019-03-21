;;polynomial
;;
;;by Jinzhe Li
;;11 November 2018
;;
;;This is a Scheme program that operate on 
;;single-variable polynomials. It takes
;;polunomials to do operations which are
;;add, sub, mult, evel or diff. After operation,
;;it returns the new formed polynomial, which
;;is the result of operation.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;make poly
;;The polynomial is represented as a list of number,
;;this function makes that list into a list of pairs.

(define (makePoly polynomial)
  (cond
    ((null? polynomial) '())
    (else
      (append (list (list (car polynomial) (car (cdr polynomial))))
        (makePoly (cddr polynomial))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;sort poly
;;Source code is from Rosanna Heise,
;;I made some little change to it to meet my need.
;;This function keeps the polynomials in order
;;from highest exponent to lowest.

(define (partition compFn aList)
   (cond
      ((null? aList) '())
      ((compFn (cadar aList)) (cons (car aList) (partition compFn (cdr aList))))
      (else (partition compFn (cdr aList)))))

(define (qSort aList)
   (cond
      ((null? aList) '())
      (else                  ;;(car aList) is pivot
         (append (append 
                    (qSort (partition (lambda (x) (> x (cadar aList))) aList))
                    (partition (lambda (x) (= x (cadar aList))) aList))
                 (qSort (partition (lambda (x) (< x (cadar aList))) aList))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;write poly
;;Every operation returns one list to here,
;;this function take that list and print it
;;out. The cases that coefficient is 0 or 1,
;;exponent is 0 or 1, and they are negative
;;or positive, are considered here.

(define (writePoly poly)
  (cond
    ((number? (car poly))(apply + poly))
    (else
      (cond
        ((and (pair? (car poly)) (zero? (cadar (qSort poly))))
         (display (abs (caar (qSort poly))))
         (helpWritePoly poly))
        ((and (negative? (caar (qSort poly)))
              (and (and (pair? (car poly)) 
                        (equal? (caar (qSort poly)) -1)) 
                   (equal? (cadar (qSort poly)) 1)))
         (display "-") (display "x") (helpWritePoly poly))
        ((and (negative? (caar (qSort poly)))
              (and (pair? (car poly))
                   (equal? (caar (qSort poly)) -1)))
         (display "-") (display "x^") (display (cadar (qSort poly)))
         (helpWritePoly poly))
        ((and (negative? (caar (qSort poly))) 
              (and (pair? (car poly))
                   (equal? (cadar (qSort poly)) -1)))
         (display "-") (display (abs (caar (qSort poly)))) (display "x")
         (helpWritePoly poly))
        ((and (negative? (caar (qSort poly))) (pair? (car poly)))
         (display "-") (display (abs (caar (qSort poly)))) (display "x^")
         (display (cadar (qSort poly))) (helpWritePoly poly))
        ((and (and (pair? (car poly)) 
                   (equal? (caar (qSort poly)) 1))
              (equal? (cadar (qSort poly)) 1))
         (display "x") (helpWritePoly poly))
        ((and (pair? (car poly)) 
              (equal? (caar (qSort poly)) 1))
         (display "x^") (display (cadar (qSort poly))) (helpWritePoly poly))
        ((and (pair? (car poly)) (equal? (cadar (qSort poly)) 1))
         (display (caar (qSort poly))) (display "x") (helpWritePoly poly))
        ((pair? (car poly))
         (display (caar (qSort poly))) (display "x^")
         (display (cadar (qSort poly))) (helpWritePoly poly))))))


(define (helpWritePoly poly)
  (cond
    ((null? (cdr (qSort poly))))
    (else
      (cond
        ((and (negative? (caar (cdr (qSort poly)))) 
              (zero? (cadr (car (cdr (qSort poly))))))
         (display " - ") (display (abs (caar (cdr (qSort poly))))) 
         (helpWritePoly (cdr (qSort poly))))
        ((and (and (negative? (caar (cdr (qSort poly)))) 
                   (equal? (abs (caadr (qSort poly))) 1))
              (equal? (cadr (car (cdr (qSort poly)))) 1))
         (display " - ") (display "x") (helpWritePoly (cdr (qSort poly))))
        ((and (negative? (caar (cdr (qSort poly)))) 
              (equal? (abs (caadr (qSort poly))) 1))
         (display " - ") (display "x^") (display (cadr (car (cdr (qSort poly))))) 
         (helpWritePoly (cdr (qSort poly))))
        ((and (negative? (caar (cdr (qSort poly)))) 
              (equal? (cadr (car (cdr (qSort poly)))) 1))
         (display " - ") (display (abs (caar (cdr (qSort poly))))) 
         (display "x")
         (helpWritePoly (cdr (qSort poly))))
        ((negative? (caar (cdr (qSort poly))))
         (display " - ") (display (abs (caar (cdr (qSort poly))))) 
         (display "x^") 
         (display (cadr (car (cdr (qSort poly))))) 
         (helpWritePoly (cdr (qSort poly))))
        (else
          (cond
            ((zero? (cadr (car (cdr (qSort poly)))))
             (display " + ") (display (caar (cdr (qSort poly))))
             (helpWritePoly (cdr (qSort poly))))
            (else
              (cond
                ((and (equal? (abs (caadr (qSort poly))) 1)
                      (equal? (cadr (car (cdr (qSort poly)))) 1))
                 (display " + ") (display "x") 
                 (helpWritePoly (cdr (qSort poly))))
                ((equal? (abs (caadr (qSort poly))) 1)
                 (display " + ") (display "x^") 
                 (display (cadr (car (cdr (qSort poly))))) 
                 (helpWritePoly (cdr (qSort poly))))
                ((equal? (cadr (car (cdr (qSort poly)))) 1)
                 (display " + ") (display (abs (caar (cdr (qSort poly))))) 
                 (display "x") 
                 (helpWritePoly (cdr (qSort poly))))
                (else
                  (display " + ") (display (caar (cdr (qSort poly))))
                  (display "x^") (display (cadr (car (cdr (qSort poly))))) 
                  (helpWritePoly (cdr (qSort poly))))))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;merge poly
;;This function does not work yet,
;;the goal of it is to merge two or more
;;terms, which have the same exponent,
;;into one term.
;;The idea is
;;if exp in (car poly) = exp in other (car poly)
;;then(append ((add theirs coeff) exp) (caddr poly))   
;;and do recursion.
;;
;;(define (mergePoly polyno)
;;  (cond
;;    ((null? polyno))
;;    ((null? (cdr polyno)))
;;    (else
;;      ((equal? (cadar polyno) (cadar (cdr polyno)))
;;         (append (list (list (+ (caar polyno)
;;                              (caadr polyno)) (cadar polyno)))
;;           (mergePoly (cdr polyno))) ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;add poly
;;It takes two polynomials and returns a list of
;;pairs which is their sum.

(define (addPolys firstPoly secondPoly)
  (append firstPoly secondPoly))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;sub polys
;;This function give a list of pairs which is 
;;the first polynomial minus the second poly.
;;
;;negToposPoly function is to change the posi/neg
;;of coeff in poly2, then poly2 becomes -poly2,
;;then do the same thing as in add function.
;;which is poly1 + (-poly2)
;;
(define (subPolys firstPoly secondPoly)
  (append firstPoly (negToposPoly secondPoly)))

(define (negToposPoly secondPoly)
  (cond
      ((null? secondPoly) '())
      (else
        (append (list (list (* (caar secondPoly) -1) (cadar secondPoly))) 
          (negToposPoly (cdr secondPoly))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;mult polys
;;This function takes two polynomials and then
;;returns a list of pairs which is the product of
;;the two polynomials.
;;
;;termMultPoly function takes one term from poly1
;;then gives the product of this term and poly2 to
;;multPolys.
;;multPolys function gives one term each time to
;;termMultPoly function to get the product, and append
;;these product together into a list of pairs.

(define (multPolys firstPoly secondPoly)
  (cond
    ((null? firstPoly) '())
    (else
      (append (termMultPoly firstPoly secondPoly) 
        (multPolys (cdr firstPoly) secondPoly)))))


(define (termMultPoly firstPoly secondPoly)
  (cond
    ((null? secondPoly) '())
    (else
      (append (list (list (* (caar firstPoly)
                           (caar secondPoly)) 
                      (+ (cadar firstPoly) (cadar secondPoly)))) 
        (termMultPoly firstPoly (cdr secondPoly))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;evel polys
;;This function takes a real number as the first parameter
;;and a polynomial as the second parameter.
;;This function is different because it returns a
;;list of numbers, not a list of pairs.
;;
;;it gives a number to poly1 and get the result of
;;each pair into a list of numbers. This list of numbers
;;will be take care in writePoly and be sum together to
;;get a number of answer.

(define (evelPoly realNum firstPoly)
  (cond
    ((null? firstPoly) '())
    (else
      (append (list (* (caar firstPoly) (expt realNum (cadar firstPoly))))
        (evelPolys realNum (cdr firstPoly))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;diff polys
;;This function takes a polynomial as a parameter,
;;and returns a list of pairs which is its
;;derivative.
;;
;;In each pair, coeff after operation will be
;;coeff * exp, exponent after operation will be 
;;exp - 1.
;;
(define (diffPoly firstPoly)
  (cond
    ((null? firstPoly) '())
    (else
      (cond
        ((zero? (cadar firstPoly)) (diffPolys (cdr firstPoly)))
        (else
          (append (list (list (* (caar firstPoly) (cadar firstPoly))
                          (- (cadar firstPoly) 1))) 
            (diffPolys (cdr firstPoly))))))))