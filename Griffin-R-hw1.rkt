;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Griffin-R-hw1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Ryan Griffin
; rmgriffin

; make-date: Natural Natural Natural -> date
; date?: Any -> Boolean
; date-year: date -> Natural
; date-month: date -> Natural
; date-day: date -> Natural
(define-struct date (year month day))

(define DATE1 (make-date 1999 10 5))
(define DATE2 (make-date 2006 4 20))
(define DATE3 (make-date 1999 10 5))

; make-film: String String Natural String date Number -> film
; film?: Any -> Boolean
; film-title: film -> String
; film-rating: film -> String
; film-running-time: film -> Natural
; film-genre: film -> String
; film-opening-date: film -> date
; film-box-office: film -> Number
(define-struct film (title rating running-time genre opening-date box-office))

(define INCEPTION (make-film "Inception" "PG-13" 120 "Sci-Fi" DATE1 90))
(define SCREAM (make-film "Scream" "R" 100 "Horror" DATE2 70))
(define MATRIX (make-film "Matrix" "PG-13" 112 "Sci-Fi" DATE3 110))

; double-bill?: film film -> Boolean
; consumes two films and returns true if they have the same rating and genre
(check-expect (double-bill? INCEPTION MATRIX) true)
(check-expect (double-bill? INCEPTION SCREAM) false)

(define (double-bill? film1 film2)
  (if (and (string=? (film-rating film1) (film-rating film2))
           (string=? (film-genre film1) (film-genre film2)))
      true
      false))

; combined-earnings: film film -> Number
; consumes two films and returns the combined earnings
(check-expect (combined-earnings INCEPTION MATRIX) 200)
(check-expect (combined-earnings INCEPTION SCREAM) 160)

(define (combined-earnings film1 film2)
        (+ (film-box-office film1) (film-box-office film2)))

; update-running-time: film Number -> film
; consumes a film and a number and produces the film with a new running time
(check-expect (update-running-time INCEPTION 150) (make-film "Inception" "PG-13" 150 "Sci-Fi" DATE1 90))
(check-expect (update-running-time SCREAM 50) (make-film "Scream" "R" 50 "Horror" DATE2 70))

(define (update-running-time film1 time)
        (make-film (film-title film1)
                   (film-rating film1)
                   time
                   (film-genre film1)
                   (film-opening-date film1)
                   (film-box-office film1)))

; opened-earlier: film film -> String
; consumes two films and produces the title of the film that opened first or "Same Opening Date" if both films opened on the same date
(check-expect (opened-earlier SCREAM INCEPTION) "Inception")
(check-expect (opened-earlier INCEPTION MATRIX) "Same Opening Date")

(define (opened-earlier film1 film2)
        (cond [(< (date-year (film-opening-date film1)) (date-year (film-opening-date film2))) (film-title film1)]
              [(> (date-year (film-opening-date film1)) (date-year (film-opening-date film2))) (film-title film2)]
              [(< (date-month (film-opening-date film1)) (date-month (film-opening-date film2))) (film-title film1)]
              [(> (date-month (film-opening-date film1)) (date-month (film-opening-date film2))) (film-title film2)]
              [(< (date-day (film-opening-date film1)) (date-day (film-opening-date film2))) (film-title film1)]
              [(> (date-day (film-opening-date film1)) (date-day (film-opening-date film2))) (film-title film2)]
              [else "Same Opening Date"]))