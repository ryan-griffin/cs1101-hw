;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Griffin-R-hw1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Ryan Griffin
; rmgriffin

; 1 )

; a date is a (make-date Natural Natural Natural)
; interp: represents a date where
;   year is the year
;   month is the month
;   day is the day

(define-struct date (year month day))

; make-date: Natural Natural Natural -> date
; date?: Any -> Boolean
; date-year: date -> Natural
; date-month: date -> Natural
; date-day: date -> Natural

(define DATE1 (make-date 1999 10 5))
(define DATE2 (make-date 2006 4 20))
(define DATE3 (make-date 1999 10 5))

; 2 )

; a film is a (make-film String String Natural String date Number)
; interp: represents a film where
;   title is the title of the film
;   rating is the rating of the the film (PG, PG-13, R, etc)
;   running-time is the length of the film
;   genre is the genre of the film (Sci-Fi, Horror, Comedy, etc)
;   opening-date is the date the film was released
;   box-office is the total earnings of the film

(define-struct film (title rating running-time genre opening-date box-office))

; make-film: String String Natural String date Number -> film
; film?: Any -> Boolean
; film-title: film -> String
; film-rating: film -> String
; film-running-time: film -> Natural
; film-genre: film -> String
; film-opening-date: film -> date
; film-box-office: film -> Number

(define INCEPTION (make-film "Inception" "PG-13" 120 "Sci-Fi" DATE1 90))
(define SCREAM (make-film "Scream" "R" 100 "Horror" DATE2 70))
(define MATRIX (make-film "Matrix" "PG-13" 112 "Sci-Fi" DATE3 110))

; 3 )

; double-bill?: film film -> Boolean
; consumes two films and returns true if they have the same rating and genre
(check-expect (double-bill? INCEPTION MATRIX) true)
(check-expect (double-bill? INCEPTION SCREAM) false)
(check-expect (double-bill? MATRIX SCREAM) false)

(define (double-bill? film1 film2)
  (if (and (string=? (film-rating film1) (film-rating film2))
           (string=? (film-genre film1) (film-genre film2)))
      true
      false))

; 4 )

; combined-earnings: film film -> Number
; consumes two films and returns the combined earnings
(check-expect (combined-earnings INCEPTION MATRIX) 200)
(check-expect (combined-earnings INCEPTION SCREAM) 160)
(check-expect (combined-earnings SCREAM MATRIX) 180)

(define (combined-earnings film1 film2)
        (+ (film-box-office film1) (film-box-office film2)))

; 5 )

; update-running-time: film Number -> film
; consumes a film and a number and produces the film with a new running time
(check-expect (update-running-time INCEPTION 150) (make-film "Inception" "PG-13" 150 "Sci-Fi" DATE1 90))
(check-expect (update-running-time SCREAM 50) (make-film "Scream" "R" 50 "Horror" DATE2 70))
(check-expect (update-running-time MATRIX 1) (make-film "Matrix" "PG-13" 1 "Sci-Fi" DATE3 110))

(define (update-running-time film1 time)
        (make-film (film-title film1)
                   (film-rating film1)
                   time
                   (film-genre film1)
                   (film-opening-date film1)
                   (film-box-office film1)))

; 6 )

; opened-earlier: film film -> String
; consumes two films and produces the title of the film that opened first or "Same Opening Date" if both films opened on the same date
(check-expect (opened-earlier SCREAM INCEPTION) "Inception")
(check-expect (opened-earlier INCEPTION MATRIX) "Same Opening Date")
(check-expect (opened-earlier SCREAM MATRIX) "Matrix")

(define (opened-earlier film1 film2)
        (cond [(< (date-year (film-opening-date film1)) (date-year (film-opening-date film2))) (film-title film1)]
              [(> (date-year (film-opening-date film1)) (date-year (film-opening-date film2))) (film-title film2)]
              [(< (date-month (film-opening-date film1)) (date-month (film-opening-date film2))) (film-title film1)]
              [(> (date-month (film-opening-date film1)) (date-month (film-opening-date film2))) (film-title film2)]
              [(< (date-day (film-opening-date film1)) (date-day (film-opening-date film2))) (film-title film1)]
              [(> (date-day (film-opening-date film1)) (date-day (film-opening-date film2))) (film-title film2)]
              [else "Same Opening Date"]))