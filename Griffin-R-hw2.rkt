;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Griffin-R-hw2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; 1 )

; make-roadracer: Number Number String Number -> Roadracer
(define-struct roadracer (rate max-psi frame wheelsize))

; Roadracer is (make-roadracer Number Number String Number)
; interp: represents a Roadracer where
;   Rate is the daily rental cost
;   Max-psi is the max tire pressure in pounds per square inch
;   Frame is the material the frame is made of
;   Wheelsize is the size of the wheels in inches
;
; (define (fn-for-roadracer roadracer)
;         (... (roadracer-rate roadracer)
;              (roadracer-max-psi roadracer)
;              (roadracer-frame roadracer)
;              (roadracer-wheelsize roadracer)))

(define ROADRACER (make-roadracer 50 20 "carbon fiber" 28))

; make-fatbike: Number Number Number -> Fatbike
(define-struct fatbike (rate max-psi tire-width))

; Fatbike is (make-fatbike Number Number Number)
; interp: represents a Fatbike where
;  Rate is the daily rental cost
;  Max-psi is the max tire pressure in pounds per square inch
;  Tire-width is the width of the tires in inches
;
; (define (fn-for-fatbike fatbike)
;         (... (fatbike-rate fatbike)
;              (fatbike-max-psi fatbike)
;              (roadracer-tirewidth fatbike)))

(define FATBIKE (make-fatbike 60 21 10))

; make-tandem: Number Number Boolean -> Tandem
(define-struct tandem (rate wheelsize panniers?))

; Tandem is (make-tandem Number Number Boolean)
; interp: represents a Tandem where
;   Rate is the daily rental cost
;   Wheelsize is the size of the wheel in inches
;   Panniers? is whether or not panniers are present
;
; (define (fn-for-tandem tandem)
;         (... (tandem-rate tandem)
;              (tandem-wheelsize tandem)
;              (tandem-panniers? tandem)))

(define TANDEM (make-tandem 75 21 true))

; a Bicycle is one of:
;    - Roadracer
;    - Fatbike
;    - Tandem
; interp: a bicycle is one of 3 types
;
; (define (bicycle-fcn bicycle)
;         (cond [(roadracer? bicycle)
;                (...
;                 (roadracer-rate bicycle)
;                 (roadracer-max-psi bicycle)
;                 (roadracer-frame bicycle)
;                 (roadracer-wheelsize bicycle))]
;               [(fatbike? bicycle)
;                (...
;                 (fatbike-rate bicycle)
;                 (fatbike-max-psi bicycle)
;                 (fatbike-tire-width bicycle))]
;               [(tandem? bicycle)
;                (...
;                 (tandem-rate bicycle)
;                 (tandem-wheelsize bicycle)
;                 (tandem-panniers bicycle))]))

; 4 )

; special-prep?: Bicycle -> Boolean
; consumes a Bicycle and produces true if the Bicycle is a Roadracer with a carbon fiber frame and a
; wheelsize larger than 27 inches, or a Fatbike with tires wider than 5 inches or max tire pressure
; of 20 pounds per square inch or less, or a Tandem with panniers.
(check-expect (special-prep? ROADRACER) true)
(check-expect (special-prep? FATBIKE) true)
(check-expect (special-prep? TANDEM) true)

(define (special-prep? bike)
        (cond [(roadracer? bike)
               (if (and (string=? (roadracer-frame bike) "carbon fiber")
                        (> (roadracer-wheelsize bike) 27))
                   true
                   false)]
              [(fatbike? bike)
               (if (or (> (fatbike-tire-width bike) 5)
                       (<= (fatbike-max-psi bike) 20))
                   true
                   false)]
              [(tandem? bike)
               (if (tandem-panniers? bike)
                   true
                   false)]
              [else false]))

; 5 )

; frequent-rider-discount: Bicycle Number -> Bicycle
; consumes a Bicycle and a discount and produces the same Bicycle with a discount applied to the rate
(check-expect (frequent-rider-discount ROADRACER 10) (make-roadracer 45 20 "carbon fiber" 28))
(check-expect (frequent-rider-discount FATBIKE 20) (make-fatbike 48 21 10))
(check-expect (frequent-rider-discount TANDEM 12) (make-tandem 66 21 true))

(define (frequent-rider-discount bike discount)
        (cond [(roadracer? bike)
               (make-roadracer (- (roadracer-rate bike) (* (roadracer-rate bike) (/ discount 100)))
                               (roadracer-max-psi bike)
                               (roadracer-frame bike)
                               (roadracer-wheelsize bike))]
              [(fatbike? bike)
               (make-fatbike (- (fatbike-rate bike) (* (fatbike-rate bike) (/ discount 100)))
                             (fatbike-max-psi bike)
                             (fatbike-tire-width bike))]
              [(tandem? bike)
               (make-tandem (- (tandem-rate bike) (* (tandem-rate bike) (/ discount 100)))
                            (tandem-wheelsize bike)
                            (tandem-panniers? bike))]))

; a ListOfString is one of
;   empty
;   (cons String ListOfString)
; interp: ListOfString represents a list of strings

; 6 )

; total-characters: ListOfString -> Natural
; consumes a ListOfString and produces a Natural representing the sum of the characters of each string in the ListOfString
(check-expect (total-characters empty) 0)
(check-expect (total-characters (list "Hello" "World!")) 11)
(check-expect (total-characters (list "a")) 1)

(define (total-characters listOfString)
        (if (empty? listOfString)
            0
            (+ (string-length (first listOfString))
               (total-characters (rest listOfString)))))

; 7)

; possible-plurals: ListOfString -> ListOfString
; consumes a ListOfString and produces a list that contains only strings from the original list that end in "s" or "S"
(check-expect (possible-plurals (list "")) empty)
(check-expect (possible-plurals (list "Hello" "World")) empty)
(check-expect (possible-plurals (list "Hello" "Worlds")) (list "Worlds"))
(check-expect (possible-plurals (list "S")) (list "S"))

(define (possible-plurals listofstring)
  (cond [(empty? listofstring) empty]
        [(or (string=? (first listofstring) "")
             (not (string-ci=? (substring (first listofstring) (- (string-length (first listofstring)) 1)) "s")))
         (possible-plurals (rest listofstring))]
        [else (cons (first listofstring) (possible-plurals (rest listofstring)))]))

; 8 )

; all-caps: ListOfString -> ListOfString
; consumes a ListOfString and produces the same list of strings but with all letters capitalized
(check-expect (all-caps empty) empty)
(check-expect (all-caps (list "")) (list ""))
(check-expect (all-caps (list "hello" "world")) (list "HELLO" "WORLD"))
(check-expect (all-caps (list "This" "is" "a" "test")) (list "THIS" "IS" "A" "TEST"))

(define (all-caps listofstring)
  (if (empty? listofstring)
      empty
      (cons (string-upcase (first listofstring))
            (all-caps (rest listofstring)))))