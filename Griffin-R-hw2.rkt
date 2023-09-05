;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Griffin-R-hw2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; make-roadracer: Number Number String Number -> roadracer
(define-struct roadracer (rate max-psi frame wheelsize))

; make-fatbike: Number Number Number -> fatbike
(define-struct fatbike (rate max-psi tire-width))

; make-tandem: Number Number Boolean -> tandem
(define-struct tandem (rate wheelsize panniers?))

; a bicycle is one of:
;    - roadracer
;    - fatbike
;    - tandem
(define (bicycle-fcn bicycle)
        (cond [(roadracer? bicycle)
               (...
                (roadracer-rate bicycle)
                (roadracer-max-psi bicycle)
                (roadracer-frame bicycle)
                (roadracer-wheelsize bicycle))]
              [(fatbike? bicycle)
               (...
                (fatbike-rate bicycle)
                (fatbike-max-psi bicycle)
                (fatbike-tire-width bicycle))]
              [(tandem? bicycle)
               (...
                (tandem-rate bicycle)
                (tandem-wheelsize bicycle)
                (tandem-panniers bicycle))]))