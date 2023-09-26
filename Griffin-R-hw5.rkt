;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Griffin-R-hw5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Ryan Griffin
; rmgriffin

; River is a (make-river String Natural Boolean Tributaries)
; interp: represents a river where:
;   - Name is the name of the river
;   - Ph is is the pH level of the river
;   - Bloom? is whether or not algal blooms are present
;   - Tributaries is a list of rivers that feed into the river
;
; (define (river-fcn river)
;   (...
;    (river-name river)
;    (river-ph river)
;    (river-bloom? river)
;    (river-tributaries? river)
;    ...))
;
; Tributaries is one of:
;   - empty
;   - (cons river tributaries)
;
; (define (list-of-river-fcn tributaries)
;   (cond [(empty? tributaries) (...)]
;         [else (... (first tributaries)
;                    (list-of-river-fcn (rest tributaries)))]))

(define-struct river (name ph bloom? tributaries))

(define JEFFERSON (make-river "Jefferson" 7.1 false empty))
(define SUN (make-river "Sun" 11.5 false (list JEFFERSON)))
(define MISSOURI (make-river "Missouri" 6 true (list SUN)))