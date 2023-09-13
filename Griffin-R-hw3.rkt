;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Griffin-R-hw3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Ryan Griffin
; rmgriffin

(define-struct requisition (name category seasonal? price quantity))

; (define (requisition-fcn requisition)
;         (...
;         (requisition-name requisition)
;         (requisition-category requisition)
;         (requisition-seasonal? requisition)
;         (requisition-price quantity
;         ...)) 

(define REQ1 (make-requisition "candy" "food" false 2 10))
(define REQ2 (make-requisition "Jackets" "clothing" true 15 3))
(define REQ3 (make-requisition "fiction" "books" false 3 5))

; Invoice is one of:
;   empty
;   (cons requisition Invoice)
;   interp: An invoice is a list of requisitions

(define INVOICE1 (list REQ1 REQ2 REQ3))
(define INVOICE2 (list REQ1 REQ2 REQ3 (make-requisition "t-shirts" "clothing" true 9 6)))