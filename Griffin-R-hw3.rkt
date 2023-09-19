;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Griffin-R-hw3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Ryan Griffin
; rmgriffin

(define-struct requisition (name category seasonal? price quantity))

; (define (requisition-fcn requisition)
;         (...
;          (requisition-name requisition)
;          (requisition-category requisition)
;          (requisition-seasonal? requisition)
;          (requisition-price requisition
;          (requisition-quantity requisition)
;         ...)) 

(define REQ1 (make-requisition "candy" "food" false 2 10))
(define REQ2 (make-requisition "Jackets" "clothing" true 15 3))
(define REQ3 (make-requisition "fiction" "books" false 3 5))

; Invoice is one of:
;   empty
;   (cons requisition Invoice)
;   interp: An invoice is a list of requisitions
;
; (define (invoice-fcn invoice)
;         (cond [(empty? invoice) (...)]
;               [() (...)]))

; list-expensive-clothes: Invoice Number -> Invoice
(define (list-expensive-clothes invoice threshold)
  (cond [(empty? invoice) empty]
        [(and (string=? (requisition-category (first invoice)) "clothing")
              (> (requisition-price (first invoice)) threshold))
         (cons (first invoice) (list-expensive-clothes (rest invoice) threshold))]
        [else (list-expensive-clothes (rest invoice) threshold)]))

; double-check?: Invoice -> Boolean
(define (double-check? invoice)
  (cond [(empty? invoice) empty]
        [(> (requisition-quantity (first invoice)) 1) true]
        [else (double-check? (rest invoice))]))

; count-books: Invoice -> Number
(define (count-books invoice)
  (cond [(empty? invoice) empty]
        [(requisition-quantity (first invoice))]))

(define INVOICE1 (list REQ1 REQ2 REQ3))
(define INVOICE2 (list REQ1 REQ2 REQ3 (make-requisition "t-shirts" "clothing" true 9 6)))