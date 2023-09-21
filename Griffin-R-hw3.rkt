;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Griffin-R-hw3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Ryan Griffin
; rmgriffin

(define-struct requisition (name category seasonal? price quantity))
; Requisition is (make-requisition String String Boolean Number Number)
;
; interp: represents a Requisition where
;   - Name is the name of the various items sold
;   - Category is the type of items sold (food, clothing, book, toy, etc.)
;   - Seasonal? is whether or not the items are seasonal
;   - Price is the price of a single item
;   - Quantity is the ammount of items requested
;
; (define (requisition-fcn requisition)
;   (...
;    (requisition-name requisition)
;    (requisition-category requisition)
;    (requisition-seasonal? requisition)
;    (requisition-price requisition
;    (requisition-quantity requisition)
;   ...)) 

(define REQ1 (make-requisition "candy" "food" false 2 10))
(define REQ2 (make-requisition "Jackets" "clothing" true 15 1))
(define REQ3 (make-requisition "fiction" "book" false 3 5))

; Invoice is one of:
;   empty
;   (cons requisition Invoice)
;   interp: An Invoice is a list of requisitions
;
; (define (invoice-fcn invoice)
;   (cond [(empty? invoice) (...)]
;         [else (... (first invoice)
;                    (invoice-fcn (rest invoice)))]))

(define INVOICE1 (list REQ1 REQ2 REQ3))
(define INVOICE2 (list REQ1 REQ2 REQ3 (make-requisition "t-shirts" "clothing" true 9 6)))

; list-expensive-clothes: Invoice Number -> Invoice
; consumes an Invoice and a Number and produces an Invoice containing only requisitions that are clothing items with a price greater than the given Number
(check-expect (list-expensive-clothes (list REQ1) 1) empty)
(check-expect (list-expensive-clothes (list REQ1) 2) empty)
(check-expect (list-expensive-clothes (list REQ2) 15) empty)
(check-expect (list-expensive-clothes (list REQ2) 10) (list REQ2))
(check-expect (list-expensive-clothes (list REQ2 REQ2) 11) (list REQ2 REQ2))
(check-expect (list-expensive-clothes INVOICE1 12) (list REQ2))

(define (list-expensive-clothes invoice threshold)
  (cond [(empty? invoice) empty]
        [(and (string=? (requisition-category (first invoice)) "clothing")
              (> (requisition-price (first invoice)) threshold))
         (cons (first invoice) (list-expensive-clothes (rest invoice) threshold))]
        [else (list-expensive-clothes (rest invoice) threshold)]))

; double-check?: Invoice -> Boolean
; consumes an Invoice and returns true if any of the requisitions on the invoice have a quantity greater than one
(check-expect (double-check? (list)) false)
(check-expect (double-check? (list REQ2)) false)
(check-expect (double-check? (list REQ2 REQ3)) true)
(check-expect (double-check? INVOICE1) true)

(define (double-check? invoice)
  (cond [(empty? invoice) false]
        [(> (requisition-quantity (first invoice)) 1) true]
        [else (double-check? (rest invoice))]))

; count-books: Invoice -> Number
; consumes an Invoice and produces the total Number of all books ordered
(check-expect (count-books (list)) 0)
(check-expect (count-books (list REQ1)) 0)
(check-expect (count-books INVOICE1) 5)
(check-expect (count-books (list REQ3 REQ1 REQ3)) 10)

(define (count-books invoice)
  (cond [(empty? invoice) 0]
        [(string=? (requisition-category (first invoice)) "book")
         (+ (requisition-quantity (first invoice))
            (count-books (rest invoice)))]
        [else (count-books (rest invoice))]))

; invoice-total: Invoice -> Number
; consumes an Invoice and produces the total cost of the requisitions (a Number).
(check-expect (invoice-total (list)) 0)
(check-expect (invoice-total (list REQ1)) 20)
(check-expect (invoice-total (list REQ2)) 15)
(check-expect (invoice-total INVOICE1) 50)

(define (invoice-total invoice)
  (if (empty? invoice) 0
      (+ (* (requisition-price (first invoice))
            (requisition-quantity (first invoice)))
         (invoice-total (rest invoice)))))

; seasonal-sale: Invoice Number -> Number
; consumes an Invoice and a discount (Number) and produces the total cost of the requisitions with the discount applied to seasonal requisitions
(check-expect (seasonal-sale (list) 0.25) 0)
(check-expect (seasonal-sale (list REQ1 REQ3) 0.25) 35)
(check-expect (seasonal-sale (list REQ2 REQ2 REQ2) 0.25) 33.75)
(check-expect (seasonal-sale INVOICE1 0.25) 46.25)

(define (seasonal-sale invoice discount)
  (if (empty? invoice) 0
      (+ (* (if (requisition-seasonal? (first invoice))
                (- (requisition-price (first invoice))
                   (* (requisition-price (first invoice)) discount))
                (requisition-price (first invoice)))
            (requisition-quantity (first invoice)))
         (seasonal-sale (rest invoice) discount))))