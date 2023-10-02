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

(define JEFFERSON (make-river "Jefferson" 6.5 true empty))
(define SUN (make-river "Sun" 11.5 false (list JEFFERSON)))
(define MISSOURI (make-river "Missouri" 6 true (list SUN)))

; 4 )

; list-blooming-rivers: River -> ListOfString
; consumes a river system and produces a list of the names of rivers with algal blooms
(check-expect (list-blooming-rivers JEFFERSON) (list "Jefferson"))
(check-expect (list-blooming-rivers SUN) (list "Jefferson"))
(check-expect (list-blooming-rivers MISSOURI) (list "Missouri" "Jefferson"))

(define (list-blooming-rivers river)
  (let ([subsystem-results (apply append (map list-blooming-rivers (river-tributaries river)))])
    (if (river-bloom? river)
        (cons (river-name river) subsystem-results)
        subsystem-results)))

; 5 )

; any-too-acidic?: River -> Boolean
; consumes a river system and returns true if any river in the system has a pH level below 6.5
(check-expect (any-too-acidic? JEFFERSON) false)
(check-expect (any-too-acidic? SUN) false)
(check-expect (any-too-acidic? MISSOURI) true)

(define (any-too-acidic? river)
  (if (< (river-ph river) 6.5)
      true
      (ormap any-too-acidic? (river-tributaries river))))

; 6 )

; lower-all-ph: River -> River
; consumes a river system and lowers the pH values of all rivers in the system by 0.2
(check-expect (lower-all-ph JEFFERSON)
              (make-river "Jefferson" 6.3 true empty))
(check-expect (lower-all-ph SUN)
              (make-river "Sun" 11.3 false (list (make-river "Jefferson" 6.3 true empty))))
(check-expect (lower-all-ph MISSOURI)
              (make-river "Missouri" 5.8 true (list (make-river "Sun" 11.3 false (list (make-river "Jefferson" 6.3 true empty))))))

(define (lower-all-ph river)
  (make-river
   (river-name river)
   (- (river-ph river) 0.2)
   (river-bloom? river)
   (map lower-all-ph (river-tributaries river))))

; 7 )

; find-subsystem: String River -> (U River False)
; consumes the name of a river and a river system and returns the subsystem with the given name as the root, or false if not found
(check-expect (find-subsystem "River" JEFFERSON) false)
(check-expect (find-subsystem "Jefferson" JEFFERSON) JEFFERSON)
(check-expect (find-subsystem "Jefferson" MISSOURI) JEFFERSON)
(check-expect (find-subsystem "Sun" MISSOURI) SUN)

(define (find-subsystem name river)
  (if (string=? name (river-name river))
      river
      (let ([valid-subsystems (filter (lambda (subsystem) (not (false? subsystem)))
                       (map (lambda (subriver) (find-subsystem name subriver))
                            (river-tributaries river)))])
        (if (empty? valid-subsystems)
            false
            (car valid-subsystems)))))


; a Requisition is a (make-Requisition String String Boolean Number Natural)
; interp:
;    Requisition represents a goody sold by Gompei's Goodies, where
;    Name is the name of the goody
;    Category indicates whether the item is food, clothing, book, toy, etc.
;    Seasonal? is true if the item is stocked temporarily - for the season
;    Price is the cost of a single goody
;    Quantity is the number of goodies being purchased (multiplicity)
(define-struct requisition (name category seasonal? price quantity))

(define CLOTHING-REQUISITION-1 (make-requisition "Shirt" "clothing" true 16 1))
(define CLOTHING-REQUISITION-2 (make-requisition "Socks" "clothing" false 6 3))
(define FOOD-REQUISITION (make-requisition "Chocolate" "food" false 2.5 5))
(define BOOK-REQUISITION (make-requisition "Novel" "book" true 13 1))

; an Invoice (ListOfRequisition) is one of:
;    empty
;    (cons Requisition Invoice)

(define EMPTY-INVOICE empty)
(define SINGLE-ITEM-INVOICE (list CLOTHING-REQUISITION-1))
(define MULTI-ITEM-INVOICE (list CLOTHING-REQUISITION-1 CLOTHING-REQUISITION-2 FOOD-REQUISITION BOOK-REQUISITION))

; 8 )

; list-expensive-clothing: ListOfRequisition Number -> ListOfRequisition
; consumes an invoice and a number and produces an invoice with just those requisitions of clothing items with prices that exceed the number
(check-expect (list-expensive-clothing EMPTY-INVOICE 10) empty)
(check-expect (list-expensive-clothing SINGLE-ITEM-INVOICE 10) (list CLOTHING-REQUISITION-1))
(check-expect (list-expensive-clothing MULTI-ITEM-INVOICE 10) (list CLOTHING-REQUISITION-1))

(define (list-expensive-clothing invoice threshold)
  (filter (lambda (requisition)
            (and (string=? (requisition-category requisition) "clothing")
                 (> (requisition-price requisition) threshold))) invoice))

; 9 )

; double-check?: ListOfRequisition -> Boolean
; consumes an invoice and produces true if any requisition has a quantity greater than 1
(check-expect (double-check? EMPTY-INVOICE) false)
(check-expect (double-check? SINGLE-ITEM-INVOICE) false)
(check-expect (double-check? MULTI-ITEM-INVOICE) true)

(define (double-check? invoice)
  (not (empty? (filter (lambda (requisition)
                         (> (requisition-quantity requisition) 1)) invoice))))

; 10 )

; seasonal-sales-goodies: ListOfRequisition -> ListOfString
; consumes an invoice and returns a list of the names of seasonal goodies (requisitions)
(check-expect (seasonal-sales-goodies EMPTY-INVOICE) empty)
(check-expect (seasonal-sales-goodies SINGLE-ITEM-INVOICE) (list "Shirt"))
(check-expect (seasonal-sales-goodies MULTI-ITEM-INVOICE) (list "Shirt" "Novel"))

(define (seasonal-sales-goodies invoice)
  (map requisition-name (filter (lambda (requisition)
                                  (requisition-seasonal? requisition)) invoice)))