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

(define JEFFERSON (make-river "Jefferson" 7.1 true empty))
(define SUN (make-river "Sun" 11.5 false (list JEFFERSON)))
(define MISSOURI (make-river "Missouri" 6 true (list SUN)))

; 4 )

; list-blooming-rivers: River -> ListOfString
; consumes a river system and produces a list of the names of rivers with algal blooms
(check-expect (list-blooming-rivers JEFFERSON) (list "Jefferson"))
(check-expect (list-blooming-rivers MISSOURI) (list "Missouri" "Jefferson"))

(define (list-blooming-rivers river)
  (if (river-bloom? river)
      (cons (river-name river)
            (apply append (map list-blooming-rivers (river-tributaries river))))
      (apply append (map list-blooming-rivers (river-tributaries river)))))

; 5 )

; any-too-acidic?: River -> Boolean
; consumes a river system and returns true if any river in the system has a pH level below 6.5
(check-expect (any-too-acidic? JEFFERSON) false)
(check-expect (any-too-acidic? MISSOURI) true)

(define (any-too-acidic? river)
  (if (< (river-ph river) 6.5)
      true
      (ormap any-too-acidic? (river-tributaries river))))

; 6 )

; lower-all-ph: River -> River
; consumes a river system and lowers the pH values of all rivers in the system by 0.2
(check-expect (lower-all-ph JEFFERSON)
              (make-river "Jefferson" 6.9 true empty))
(check-expect (lower-all-ph SUN)
              (make-river "Sun" 11.3 false (list (make-river "Jefferson" 6.9 true empty))))
(check-expect (lower-all-ph MISSOURI)
              (make-river "Missouri" 5.8 true (list (make-river "Sun" 11.3 false (list (make-river "Jefferson" 6.9 true empty))))))

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
      (local [(define subsystems (map (lambda (subriver) (find-subsystem name subriver)) (river-tributaries river)))]
        (if (empty? (filter (lambda (subsystem) (not (false? subsystem))) subsystems))
            false
            (car (filter (lambda (subsystem) (not (false? subsystem))) subsystems))))))






