;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Griffin-R-hw4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Ryan Griffin
; rmgriffin

(define-struct student (name email))

(define RYAN (make-student "Ryan Griffin" "rmgriffin@wpi.edu"))
(define JOHN (make-student "John Doe" "jdoe@wpi.edu"))

; ListOfStudent is one of:
;   - empty
;   - (cons student ListOfStudent)

(define STUDENTS1 (list RYAN))
(define STUDENTS2 (list RYAN JOHN))

(define-struct projectnode (project-id title students advisor left right))

; (define (projectnode-fcn projectnode)
;   (projectnode-id projectnode)
;   (projectnode-title projectnode)
;   (projectnode-students projectnode)
;   (projectnode-advisor projectnode)
;   (projectnode-left projectnode)
;   (projectnode-right projectnode))