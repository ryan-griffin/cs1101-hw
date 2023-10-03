;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Griffin-R-hw6) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; Ryan Griffin
; rmgriffin

; 1 )

; Email is a (make-email Natural String Boolean)
; interp:
;   User-Id is the unique identifier to a user
;   Message is the body of the email
;   Downloaded? is whether or not the user has downloaded the email
;
; ListOfEmail is one of:
;   - empty
;   - (cons Email Empty)
(define-struct email (user-id message downloaded?))

; Mailuser is a (make-mailuser Natural ListOfEmail)
; interp:
;   User-Id is the unique identifier of the user
;   Inbox is a list of emails
;
; ListOfMailuser is one of:
;   - empty
;   - (cons Mailuser Empty)
(define-struct mailuser (user-id inbox))

; 2 )

(define mailsys empty)

; 3 )

; add-mailuser: Natural -> Void
; consumes a user id and creates a new user in ListOfMailuser and returns void
(define (add-mailuser user-id)
  (begin (cons (make-mailuser user-id empty) mailsys) void))

; 4 )
; send-email: Natural Natural String -> Void
; consumes sender user id, recipient user id and a message to send to the recipient and returns void
(define (send-email sender-uid recipient-uid message)
  (begin (...) void))