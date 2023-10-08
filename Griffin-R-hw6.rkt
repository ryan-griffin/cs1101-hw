;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Griffin-R-hw6) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; Ryan Griffin
; rmgriffin

; 1 )

; Email is a (make-email String String Boolean)
; interp:
;   User-Id is the unique identifier to a user
;   Message is the body of the email
;   Downloaded? is whether or not the user has downloaded the email
;
; ListOfEmail is one of:
;   - empty
;   - (cons Email Empty)
(define-struct email (user-id message downloaded?))

; Mailuser is a (make-mailuser String ListOfEmail)
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

; add-mailuser: String -> Void
; consumes a user id and creates a new user in mailsys and returns void
(define (add-mailuser user-id)
  (set! mailsys (cons (make-mailuser user-id empty) mailsys)))

; 4 )

; find-user: String -> Mailuser
; consumes a user id and returns the matching user from mailsys
(define (find-user user-id)
  (first (filter (lambda (mailuser)
                 (string=? (mailuser-user-id mailuser) user-id))
               mailsys)))

; send-email: String String String -> Void
; consumes sender user id, recipient user id and a message to send to the recipient and returns void
(define (send-email sender-id recipient-id message)
  (let ([recipient-user (find-user recipient-id)])
    (set-mailuser-inbox! recipient-user
                         (cons (make-email sender-id message false)
                               (mailuser-inbox recipient-user)))))

; 5 )

; get-unread-emails: String -> ListOfEmail
; Returns a list of undownloaded emails for the specified user and marks them as downloaded
(define (get-unread-emails user-id)
    (let ([unread-emails (filter (lambda (email)
                                   (not (email-downloaded? email)))
                                 (mailuser-inbox (find-user user-id)))])
      (begin (for-each (lambda (email)
                         (set-email-downloaded?! email true))
                       unread-emails)
             unread-emails)))

; 6 )

; most-emails: -> Mailuser
; Returns the mailuser with the largest inbox or an error if the system is empty.
(define (most-emails)
  (if (empty? mailsys)
      (error "No mailusers in the system")
      (foldl (lambda (mailuser1 mailuser2)
               (if (> (length (mailuser-inbox mailuser1))
                      (length (mailuser-inbox mailuser2)))
                   mailuser1
                   mailuser2))
             (first mailsys)
             (rest mailsys))))

; 8 )

; total-characters: ListOfString -> Natural
; Consumes a ListOfString and produces the sum of the characters of each string.
(define (total-characters los)
  (apply + (map string-length los)))

; 9 )

; all-caps: ListOfString -> ListOfString
; Consumes a ListOfString and produces a ListOfString with all strings in uppercase.
(define (all-caps los)
  (map string-upcase los))
