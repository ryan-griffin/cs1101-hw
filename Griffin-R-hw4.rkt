;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Griffin-R-hw4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Ryan Griffin
; rmgriffin

; Student is a (make-student String String)
; interp: represents a Student where:
;   - Name is the name of the student
;   - email is the email of the student
;
; (define (student-fcn student)
;   (...
;    (student-name student)
;    (student-email student)
;    ...))

(define-struct student (name email))

(define RYAN (make-student "Ryan Griffin" "rmgriffin@wpi.edu"))
(define JOHN (make-student "John Doe" "jdoe@wpi.edu"))

; ListOfStudent is one of:
;   - empty
;   - (cons student ListOfStudent)
;   interp: a ListOfStudent is a list of students
;
; (define (listofstudent-fcn listofstudent)
;   (cond [(empty? listofstudent) (...)]
;         [else (... (first listofstudent)
;                    (listofstudent-fcn (rest listofstudent)))]))

(define STUDENTS1 (list RYAN))
(define STUDENTS2 (list RYAN JOHN))

; a BST is one of
;   - false
;   - ProjectNode
;   interp: represents a binary search tree where:
;   - if its a ProjectNode:
;      - all Project-Ids in the left subtree are less than the Project-Id in the ProjectNode.
;      - all Project-Ids in the right subtree are greater than the Project-Id in the ProjectNode.
;   - if its false:
;      its an empty binary search tree.
;
; ProjectNode is a (make-projectnode Number String ListOfStudent String BST BST)
; interp: represents a ProjectNode where:
;   - Project-Id is is a unique project ID
;   - Title is the title of the project
;   - Students is a list of students working on the project
;   - Advisor is the name of the project advisor
;   - Left is the left BST
;   - Right is the right BST
;
; (define (projectnode-fcn projectnode)
;   (...
;    (projectnode-id projectnode)
;    (projectnode-title projectnode)
;    (projectnode-students projectnode)
;    (projectnode-advisor projectnode)
;    (projectnode-left projectnode)
;    (projectnode-right projectnode)
;    ...))

(define-struct projectnode (project-id title students advisor left right))

(define PROJECTS (make-projectnode 17.2205 "Project A" STUDENTS1 "Advisor A"
                                   (make-projectnode 17.2206 "Project B" STUDENTS2 "Advisor B"
                                                     false
                                                     false)
                                   (make-projectnode 17.2207 "Project C" empty "Advisor C"
                                                     false
                                                     (make-projectnode 17.2208 "Project D" empty "Advisor D"
                                                                       false
                                                                       false))))

