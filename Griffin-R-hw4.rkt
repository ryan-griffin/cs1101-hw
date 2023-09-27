;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Griffin-R-hw4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

(define PROJECTS
  (make-projectnode 17.2205 "Project A" STUDENTS1 "Advisor A"
                    (make-projectnode 17.2204 "Project B" STUDENTS2 "Advisor B"
                                      (make-projectnode 17.2203 "Project E" STUDENTS1 "Advisor E" false false)
                                      false)
                    (make-projectnode 17.2207 "Project C" empty "Advisor C"
                                      false
                                      (make-projectnode 17.2208 "Project D" empty "Advisor D" false false))))
(define PROJECTS1 (make-projectnode 17.2201 "Project X" STUDENTS1 "Advisor X" false false))

; 4 )

; change-advisor: ProjectNode Number String -> ProjectNode
; consumes a project, the number of a project, and the name of an advisor and produces a project updated with the new advisors name
(check-expect (change-advisor PROJECTS1 17.2201 "New Advisor")
              (make-projectnode 17.2201 "Project X" STUDENTS1 "New Advisor" false false))

(define (change-advisor project number new-advisor)
  (cond [(< number (projectnode-project-id project))
         (make-projectnode
          (projectnode-project-id project)
          (projectnode-title project)
          (projectnode-students project)
          (projectnode-advisor project)
          (change-advisor (projectnode-left project) number new-advisor)
          (projectnode-right project))]
        [(> number (projectnode-project-id project))
         (make-projectnode
          (projectnode-project-id project)
          (projectnode-title project)
          (projectnode-students project)
          (projectnode-advisor project)
          (projectnode-left project)
          (change-advisor (projectnode-right project) number new-advisor))]
        [else
         (make-projectnode
          (projectnode-project-id project)
          (projectnode-title project)
          (projectnode-students project)
          new-advisor
          (projectnode-left project)
          (projectnode-right project))]))

; 5 )

; number-of-projects-in-dept: BST Number -> Number
; consumes a tree of projects and the number of a department and produces the number of projects from given department
(check-expect (number-of-projects-in-dept false 17) 0)
(check-expect (number-of-projects-in-dept PROJECTS 17) 5)
(check-expect (number-of-projects-in-dept PROJECTS 18) 0)

(define (number-of-projects-in-dept tree department-number)
  (if (false? tree)
      0
      (local ((define project-id (floor (projectnode-project-id tree))))
        (cond [(= project-id department-number)
               (+ 1
                  (number-of-projects-in-dept (projectnode-left tree) department-number)
                  (number-of-projects-in-dept (projectnode-right tree) department-number))]
              [(< project-id department-number)
               (number-of-projects-in-dept (projectnode-right tree) department-number)]
              [else (number-of-projects-in-dept (projectnode-left tree) department-number)]))))


; 6 )

; student-in-list?: String ListOfStudent-> Boolean
; consumes an email address and a list of students and returns true if the email is in the list
(define (student-in-list? email students)
  (cond [(empty? students) false]
        [(string=? email (student-email (first students))) true]
        [else (student-in-list? email (rest students))]))

; student-has-project?: BST String -> Boolean
; consumes a tree of projects and an email and produces true if the email appears in the list of students working on a project
(check-expect (student-has-project? false "jdoe@wpi.edu") false)
(check-expect (student-has-project? PROJECTS "jdoe@wpi.edu") true)
(check-expect (student-has-project? PROJECTS "rmgriffin@wpi.edu") true)
(check-expect (student-has-project? PROJECTS "nonexistent@wpi.edu") false)

(define (student-has-project? tree email)
  (cond [(false? tree) false]
        [(student-in-list? email (projectnode-students tree)) true]
        [else (or (student-has-project? (projectnode-left tree) email)
                  (student-has-project? (projectnode-right tree) email))]))

; 7 )

; list-of-projects-ordered-by-id-num: BST -> (listof String)
; consumes a tree of projects and produces a list of titles of the projects sorted by ascending project number
(check-expect (list-of-projects-ordered-by-id-num false) empty)
(check-expect (list-of-projects-ordered-by-id-num PROJECTS) (list "Project E" "Project B" "Project A" "Project C" "Project D"))
(check-expect (list-of-projects-ordered-by-id-num PROJECTS1) (list "Project X"))

(define (list-of-projects-ordered-by-id-num tree)
  (cond [(false? tree) empty]
        [else (append (list-of-projects-ordered-by-id-num (projectnode-left tree))
                      (list (projectnode-title tree))
                      (list-of-projects-ordered-by-id-num (projectnode-right tree)))]))

; 8 )

; create-project: BST Number String String -> BST
; consumes a BST, project number,project title, and name of advisor, and creates a BST same as original but with a new project added
(check-expect (make-projectnode 17.2201 "Project X" STUDENTS1 "Advisor X" false false) PROJECTS1)

(define (create-project tree number title advisor)
  (cond [(false? tree) (make-projectnode number
                                         title
                                         empty
                                         advisor
                                         false
                                         false)]
        [(< number (projectnode-project-id tree))
         (make-projectnode (projectnode-project-id tree)
                           (projectnode-title tree)
                           (projectnode-students tree)
                           (projectnode-advisor tree)
                           (create-project (projectnode-left tree) number title advisor)
                           (projectnode-right tree))]
        [(> number (projectnode-project-id tree))
         (make-projectnode (projectnode-project-id tree)
                           (projectnode-title tree)
                           (projectnode-students tree)
                           (projectnode-advisor tree)
                           (projectnode-left tree)
                           (create-project (projectnode-right tree) number title advisor))]
        [else tree]))
