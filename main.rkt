#lang racket/base
(require racket/runtime-path
         racket/list
         racket/function
         xml
         (prefix-in d: racket/date))

(struct date (year month day))
(struct user (name bday))
(struct activity (id except never-died? start-age end-age freq name))

(define (date->seconds d)
  (d:find-seconds 0 0 0 (date-day d) (date-month d) (date-year d)))

;; xxx Not the same computation humans normally use
(define (user-age u)
  (if (date? (user-bday u))
    (/ (- (current-seconds)
          (date->seconds (user-bday u)))
       (* 60 60 24 365))
    (user-bday u)))

(define (list-max l)
  (apply max l))

(define (activity-live? a u)
  (and (<= (activity-start-age a)
           (user-age u))
       (< (user-age u)
          (activity-end-age a))))

(define (activity-died? u a)
  (and (<= (activity-start-age a)
           (user-age u))
       (not (< (user-age u)
               (activity-end-age a)))))

(define (add-spacing l)
  (if (= (length l) 5)
    l
    (add-between l `(td ([class "gapday"]) nbsp))))

(define-runtime-path source-dir ".")

(define (go output-p users activities)
  (define live-activities
    (filter (λ (a)
              (ormap (curry activity-live? a)
                     users))
            activities))
  (define max-freq
    (list-max (map activity-freq live-activities)))

  (define xe
    `(html
      (head
       (title "Kid Grid")
       (link ([rel "stylesheet"]
              [type "text/css"]
              [href "style.css"])))
      (body
       (div
        ([id "container"])
        (div
         ([id "header"])
         (h3 "Week of "
             (script
              "document.write(new Date().toDateString());")))
        (div
         ([id "body"])
         (table
          ([id "activities"])
          (tbody
           (tr ([class "boxes"])
               ,@(for/list ([u (in-list users)])
                   (define all-live-for-u
                     (map activity-id
                          (filter (λ (a) (activity-live? a u))
                                  live-activities)))
                   (define (live-sibling? a)
                     (member (activity-id a)
                             all-live-for-u))
                   (define-values (died-as live-as)
                     (partition
                      (curry activity-died? u)
                      activities))
                   (define-values (live-sibling still-died-as)
                     (partition
                      live-sibling?
                      died-as))
                   (define-values (except-u still-still-died-as)
                     (partition
                      (λ (a)
                        (member u (activity-except a)))
                      still-died-as))
                   (define-values (nd still-still-still-died-as)
                     (partition
                      activity-never-died?
                      still-still-died-as))
                   (define final-died-as
                     still-still-still-died-as)
                   (printf "~a: Ignored ~v\n"
                           (user-name u)
                           (map activity-name
                                (append live-sibling except-u nd)))
                   `(td ([class "died"])
                        (table
                         (tr
                          (td ,(user-name u))
                          (td ,@(map (λ (a) `(span ,(activity-name a))) 
                                     final-died-as)))))))
           ,@(append*
              (for/list ([a (in-list live-activities)])
                (list
                 `(tr ([class "activity"])
                      (td ([colspan ,(number->string (length users))])
                          ,(activity-name a)))
                 `(tr ([class "boxes"])
                      ,@(for/list ([u (in-list users)])
                          (if (activity-live? a u)
                            `(td
                              (table ([id "days"])
                                     (tr ,@(add-spacing
                                            (for/list ([i (in-range (activity-freq a))])
                                              `(td
                                                (table ([id "day"])
                                                       (tr
                                                        ,@(for/list ([j (in-range 3)])
                                                            `(td nbsp))))))))))
                            `(td ([class "blank"]) nbsp))))))))))
        (div ([id "footer"])
             ,@(for/list ([i (in-range 21)])
                 `(span ,(number->string i) nbsp)))))))

  (with-output-to-file output-p
    #:exists 'replace
    (λ ()
      (displayln "<!DOCTYPE html>")
      (write-xexpr xe))))

(module+ main
  (require racket/runtime-path)
  (define-runtime-path index.tex "static/index.tex")

  (define current-id
    (make-parameter (λ () (gensym))))
  (define current-except-users
    (make-parameter empty))
  (define current-never-died
    (make-parameter #f))
  (define-syntax-rule (*activity . a)
    (activity ((current-id)) (current-except-users) (current-never-died)
              . a))

  (define-syntax-rule (meta-activity a ...)
    (let ([the-id (gensym)])
      (parameterize ([current-id (λ () the-id)])
        (list a ...))))
  (define-syntax-rule (except-user u a ...)
    (parameterize ([current-except-users (list u)])
      (list a ...)))
  (define-syntax-rule (never-died a ...)
    (parameterize ([current-never-died #t])
      (list a ...)))

  (define u:frog
    (user "Frog"
          6.0))
  (define u:peach
    (user "Peach"
          4.0))
  (define u:hazel
    (user "Hazel"
          2.0))

  (define ??? 18.0)
  ;; xxx specify how long/how many of each needs to be done at various
  ;; ages and increase jack's limits to at least 5 words in sentences
  ;; and reading 15 sentences rather than 10

  ;; xxx ensure that tasks have lists in anki

  ;; xxx generate multi-digit addition cards according to multi-digit
  ;; addition (75% carrying, 50% 3x3 (last), 25% 3x2, 25% 2x2)

  ;; xxx convert to latex output?
  (go
   index.html
   (list
    u:frog
    u:peach
    u:hazel)
   (flatten
    (list
     (*activity 2.0 3.5 5 "Reading > Letters")
     (*activity 3.0 3.5 5 "Math > Numbers")
     ;; (*activity 3.0 ??? 1 "Life Skills")
     ;; (*activity 3.0 3.5 3 "Reading > Learning Books")
     (never-died
      (*activity 3.0 3.5 3 "Writing > Tracing"))
     (*activity 3.25 6.0 3 "Computer > Keyboard & Mouse")
     (except-user u:frog
                  (*activity 3.5 4.5 5 "Reading > Letters [written]")
                  (*activity 3.5 4.5 5 "Math > Numbers [written]"))
     (*activity 3.5 5.5 5 "Reading > Blends")
     (*activity 4.0 ??? 5 "Reading > Words [written]")
     (*activity 4.0 5.0 5 "Math > Addition [10x10] [written]")
     (meta-activity
      (*activity 4.5 5.5 5 "Reading > Beginning reader books (to parent) [5s]")
      (*activity 5.5 ??? 5 "Reading > Beginning reader books (to parent) [10s]"))
     (*activity 5.0 6.0 5 "Math > Subtraction [10x10] [written]")
     (meta-activity
      (*activity 5.0 5.5 3 "Science > Basic reading (w/ parent)")
      (*activity 5.5 ??? 5 "Science > Basic reading (w/ parent)"))
     (meta-activity
      (*activity 5.0 5.5 3 "History > Basic reading (w/ parent)")
      (*activity 5.5 ??? 5 "History > Basic reading (w/ parent)"))
     (*activity 5.0 ??? 5 "Literature > Basic reading (w/ parent)")
     (*activity 5.0 ??? 5 "Composition > Sentences [written]")
     (*activity 5.5 ??? 5 "Math > Addition [10x10] [memory]")
     (*activity 5.5 ??? 5 "Math > Counting")
     (*activity 6.0 ??? 5 "Writing > Words [spelled]")
     (*activity 6.0 ??? 5 "Math > Subtraction [10x10] [memory]")
     (*activity 6.0 ??? 5 "Math > Multi-Digit Addition")
     (*activity 6.0 ??? 6 "Piano")
     (*activity 6.0 ??? 5 "Computer > Typing")
     (*activity 6.5 ??? 5 "Math > Multiplication [10x10] [written]")))))
