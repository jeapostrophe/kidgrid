#lang racket/base
(require racket/runtime-path
         racket/list
         racket/function
         xml
         (prefix-in d: racket/date))

(struct date (year month day))
(struct user (name bday))
(struct activity (id except never-died? start-age end-age freq name*))

(define (activity-name a)
  (regexp-replace*
   (regexp-quote "&")
   (regexp-replace*
    (regexp-quote ">")
    (activity-name* a)
    "\\\\texttt{>}")
   "\\\\&"))

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

  (with-output-to-file output-p
    #:exists 'replace
    (λ ()
      (for ([u (in-list users)])
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
        (define seen? (make-hasheq))
        (define s4-died-as
          (reverse
           (filter (λ (a)
                     (define i (activity-id a))
                     (if (hash-has-key? seen? i)
                         #f
                         (hash-set! seen? i #t)))
                   (reverse still-still-still-died-as))))
        (define final-died-as
          s4-died-as)
        (eprintf "~a: Ignored ~v\n"
                 (user-name u)
                 (map activity-name
                      (append live-sibling except-u nd)))

        (printf "\\begin{user}\n")
        (printf "\t\\name{~a}\n" (user-name u))
        (printf "\n")
        (printf "\t\\begin{died}")
        (for ([a (in-list final-died-as)])
          (printf "\\activitybox{~a}" (activity-name a)))
        (when (empty? final-died-as)
          (printf "\\activitybox{~a}" "Tickles!"))
        (printf "\\end{died}\n")
        (printf "\n")
        (for ([a (in-list live-activities)]
              #:when (activity-live? a u))
          (printf "\t\\activitylabel{~a}\n" (activity-name a))
          (printf "\t\\begin{activitydays}")
          (for ([i (activity-freq a)])
            (printf "\\activityday{}"))
          (printf "\\end{activitydays}\n"))
        (printf "\\end{user}\n")))))

(module+ main
  (require racket/runtime-path)
  (define-runtime-path index.tex "index.tex")

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
          8.0))
  (define u:peach
    (user "Peach"
          6.5))
  (define u:hazel
    (user "Hazel"
          4.0))

  (define ??? 18.0)
  (go
   index.tex
   (list
    u:frog
    u:peach
    u:hazel)
   (flatten
    (list
     (*activity 2.0 3.5 5 "Reading > Letters [x10]")
     (*activity 2.0 3.5 5 "Math > Numbers [x5]")
     ;; (*activity 3.0 ??? 1 "Life Skills")
     ;; (*activity 3.0 3.5 3 "Reading > Learning Books")
     (never-died
      (*activity 3.0 3.5 3 "Writing > Tracing"))
     (meta-activity
      (*activity 3.25 4.25 3 "Computer > Keyboard & Mouse")
      (*activity 4.25 ??? 5 "Computer > Typing"))
     (except-user u:frog
                  (*activity 3.5 4.5 5 "Reading > Letters [written] [x10]")
                  (*activity 3.5 4.5 5 "Math > Numbers [written] [x5]"))
     (meta-activity
      (*activity 3.5 4.0 5 "Reading > Blends [x10]")
      (*activity 4.0 5.5 5 "Reading > Blends [x25]"))
     (meta-activity
      (*activity 4.0 4.5 5 "Reading > Words [r/w] [x5]")
      (*activity 4.5 6.0 5 "Reading > Words [r/w] [x10]")
      (*activity 6.0 6.5 5 "Reading > Words [r/w] [x10]")
      (*activity 6.5 ??? 5 "Reading > Words [r] [x20]"))
     (meta-activity
      (*activity 4.0 4.5 5 "Math > Addition [10x10] [written] [x5]")
      (*activity 4.5 5.0 5 "Math > Addition [10x10] [written] [x10]"))
     (meta-activity
      (*activity 4.5 5.5 5 "Reading > Beginning reader books (to parent) [5s]")
      (*activity 5.5 6.0 5 "Reading > Beginning reader books (to parent) [10s]")
      (*activity 6.0 6.5 5 "Reading > Beginning reader books (to parent) [15s]")
      (*activity 6.5 ??? 5 "Reading > Beginning reader books (to parent) [1u]"))
     (meta-activity
      (*activity 4.5 6.5 5 "Math > Khan Academy [x5]")
      (*activity 6.5 6.75 5 "Math > Khan Academy [x10]")
      (*activity 6.75 7.0 5 "Math > Khan Academy [x13]")
      (*activity 7.25 7.5 5 "Math > Khan Academy [x15]")
      (*activity 7.5 ??? 5 "Math > Khan Academy [x20]"))
     (*activity 5.0 6.0 5 "Math > Subtraction [10x10] [written] [x10]")
     (meta-activity
      (*activity 5.0 5.5 3 "Science > Basic reading (w/ parent)")
      (*activity 5.5 ??? 5 "Science > Basic reading (w/ parent)"))
     (meta-activity
      (*activity 5.0 5.5 3 "History > Basic reading (w/ parent)")
      (*activity 5.5 ??? 5 "History > Basic reading (w/ parent)"))
     (*activity 5.0 ??? 5 "Literature > Basic reading (w/ parent)")
     (meta-activity
      (*activity 5.0 6.0 5 "Composition > Copy-work / Journal [>10w]")
      (*activity 6.0 ??? 5 "Composition > Copy-work / Journal [>20w]"))
     (*activity 5.5 6.5 5 "Math > Addition [10x10] [memory] [x10]")
     (meta-activity
      (*activity 6.0 6.5 5 "Writing > Words [o/w] [x10]")
      (*activity 6.5 ??? 5 "Writing > Words [o/w] [x20]"))
     (meta-activity
      (*activity 6.0 6.5 5 "Math > Subtraction [10x10] [memory] [x5]")
      (*activity 6.5 ??? 5 "Math > Subtraction [10x10] [memory] [x10]"))
     (meta-activity
      (*activity 6.0 6.5 5 "Math > Multi-Digit Addition [x5]")
      (*activity 6.5 6.75 5 "Math > Multi-Digit Addition [x10]")
      (*activity 6.75 ??? 5 "Math > Multi-Digit Addition [x5]"))
     (*activity 6.0 ??? 5 "Music")
     (meta-activity
      (*activity 6.5 ??? 5 "Math > Multi-Digit Subtraction [x5]"))
     (*activity 6.75 ??? 5 "Math > Multiplication [10x10] [written] [x10]")
     (*activity 7.0 ??? 5 "Division")))))
