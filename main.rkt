#lang racket/base
(require racket/runtime-path
         web-server/servlet
         web-server/servlet-env
         (prefix-in d: racket/date))

(struct date (year month day))
(struct user (name bday))
(struct activity (start-age end-age freq name))

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
       (<= (user-age u)
           (activity-end-age a))))

(define-runtime-path source-dir ".")

(define (go users activities)
  (define live-activities
    (filter (λ (a)
              (ormap (λ (u) (activity-live? a u))
                     users))
            activities))
  (define max-freq
    (list-max (map activity-freq live-activities)))

  (define (start req)
    (response/xexpr
     `(html
       (head
        (title "Kid Grid")
        (link ([rel "stylesheet"]
               [type "text/css"]
               [href "/style.css"])))
       (body
        (table
         (thead
          (tr
           (th "Activity")
           ,@(for/list ([u (in-list users)])
               `(th ,(user-name u)))))
         (tbody
          ,@(for/list ([a (in-list live-activities)])
              `(tr (td ,(activity-name a))
                   ,@(for/list ([u (in-list users)])
                       (if (activity-live? a u)
                         `(td
                           (table
                            (tr ,@(for/list ([i (in-range (activity-freq a))])
                                    `(td nbsp)))))
                         `(td ([class "blank"]) nbsp)))))))))))

  (serve/servlet
   start
   #:extra-files-paths (list (build-path source-dir "static"))
   #:port 0))

(module+ main
  (define ??? 18.0)
  (go
   (list
    (user "Peach" 
          3.0 #;
          (date 2010 08 23))
    (user "Frog"
          5.0 #;
          (date 2008 09 09)))
   (list
    (activity 3.0 3.5 5 "Reading > Letters")
    (activity 3.0 3.5 5 "Math > Numbers")
    (activity 3.0 ??? 1 "Life Skills")
    (activity 3.5 4.0 5 "Reading > Letters [written]")
    (activity 3.5 4.0 5 "Math > Numbers [written]")
    (activity 3.5 ??? 5 "Reading > Blends")
    (activity 4.0 ??? 5 "Reading > Words [written]")
    (activity 4.0 ??? 5 "Math > Addition [10x10] [written]")
    (activity 4.0 ??? 3 "Computer > Keyboard & Mouse")
    (activity 4.5 ??? 5 "Reading > Beginning reader books (to parent)")
    (activity 5.0 ??? 5 "Math > Subtraction [10x10] [written]")
    (activity 5.0 ??? 3 "Science > Basic reading (w/ parent)")
    (activity 5.0 ??? 3 "History > Basic reading (w/ parent)")
    (activity 5.0 ??? 5 "Literature > Basic reading (w/ parent)"))))
